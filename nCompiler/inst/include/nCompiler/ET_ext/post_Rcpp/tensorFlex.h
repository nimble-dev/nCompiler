#ifndef __TENSOR_FLEX
#define __TENSOR_FLEX

// This file supports code like
// flex_(y) = x
// where flex_ creates a class with an overloaded operator=
// that can do smart things to accept assignment from x.
// The purpose is to simplify code-generation from nCompiler and
// also to handle decisions that can only be made at run-time.
// The underlying assignment is done by smartAssign, which is tag-dispatched
// based on the LHS (y).
//
// There are three kinds of LHS (y):
// 1. A true scalar (double, int, bool, long, std::string, CppAD::AD<double>).
//    x must have exactly one element (any nDim).
// 2. A whole Eigen::Tensor, which can be resized by the assignment.
//    - If x has the same nDim, y = x.
//    - If x has a different nDim, the extents of x other than 1s are used in order for y.
//      Any spare dimensions of y keep the leftmost 1s of x that come before its last non-1
//      extent, and then y is padded with trailing 1s. E.g. a 10x1 matrix becomes a length-10
//      vector, a length-10 vector becomes a 10x1 matrix, and a 1x10 matrix becomes a 1x10x1 array.
//    - If x is zero-dimensional (a true scalar or a 0-dimensional Eigen object),
//      y is resized to all extents of 1.
// 3. A fixed-size LHS, such as y[1:10] (an Eigen op that can be assigned to but not resized).
//    - The extents of x other than 1s must match the extents of y other than 1s, in order.
//      E.g. a 1x10 or 10x1 matrix can be assigned to y[1:10].
//    - If x is zero-dimensional, all elements of y are set to its value.
// 4. Any other (non-Eigen) LHS type, such as DiagIO from diag(x) <- v, is assigned with its own operator=.
// "Smart" assignment does not flatten or reshape otherwise: extents must match apart from 1s.
// Mismatched extents are an error.
// Scalar types are cast as needed, except that character objects are never cast to or from
// other types.

#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>
#include <utility> // std::declval
#include <string>
#include <sstream>
#include "tensorUtils.h"
#include <nCompiler/ET_ext/flexDims.h> // flexDimsSetup
#include <nCompiler/utils.h> // nStop_

// These structs are used for tag dispatch.
// A function can take an argument of one of these types that is never used but
// serves to determine which over-loaded function definition should be used.
struct trueScalar {};
struct eigenOp {};
struct eigenTensor {};

// type_category<SomeType>::type will be either trueScalar (for double, long, int, bool,
// std::string, and CppAD::AD<double> in tensorFlex_CppAD.h),
// eigenTensor (for Eigen::Tensor< > ), or eigenOp (default for anything else).
// Uses of trueScalar that cast must not cast to or from std::string (see flex_cast_allowed_v).
template<typename T>
struct type_category {
  typedef eigenOp type; // default
};

template<typename ScalarType, int nDim>
struct type_category<Eigen::Tensor<ScalarType, nDim> > {
  typedef eigenTensor type;
};

template<>
struct type_category<double> {
  typedef trueScalar type;
};
template<>
struct type_category<long> {
  typedef trueScalar type;
};
template<>
struct type_category<int> {
  typedef trueScalar type;
};
template<>
struct type_category<bool> {
  typedef trueScalar type;
};
template<>
struct type_category<std::string> {
  typedef trueScalar type;
};

template <typename T>
using type_category_t = typename type_category<T>::type;

// True if T is an Eigen tensor object or op, i.e. derived from Eigen::TensorBase.
// Every tensor type has TensorBase<Derived, ReadOnlyAccessors> as a base (directly, or via
// TensorBase<Derived, WriteAccessors>), so testing for that one base avoids ambiguous deduction.
// This is used so that flex_ passes other (non-Eigen) LHS types, such as DiagIO
// (tensorOperations.h), through to their own operator=.
template<typename T>
struct is_tensor_xpr {
private:
  template<typename Derived>
  static std::true_type test(const Eigen::TensorBase<Derived, Eigen::ReadOnlyAccessors>*);
  static std::false_type test(...);
public:
  static constexpr bool value = decltype(test(std::declval<T*>()))::value;
};

template<typename T>
constexpr bool is_tensor_xpr_v = is_tensor_xpr<T>::value;

#define FLEX_STRING_TYPE_ERROR "C++ COMPILATION ERROR: a character variable can only be assigned from a character value."

// True if a cast between scalar types A and B is allowed, i.e. unless std::string is on only one side.
// Character objects are never cast to or from other scalar types.
template<typename A, typename B>
constexpr bool flex_cast_allowed_v =
  std::is_same<typename std::remove_cv<A>::type, std::string>::value ==
  std::is_same<typename std::remove_cv<B>::type, std::string>::value;

// The scalar type and number of dimensions of a true scalar or Eigen object, from Eigen::nDimTraits2.
template<typename XprType>
using flex_scalar_t = typename std::remove_const<typename Eigen::nDimTraits2<XprType>::Scalar>::type;

template<typename XprType>
constexpr int flex_nDim_v = static_cast<int>(Eigen::nDimTraits2<XprType>::NumDimensions);

// checkDims is used for an assignment where the sizes of the LHS can't change.
// example: If we have A[1:10] <- B, where B is a matrix or array with nDim > 1,
// then the assignment is valid if all dimensions of B have size 1 except for one dimension
// that has size 10.  This function returns true if that is the case.
// More generally, it returns true if the extents other than 1s of the two arguments
// are the same, in the same order, so the order of the arguments does not matter.
// Note that we may have a case like: A[1:10, 1, drop = FALSE] <- B, where B is a 3D array.
// B needs to have one dimension that is 10 and then be assigned to A as a 10x1 2D array.
// The dimensions arguments can be any array-like type (e.g. Eigen::DSizes or Eigen::array).
template<int LessNdim, int MoreNdim, typename LessDimsType, typename MoreDimsType>
bool checkDims(const LessDimsType &LessDims,
               const MoreDimsType &MoreDims) {
  int iLess = 0;
  // iterate through dimensions of the larger one.
  for(int iMore = 0; iMore < MoreNdim; ++iMore) {
    if(MoreDims[iMore] == 1) continue;
    // Move to the next non-1 dimension of LessDims
    while(iLess < LessNdim && LessDims[iLess] == 1) ++iLess;
    if(iLess == LessNdim) return false; // No more non-1 dimensions of LessDims: Fail
    if(LessDims[iLess] != MoreDims[iMore]) return false; // Non-1 dimensions don't match: Fail
    ++iLess;
  }
  // check that any remaining LessDims are all 1
  for( ; iLess < LessNdim; ++iLess) {
    if(LessDims[iLess] != 1) return false;
  }
  return true;
}

// checkAndSetupDims is used for an assignment where the sizes of the LHS can change.
// example: If we have A <- X %*% b, where A is a vector and X %*% b returns a 10x1 matrix at run-time.
// This function checks that the number of non-1 dimensions on the RHS is <= nDim of A,
// and sets LhsDims from RhsDims by the flex rule (see flexDims.h):
// If the RHS has *fewer* non-1 dimensions than nDim of A, the spare LHS dimensions keep
// the leftmost 1s of the RHS that come before its last non-1 dimension (leading or interior 1s),
// and any still remaining are padded on as trailing 1s.
// E.g. for a 3D LHS, RHS extents (5, 1, 6, 1) give (5, 1, 6), and (1, 5) gives (1, 5, 1).
// This function populates values of LhsDims so it can be an argument to reshape on the RHS.
template<int LhsNdim, int RhsNdim, typename LhsDimsType, typename RhsDimsType>
bool checkAndSetupDims(LhsDimsType &LhsDims,
                       const RhsDimsType &RhsDims) {
  return flexDimsSetup(RhsDims, RhsNdim, LhsDims, LhsNdim);
}

template<int Ndim, typename DimsType>
std::string flex_dims_string(const DimsType &dims) {
  std::ostringstream os;
  os << "(";
  for(int i = 0; i < Ndim; ++i) os << (i ? ", " : "") << dims[i];
  os << ")";
  return os.str();
}

// The error for extents that can't be assigned.
// LhsDescription describes the LHS, e.g. "a scalar" or "an object with extents (10)".
template<int RhsNdim, typename RhsDimsType>
[[noreturn]] void flex_dims_error(const std::string &LhsDescription, const RhsDimsType &RhsDims) {
  std::ostringstream os;
  os << "Cannot assign an object with extents " << flex_dims_string<RhsNdim>(RhsDims)
     << " to " << LhsDescription << ". Extents must match except for extents of 1.";
  nStop_(os);
}

// scalar_cast_ casts an object with exactly one element to a scalar type.
// The object can be a true scalar, a 0-dimensional Eigen object (e.g. from many reduction operators),
// or a non-scalar Eigen object, which is checked at run-time to have exactly one element.
// syntax will be scalar_cast_<int>::cast(x);
template<typename TargetT, typename T>
TargetT do_scalar_cast(const T &x, trueScalar) {
  static_assert(flex_cast_allowed_v<TargetT, T>, FLEX_STRING_TYPE_ERROR);
  return static_cast<TargetT>(x);
}

template<typename TargetT, typename ETtype>
TargetT do_scalar_cast(const ETtype &x, eigenTensor) {
  static_assert(flex_cast_allowed_v<TargetT, flex_scalar_t<ETtype> >, FLEX_STRING_TYPE_ERROR);
  static constexpr int nDim = flex_nDim_v<ETtype>;
  if constexpr (nDim == 0) {
    return static_cast<TargetT>(x());
  } else {
    if(x.size() != 1) flex_dims_error<nDim>("a scalar", x.dimensions());
    return static_cast<TargetT>(x.data()[0]);
  }
}

template<typename TargetT, typename XprType>
TargetT do_scalar_cast(const XprType &x, eigenOp) {
  typedef flex_scalar_t<XprType> XprScalar;
  static_assert(flex_cast_allowed_v<TargetT, XprScalar>, FLEX_STRING_TYPE_ERROR);
  static constexpr int nDim = flex_nDim_v<XprType>;
  if constexpr (nDim != 0) {
    // Check before evaluating.
    auto dims = nDimTraits2_dimensions(x);
    if(Eigen::internal::array_prod(dims) != 1) flex_dims_error<nDim>("a scalar", dims);
  }
  Eigen::Tensor<XprScalar, nDim> ans = x;
  return static_cast<TargetT>(ans.data()[0]);
}

template<typename ScalarOutput>
struct scalar_cast_ {
  template<typename XprType>
  static ScalarOutput cast(const XprType &x) {
    return do_scalar_cast<ScalarOutput, XprType>(x,
                                                 typename type_category<XprType>::type());
  }
};

// flex_cast is the generic casting system that handles scalars and Eigen objects appropriately.
// syntax will be flex_cast<double>(x), or flex_cast<double, true>(x) if an eigen op (x) should be evaluated to a concrete tensor.
// With eval = false, if no cast is needed, the result is x itself (by const reference).
// flex_cast<...>(x) should be used within
// the statement where it is created, since it may refer to x.
template<typename TargetT, typename T, bool eval>
TargetT do_flex_cast(const T &x, trueScalar) {
  static_assert(flex_cast_allowed_v<TargetT, T>, FLEX_STRING_TYPE_ERROR);
  return static_cast<TargetT>(x);
}

template<typename TargetScalarT, typename ETtype, bool eval>
decltype(auto) do_flex_cast(const ETtype &x, eigenTensor) {
  static_assert(flex_cast_allowed_v<TargetScalarT, flex_scalar_t<ETtype> >, FLEX_STRING_TYPE_ERROR);
  using TargetTensor = Eigen::Tensor<TargetScalarT, Eigen::internal::traits<ETtype>::NumDimensions>;
  if constexpr (eval) {
    // eval=true gives a concrete tensor. Note that Eigen's .eval() does NOT do that:
    // it returns a lazy TensorForcedEvalOp that is evaluated into a raw temporary buffer,
    // which skips destructors of non-arithmetic scalars (e.g. std::string) and adds a copy.
    // Hence we do not use .eval() here, but instead construct a concrete tensor directly.
    if constexpr (std::is_same_v<TargetScalarT, typename ETtype::Scalar>) {
      return x;
    } else {
      // Cast and materialize
      return TargetTensor(x.template cast<TargetScalarT>());
    }
  } else {
    // Return lazy expression when eval=false
    if constexpr (std::is_same_v<TargetScalarT, typename ETtype::Scalar>) {
      return x;
    } else {
      // Return lazy cast expression
      return x.template cast<TargetScalarT>();
    }
  }
}

template<typename TargetScalarT, typename XprType, bool eval>
decltype(auto) do_flex_cast(const XprType &x, eigenOp) {
  static_assert(flex_cast_allowed_v<TargetScalarT, flex_scalar_t<XprType> >, FLEX_STRING_TYPE_ERROR);
  using TargetTensor = Eigen::Tensor<TargetScalarT, Eigen::internal::traits<XprType>::NumDimensions>;
  if constexpr (eval) {
    // eval=true gives a concrete tensor (see note in the eigenTensor case above).
    if constexpr (std::is_same_v<TargetScalarT, typename Eigen::internal::traits<XprType>::Scalar>) {
      // materialize the op
      return TargetTensor(x);
    } else {
      // cast and materialize
      return TargetTensor(x.template cast<TargetScalarT>());
    }
  } else {
    // Return lazy expression when eval=false
    if constexpr (std::is_same_v<TargetScalarT, typename Eigen::internal::traits<XprType>::Scalar>) {
      return x;
    } else {
      return x.template cast<TargetScalarT>();
    }
  }
}

// decltype(auto) passes through a const reference from do_flex_cast, so a concrete tensor
// that needs no cast is not copied.
template<typename TargetT, bool eval=false>
struct flex_cast_ {
  template<typename XprType>
  static decltype(auto) cast(const XprType &x) {
    return do_flex_cast<TargetT, XprType, eval>(x, typename type_category<XprType>::type());
  }
};

// Convenience function for easier syntax: flex_cast<double>(x) instead of flex_cast_<double>::cast(x)
template<typename TargetT, bool eval=false, typename XprType>
decltype(auto) flex_cast(const XprType &x) {
  return flex_cast_<TargetT, eval>::cast(x);
}

namespace nCompiler {

// smartAssign is tag-dispatched based on the LHS.

template<typename TargetT, typename RhsXprType>
void smartAssign(TargetT &Lhs, const RhsXprType &Rhs, trueScalar) {
  Lhs = scalar_cast_<TargetT>::cast(Rhs);
}

// smartAssign for a whole Eigen::Tensor LHS, which can be resized (formerly smartAssignWholeObject).
template<typename TargetScalarT, int LhsNdim, typename RhsXprType>
void smartAssign(Eigen::Tensor<TargetScalarT, LhsNdim> &Lhs, const RhsXprType &Rhs, eigenTensor) {
  static constexpr int RhsNdim = flex_nDim_v<RhsXprType>;
  if constexpr (RhsNdim == 0) {
    // Get the value before resizing, in case RHS refers to LHS.
    TargetScalarT value = scalar_cast_<TargetScalarT>::cast(Rhs);
    if constexpr (LhsNdim > 0) {
      Eigen::DSizes<Eigen::Index, LhsNdim> NewLhsDims;
      for(int i = 0; i < LhsNdim; ++i) NewLhsDims[i] = 1;
      Lhs.resize(NewLhsDims);
    }
    Lhs.setConstant(value);
  } else if constexpr (LhsNdim == RhsNdim) {
    Lhs = flex_cast<TargetScalarT>(Rhs);
  } else {
    auto RhsDims = nDimTraits2_dimensions(Rhs);
    Eigen::DSizes<Eigen::Index, LhsNdim> NewLhsDims;
    if(!checkAndSetupDims<LhsNdim, RhsNdim>(NewLhsDims, RhsDims))
      flex_dims_error<RhsNdim>("an object with " + std::to_string(LhsNdim) + " dimension(s)", RhsDims);
    Lhs = flex_cast<TargetScalarT>(Rhs).reshape(NewLhsDims);
  }
}

// smartAssign for a fixed-size LHS, which can't be resized (formerly smartAssignFixedSize).
template<typename LhsXprType, typename RhsXprType>
void smartAssign(LhsXprType &Lhs, const RhsXprType &Rhs, eigenOp) {
  typedef flex_scalar_t<LhsXprType> ScalarType;
  static constexpr int LhsNdim = flex_nDim_v<LhsXprType>;
  static constexpr int RhsNdim = flex_nDim_v<RhsXprType>;
  if constexpr (RhsNdim == 0) {
    Lhs.setConstant(scalar_cast_<ScalarType>::cast(Rhs));
  } else {
    auto LhsDims = nDimTraits2_dimensions(Lhs);
    auto RhsDims = nDimTraits2_dimensions(Rhs);
    // Shortcut for identical extents, which need no reshape. Otherwise checkDims is still
    // needed: e.g. LHS (3, 1) and RHS (1, 3) have the same nDim and are compatible. They will
    // result in sameDims == false, but checkDims will return true.
    if constexpr (LhsNdim == RhsNdim) {
      bool sameDims = true;
      for(int i = 0; i < LhsNdim; ++i) sameDims = sameDims && (LhsDims[i] == RhsDims[i]);
      if(sameDims) {
        Lhs = flex_cast<ScalarType>(Rhs);
        return;
      }
    }
    bool ok = LhsNdim <= RhsNdim ?
      checkDims<LhsNdim, RhsNdim>(LhsDims, RhsDims) :
      checkDims<RhsNdim, LhsNdim>(RhsDims, LhsDims);
    if(!ok)
      flex_dims_error<RhsNdim>("an object with extents " + flex_dims_string<LhsNdim>(LhsDims), RhsDims);
    Lhs = flex_cast<ScalarType>(Rhs).reshape(LhsDims);
  }
}

} // namespace nCompiler

// flex__ is the key class.
// Usage is flex_(x) = expr.
// Note that flex_(x) returns a flex__<type of x> object, which holds a reference to x.
// x may be a temporary (such as y[1:10], an Eigen op), which lives (only) until the end of the statement.
template<typename xType>
class flex__ {
public:
  xType &x;
  explicit flex__(xType &x_) : x(x_) {}
  explicit flex__(xType &&x_) : x(x_) {}
  template<typename oType>
  xType& operator=(const oType &other) {
    if constexpr (std::is_same<xType, std::string>::value &&
                  std::is_convertible<const oType&, std::string>::value) {
      // std::string, string literals (char[N]) and const char* are assigned directly.
      x = other;
    } else if constexpr (std::is_same<type_category_t<xType>, eigenOp>::value &&
                         !is_tensor_xpr_v<xType>) {
      // Not an Eigen object (e.g. DiagIO): use its own operator=.
      x = other;
    } else {
      nCompiler::smartAssign(x, other, typename type_category<xType>::type());
    }
    return x;
  }
};

// The flex_ function captures the type of x and returns a flex__<type of x> object, which does the magic via operator=.
// This means we can code flex_(x) = foo(y);
template<typename xType>
flex__<typename std::remove_reference<xType>::type > flex_(xType &&x_)
{return flex__<typename std::remove_reference<xType>::type >(std::forward<xType>(x_));}

// DEPRECATED: use Eigen::nDimTraits2 (tensorUtils.h) instead.
// nDimTraits is no longer used by flex_ but is still used in tensorIndexingOps.h.
//   At some point when that is cleaned up, we will remove nDimTraits (and potentially rename nDimTraits2 to nDimTraits).
// nDimTraits<SomeEigenType>::NumDimensions will give the number of dimensions of SomeEigenType
// nDimTraits<SomeEigenType>::Dimensions will give a valid type for dimensions.
// nDimTraits<SomeEigenType>::EvaluatorType give the type returned by:
// nDimTraits<SomeEigenType>::getEvaluator() will return an EvaluatorType, from which
// one can get a reference to .dimensions().
template<typename XprType>
struct nDimTraits {
  static const int NumDimensions = Eigen::internal::traits<XprType>::NumDimensions;
  typedef Eigen::TensorEvaluator<const XprType, Eigen::DefaultDevice> EvaluatorType;
  typedef Eigen::array<long, NumDimensions> Dimensions;

  static const EvaluatorType getEvaluator(const XprType &x) {
    const typename Eigen::internal::remove_all<typename XprType::Nested>::type x_removed(x);
    // careful: m_impl might go out of scope
    return EvaluatorType(x_removed,
                         Eigen::DefaultDevice());
  }
};

template<>
struct nDimTraits<double> {
  static const int NumDimensions = 0;
};
template<>
struct nDimTraits<long> {
  static const int NumDimensions = 0;
};
template<>
struct nDimTraits<int> {
  static const int NumDimensions = 0;
};
template<>
struct nDimTraits<bool> {
  static const int NumDimensions = 0;
};

#endif
