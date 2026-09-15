#ifndef _NCOMPILER_TENSOR_CREATION
#define _NCOMPILER_TENSOR_CREATION

#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>

namespace nCompiler {
namespace tensor_creation_detail {

// -----------------------------------------------------------------------
// Detection of a "dim expression": a single argument describing all
// output dimensions at once, as opposed to a list of individual scalar
// dimensions (e.g. createTensor<double, 2>(value, 3, 4)).
//
// A dim expression used to have to be exactly Eigen::Tensor<int, 1>, but
// the DSL can hand us any Eigen tensor expression -- concrete or lazy
// (e.g. the result of a concatenation, or a .cast<...>() applied to one)
// -- that will evaluate to a 1-D tensor of dimension sizes, and its
// scalar type need not be int (it is often double). is_dim_expr below
// matches any such expression by checking that it derives from
// Eigen::TensorBase and has exactly one dimension, regardless of scalar
// type; get_dims() below then materializes it and static_casts each
// element to the index type it actually needs.
//
// The std::is_class guard below is essential: forming
// Eigen::TensorBase<T, ...> for a non-class T (e.g. plain int/double
// dimension arguments) would try to instantiate Eigen::internal::traits<T>,
// which is a hard (non-SFINAE-able) error for such T since that primary
// traits template is only forward-declared. Dispatching on
// std::is_class<T>::value via partial specialization -- rather than
// combining both checks in one expression with && -- avoids ever forming
// that instantiation for non-class T.
// -----------------------------------------------------------------------

template <typename T, bool IsClass>
struct derives_from_tensor_base {
  static const bool value = false;
};

template <typename T>
struct derives_from_tensor_base<T, true> {
  static const bool value =
    std::is_base_of<Eigen::TensorBase<T, Eigen::ReadOnlyAccessors>, T>::value;
};

template <typename T, bool IsTensorExpr>
struct tensor_expr_num_dimensions {
  static const int value = -1;
};

template <typename T>
struct tensor_expr_num_dimensions<T, true> {
  static const int value = Eigen::internal::traits<T>::NumDimensions;
};

template <typename T>
struct is_dim_expr {
  typedef typename std::decay<T>::type DecayedT;
  static const bool is_tensor_expr =
    derives_from_tensor_base<DecayedT, std::is_class<DecayedT>::value>::value;
  static const bool value =
    tensor_expr_num_dimensions<DecayedT, is_tensor_expr>::value == 1;
};

// True iff Dim... is a single argument that is a dim expression (as
// opposed to a list of individual scalar dimensions).
template <typename... Dim>
struct dim_is_expr : std::false_type {};

template <typename Dim0>
struct dim_is_expr<Dim0> : std::integral_constant<bool, is_dim_expr<Dim0>::value> {};

// -----------------------------------------------------------------------
// get_dims(): collapse whichever form the caller's dim... argument(s)
// took into the std::array<IndexType, NumDimensionsOut> that Eigen::Tensor
// wants, dispatched (via the tag from dim_is_expr above) on individual
// scalar dimensions vs. a single dim expression. This is the only place
// those two forms need to be told apart; everything downstream (see
// createTensor() below) just consumes the resulting array.
// -----------------------------------------------------------------------

// individual scalar dimensions, e.g. createTensor<double, 2>(value, 3, 4).
// We must static_cast each dim here to avoid this error:
// error: type 'double' cannot be narrowed to 'long' in initializer list [-Wc++11-narrowing]
template <typename IndexType, int NumDimensionsOut, typename... Dim>
std::array<IndexType, NumDimensionsOut> get_dims(std::false_type /* dim_is_expr */,
						   Dim... dim) {
  return std::array<IndexType, NumDimensionsOut>{{static_cast<IndexType>(dim)...}};
}

// a single dim expression, e.g. createTensor<double, 2>(value, dim) where
// dim is an Eigen::Tensor<int, 1>, or any lazy Eigen tensor op (a cast, a
// concatenation, ...) that will evaluate to one.
template <typename IndexType, int NumDimensionsOut, typename DimT>
std::array<IndexType, NumDimensionsOut> get_dims(std::true_type /* dim_is_expr */,
						   const DimT& dim) {
  // dim may be a lazy expression, so it must be materialized into concrete
  // storage before its elements can be indexed. DimScalar is whatever
  // scalar type it actually produces (commonly int, but e.g. the result of
  // a .cast<double>() is fine too); each element is static_cast to
  // IndexType below regardless.
  typedef typename Eigen::internal::traits<DimT>::Scalar DimScalar;
  Eigen::Tensor<DimScalar, 1> dim_concrete = dim;
  std::array<IndexType, NumDimensionsOut> dims;
  for (unsigned int i = 0; i < NumDimensionsOut; i++) {
    dims[i] = static_cast<IndexType>(dim_concrete(i));
  }
  return dims;
}

}  // namespace tensor_creation_detail
}  // namespace nCompiler

template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, typename... Dim>
Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> createTensor(const DerivedIn& value, Dim... dim) {
  using namespace nCompiler::tensor_creation_detail;
  typedef typename Eigen::Tensor<ScalarTypeOut, NumDimensionsOut>::Index IndexType;
  std::array<IndexType, NumDimensionsOut> dims =
    get_dims<IndexType, NumDimensionsOut>(
      std::integral_constant<bool, dim_is_expr<Dim...>::value>(), dim...);
  // value is either a scalar to fill every element with, or a tensor to
  // reshape into the output shape; the branch not taken here is discarded
  // before it is ever checked against the other case's requirements on
  // DerivedIn (a scalar has no .reshape(), a tensor can't setConstant()).
  if constexpr (std::is_scalar<DerivedIn>::value) {
    Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> ans(dims);
    ans.setConstant(value);
    return(ans);
  } else {
    Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> ans = value.reshape(dims);
    return(ans);
  }
}

#endif
