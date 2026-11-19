#ifndef NCOMPILER_NC_AS_H_
#define NCOMPILER_NC_AS_H_

#include <exception>
#include <memory>
#include <type_traits>
#include "../../ET_ext/StridedTensorMap.h"

// The kind of view an as_nC proxy provides:
//   TM  -- read-only (RHS) use of a whole object, as a TensorMap.
//   STM -- read-only (RHS) use that will be indexed, as a StridedTensorMap.
//   LHS -- writable use (assignment), as a StridedTensorMap, with write-back if cast.
//   CopyProxy -- a new object of the target type (e.g. for passing to a by-value (copy) argument),
//           made by flex_ assignment from any source, including expressions and scalars.
//   BlockRefProxy -- the same as LHS, named for passing to a blockRef (StridedTensorMap) argument.
//   RefProxy -- a copy of the target type, as for CopyProxy, that is written back to the source
//          (if it is not a temporary), for passing to a ref (Tensor&) argument.
enum class AsMode { TM, STM, LHS, CopyProxy, RefProxy, BlockRefProxy = LHS };

// ---------------------------------------------------------------------------
// All as_nC<>() calls return a proxy. Every proxy exposes operator()() to
// obtain the underlying tensor expression or reference. This uniform interface
// means callers always write as_nC<T,N,mode>(x)() regardless of mode or
// scalar match.
//
// Proxy types selected by as_nC():
//
//   EmptyProxy<ViewType>               — same-scalar; wraps TM or STM view
//   RHSCastProxy<Src,Tgt,N>            — cross-scalar RHS; lazy, no allocation
//   CastingProxy<Tgt,ViewType>         — cross-scalar LHS; eager copy + write-back
//   CopyProxy<TargetType>              — AsMode::CopyProxy; owns a new object made by flex_ assignment
//   RefProxy<TargetType,Source>        — AsMode::RefProxy; as CopyProxy, plus flex_ write-back
//   RuntimeCastingProxy<Tgt,N>         — runtime-source (ETaccessorBase)
//
// A fifth proxy, KnownProxy<TargetType>, is selected explicitly by
// known_nC<TargetType>() rather than by as_nC(): it is for a runtime source
// (ETaccessorBase&) whose type the caller *guarantees* at the call site, so
// it skips all cast/copy/write-back machinery and just caches a direct
// reference to the source's own storage after a one-time downcast.
// ---------------------------------------------------------------------------

// EmptyProxy<ViewType>
//
// Same-scalar proxy. Stores a TensorMap or StridedTensorMap by value (cheap:
// just a pointer + dims). operator()() returns a reference to the stored view
// for all Eigen tensor operations.
template<typename ViewType>
class EmptyProxy {
  ViewType view_;
public:
  explicit EmptyProxy(ViewType view) : view_(std::move(view)) {}
  EmptyProxy(const EmptyProxy&) = delete;
  EmptyProxy& operator=(const EmptyProxy&) = delete;
  ViewType& operator()() { return view_; }
};

template<typename Scalar>
class EmptyScalarProxy {
  Scalar& scalar_;
public:
  explicit EmptyScalarProxy(Scalar& s) : scalar_(s) {}
  EmptyScalarProxy(const EmptyScalarProxy&) = delete;
  EmptyScalarProxy& operator=(const EmptyScalarProxy&) = delete;
  Scalar& operator()() { return scalar_; }
};

template<typename TargetScalar,typename Scalar>
class CastingScalarProxy {
  static_assert(flex_cast_allowed_v<TargetScalar, Scalar>, FLEX_STRING_TYPE_ERROR);
  Scalar& scalar_;
  TargetScalar casted_scalar_;
public:
  explicit CastingScalarProxy(Scalar& s) : scalar_(s), casted_scalar_(static_cast<TargetScalar>(s)) {}
  CastingScalarProxy(const CastingScalarProxy&) = delete;
  CastingScalarProxy& operator=(const CastingScalarProxy&) = delete;
  TargetScalar& operator()() { return casted_scalar_; }
  ~CastingScalarProxy() {
    scalar_ = static_cast<Scalar>(casted_scalar_);
  }
};

// RHSCastProxy<TargetScalar, ViewType>
//
// Cross-scalar RHS proxy. ViewType is TM (AsMode::TM, non-indexed) or STM
// (AsMode::STM, indexed). Stores the view by value; operator()() returns a
// fresh lazy view.cast<TargetScalar>() expression each time. The const ViewType&
// inside the TensorConversionOp refers to the stable view_ member, not a stack
// temporary, so there is no dangling reference. No copy of source data is made.
//
// STM must be used for indexed RHS (AsMode::STM) so that non-contiguous sources
// (e.g. blockRef) produce correct strides in the lazy cast expression.
template<typename TargetScalar, typename ViewType>
class RHSCastProxy {
  ViewType view_;
public:
  explicit RHSCastProxy(ViewType view) : view_(std::move(view)) {}
  RHSCastProxy(const RHSCastProxy&) = delete;
  RHSCastProxy& operator=(const RHSCastProxy&) = delete;
  // Returns a CastSTM — a proper Eigen expression that casts elements on read.
  // Directly supports operator()(index), assignment to Tensor<>, and all Eigen ops.
  Eigen::CastSTM<TargetScalar, ViewType> operator()() { return Eigen::CastSTM<TargetScalar, ViewType>(view_); }
};

// CastingProxy<TargetScalar, ViewType>
//
// Cross-scalar LHS proxy. Holds an eager copy of the source view cast to
// TargetScalar. On destruction, casts the copy back into the original view.
// ViewType should be a StridedTensorMap so that non-contiguous sources
// (e.g. blockRef) are handled correctly.
//
// operator()() returns CopyTensor& for all Eigen write operations:
//   as_nC<int,2,AsMode::LHS>(x)()(i, j) = val;       // element write
//   ISEQS_(2, si, as_nC<int,2,AsMode::LHS>(x)()) = y; // range write
//   as_nC<int,1,AsMode::LHS>(x)() = rhs;              // whole-object write
template<typename TargetScalar, typename ViewType>
class CastingProxy {
  static constexpr int nDim = ViewType::NumDimensions;
  using SourceScalar = typename ViewType::Scalar;
  using CopyTensor = Eigen::Tensor<TargetScalar, nDim>;

  ViewType view_;
  CopyTensor copy_;

public:
  explicit CastingProxy(ViewType view)
    : view_(view), copy_(view.template cast<TargetScalar>()) {}

  ~CastingProxy() {
    view_ = copy_.template cast<SourceScalar>();
  }

  CastingProxy(const CastingProxy&) = delete;
  CastingProxy& operator=(const CastingProxy&) = delete;

  CopyTensor& operator()() { return copy_; }
};

// CopyProxy<TargetType>
//
// The proxy for AsMode::CopyProxy. Owns a new object of TargetType (an Eigen::Tensor or a true scalar),
// assigned from the source by flex_, so the source can be any object or expression
// (with a different scalar type or nDim) that flex_ assignment accepts, including a scalar
// for a tensor TargetType. operator()() moves the object out, e.g. to construct a by-value
// argument without a second copy:
//   foo(as_nC<Eigen::Tensor<double, 1>, AsMode::CopyProxy>(x + 1)());
template<typename TargetType>
class CopyProxy {
  TargetType copy_{};
public:
  template<typename T>
  explicit CopyProxy(const T& x) { flex_(copy_) = x; }
  CopyProxy(const CopyProxy&) = delete;
  CopyProxy& operator=(const CopyProxy&) = delete;
  TargetType&& operator()() { return std::move(copy_); }
};

// RefProxy<TargetType, Source>
//
// The proxy for AsMode::RefProxy, for passing a source that is not exactly TargetType (e.g. a different
// scalar type or nDim, a block, or a scalar) to a ref (TargetType&) argument. Owns a copy
// made by flex_ assignment, as in CopyProxy. operator()() returns a reference to the copy.
// On destruction, the copy is written back by flex_ assignment, so a whole-object source
// takes any resizing done through the ref, while a block (StridedTensorMap) must keep its
// extents (apart from 1s) and a scalar must receive exactly one element; otherwise flex_ throws.
// A StridedTensorMap source is stored by value (it is a view, and may be a temporary);
// other sources are stored by reference. Source = void means a temporary that is not a view
// (e.g. an expression): there is nothing to write back, so changes to the copy are lost.
// Write-back is skipped if an exception is being thrown (e.g. from the called function).
template<typename TargetType, typename Source>
class RefProxy {
  using SourceStore = std::conditional_t<Eigen::is_StridedTensorMap_v<Source>, Source, Source&>;
  SourceStore source_;
  TargetType copy_{};
  int uncaught_;
public:
  explicit RefProxy(Source& x) : source_(x), uncaught_(std::uncaught_exceptions()) { flex_(copy_) = x; }
  RefProxy(const RefProxy&) = delete;
  RefProxy& operator=(const RefProxy&) = delete;
  TargetType& operator()() { return copy_; }
  ~RefProxy() noexcept(false) {
    if(std::uncaught_exceptions() == uncaught_) flex_(source_) = copy_;
  }
};

template<typename TargetType>
class RefProxy<TargetType, void> {
  TargetType copy_{};
public:
  template<typename T>
  explicit RefProxy(const T& x) { flex_(copy_) = x; }
  RefProxy(const RefProxy&) = delete;
  RefProxy& operator=(const RefProxy&) = delete;
  TargetType& operator()() { return copy_; }
};

// RuntimeCastingProxy<TargetScalar, nDim>
//
// Runtime-source proxy (ETaccessorBase). At construction, ETaccessorTyped_ptr() tests
// whether the source scalar matches TargetScalar:
//   - Same type: data_ptr_ points directly into source data (no copy).
//   - Different type: allocates copy_, cast-copies from source;
//     data_ptr_ points into copy_->data().
// On destruction, if a copy was made and is_lhs is true, writes copy_ back
// into the source via virtual writeBackFrom* methods.
//
// operator()() returns TM for all Eigen ops (element access, ISEQS_, ...):
//   as_nC<int,1>(*acc)()(i)                         // element read
//   as_nC<int,1,AsMode::LHS>(*acc)()(i) = val;      // element write
//   as_nC<int,1,AsMode::LHS>(*acc)() = rhs;         // whole-object write

template<typename TargetType>
class RuntimeCastingProxy {
  // TargetType here should be a true scalar type,
  // because specialization to Eigen::Tensor types is below.
  typedef TargetType TargetScalar;

  ETaccessorBase& source_;
  bool is_lhs_;
  bool copy_made_;
  TargetScalar copy_;
  TargetScalar* data_ptr_;

  void castCopyFrom() {
    if constexpr (std::is_same_v<TargetScalar, double>)
      source_.castCopyToDouble(&copy_, 1);
    else if constexpr (std::is_same_v<TargetScalar, int>)
      source_.castCopyToInt(&copy_, 1);
    else if constexpr (std::is_same_v<TargetScalar, bool>)
      source_.castCopyToBool(&copy_, 1);
    else
      Rcpp::stop("RuntimeCastingProxy: unsupported TargetScalar type.");
    data_ptr_ = &copy_;
    copy_made_ = true;
  }

  void writeBack() {
    if constexpr (std::is_same_v<TargetScalar, double>)
      source_.writeBackFromDouble(&copy_, 1);
    else if constexpr (std::is_same_v<TargetScalar, int>)
      source_.writeBackFromInt(&copy_, 1);
    else if constexpr (std::is_same_v<TargetScalar, bool>)
      source_.writeBackFromBool(&copy_, 1);
    else
      Rcpp::stop("RuntimeCastingProxy: unsupported TargetScalar type.");
  }

public:
  explicit RuntimeCastingProxy(ETaccessorBase& acc, bool is_lhs = false)
    : source_(acc), is_lhs_(is_lhs), copy_made_(false)
  {
    // equivalent to dynamic_cast<ETaccessorTyped<TargetScalar>*>(&acc); done via virtual method to ensure cast is done in the object's original DLL, in case it was passed through another DLL
    auto* typed = acc.template ETaccessorTyped_ptr<TargetScalar>();
    if(typed) {
      // Same scalar type: view directly, no copy.
      data_ptr_ = &acc.scalar<TargetScalar>();
    } else {
      castCopyFrom();
    }
  }
  
  ~RuntimeCastingProxy() {
    if(copy_made_ && is_lhs_) writeBack();
  }

  RuntimeCastingProxy(const RuntimeCastingProxy&) = delete;
  RuntimeCastingProxy& operator=(const RuntimeCastingProxy&) = delete;

  TargetScalar& operator()() { return *data_ptr_; }
};

template<typename TargetScalar, int nDim>
class RuntimeCastingProxy<Eigen::Tensor<TargetScalar, nDim> > {
  using TM = Eigen::TensorMap<Eigen::Tensor<TargetScalar, nDim>>;
  using CopyTensor = Eigen::Tensor<TargetScalar, nDim>;

  ETaccessorBase& source_;
  std::unique_ptr<CopyTensor> copy_; // null when same scalar type
  TargetScalar* data_ptr_;
  Eigen::array<Eigen::Index, nDim> dims_;
  bool is_lhs_;

  // Same flex rule as mapTyped in ETaccessorTyped (see set_output_dims).
  Eigen::array<Eigen::Index, nDim> computeDims(const std::vector<int>& intDims) {
    Eigen::array<Eigen::Index, nDim> outDim;
    set_output_dims(intDims, outDim, nDim);
    return outDim;
  }

  size_t totalElems() const {
    size_t n = 1;
    for(int i = 0; i < nDim; ++i) n *= static_cast<size_t>(dims_[i]);
    return n;
  }

  void castCopyFrom() {
    size_t n = totalElems();
    copy_ = std::make_unique<CopyTensor>(dims_);
    if constexpr (std::is_same_v<TargetScalar, double>)
      source_.castCopyToDouble(copy_->data(), n);
    else if constexpr (std::is_same_v<TargetScalar, int>)
      source_.castCopyToInt(copy_->data(), n);
    else if constexpr (std::is_same_v<TargetScalar, bool>)
      source_.castCopyToBool(copy_->data(), n);
    else
      Rcpp::stop("RuntimeCastingProxy: unsupported TargetScalar type.");
    data_ptr_ = copy_->data();
  }

  void writeBack() {
    size_t n = totalElems();
    if constexpr (std::is_same_v<TargetScalar, double>)
      source_.writeBackFromDouble(copy_->data(), n);
    else if constexpr (std::is_same_v<TargetScalar, int>)
      source_.writeBackFromInt(copy_->data(), n);
    else if constexpr (std::is_same_v<TargetScalar, bool>)
      source_.writeBackFromBool(copy_->data(), n);
    else
      Rcpp::stop("RuntimeCastingProxy: unsupported TargetScalar type.");
  }

public:
  explicit RuntimeCastingProxy(ETaccessorBase& acc, bool is_lhs = false)
    : source_(acc), is_lhs_(is_lhs)
  {
    dims_ = computeDims(acc.intDims());
    // Equivalent to dynamic_cast<ETaccessorTyped<TargetScalar>*>(&acc); done via virtual method to ensure cast is done in the object's original DLL, in case it was passed through another DLL
    auto* typed = acc.template ETaccessorTyped_ptr<TargetScalar>(); 
    if(typed) {
      // Same scalar type: view directly, no copy.
      auto tm = typed->template mapTyped<nDim>();
      data_ptr_ = tm.data();
    } else {
      castCopyFrom();
    }
  }

  ~RuntimeCastingProxy() {
    if(copy_ && is_lhs_) writeBack();
  }

  RuntimeCastingProxy(const RuntimeCastingProxy&) = delete;
  RuntimeCastingProxy& operator=(const RuntimeCastingProxy&) = delete;

  TM operator()() { return TM(data_ptr_, dims_); }
};

// KnownProxy was added using Claude as a potentially useful feature,
// particularly as a target for nimble2 keyword-processing needs.
// At the time of initial drafting it remains to be seen if it will
// actually be used.
//
// KnownProxy<TargetType>
//
// Runtime-source proxy for a target type the caller *guarantees* matches the
// actual runtime type behind an ETaccessorBase (e.g. "I know this is a 2D
// double tensor"). Unlike RuntimeCastingProxy, there is no cross-scalar
// fallback: the downcast either succeeds outright or throws (via
// ETaccessorBase::ref()/scalar(), same as calling them directly). Since it
// is a hard guarantee rather than a soft cast, operator()() returns a
// reference to the source's own storage (an actual Eigen::Tensor&, not a
// TensorMap view) — no copy, no write-back, no reshaping of singleton dims.
//
// The downcast happens once, in the constructor; operator()() just returns
// the cached reference, so it is safe and cheap to call repeatedly as long
// as the source outlives the proxy:
//
//   auto my_proxy = known_nC<Eigen::Tensor<double, 2>>(my_ETaccessorBase);
//   Y = my_proxy() + B;
template<typename TargetType>
class KnownProxy {
  // Primary template covers true scalar targets (double, int, bool).
  TargetType& ref_;
public:
  explicit KnownProxy(ETaccessorBase& acc) : ref_(acc.template scalar<TargetType>()) {}
  KnownProxy(const KnownProxy&) = delete;
  KnownProxy& operator=(const KnownProxy&) = delete;
  TargetType& operator()() { return ref_; }
};

template<typename Scalar, int nDim>
class KnownProxy<Eigen::Tensor<Scalar, nDim>> {
  Eigen::Tensor<Scalar, nDim>& ref_;
public:
  explicit KnownProxy(ETaccessorBase& acc) : ref_(acc.template ref<nDim, Scalar>()) {}
  KnownProxy(const KnownProxy&) = delete;
  KnownProxy& operator=(const KnownProxy&) = delete;
  Eigen::Tensor<Scalar, nDim>& operator()() { return ref_; }
};

// known_nC — public API for KnownProxy, mirroring as_nC's runtime-source
// overloads (ETaccessorBase&, unique_ptr<ETaccessorBase>& / &&).
template<typename TargetType>
KnownProxy<TargetType> known_nC(ETaccessorBase& acc) {
  return KnownProxy<TargetType>(acc);
}

template<typename TargetType>
KnownProxy<TargetType> known_nC(std::unique_ptr<ETaccessorBase>& acc) {
  return KnownProxy<TargetType>(*acc);
}

template<typename TargetType>
KnownProxy<TargetType> known_nC(std::unique_ptr<ETaccessorBase>&& acc) {
  return KnownProxy<TargetType>(*acc);
}

// ---------------------------------------------------------------------------
// as_nC — the single public API emitted by the nCompiler code generator.
// Two overloads: compile-time source (any concrete T) and runtime source
// (ETaccessorBase&, scalar type unknown at C++ compile time).
//
// All overloads return a proxy; caller always writes as_nC<T,N,mode>(x)().
// ---------------------------------------------------------------------------

// Helpers for the compile-time-source as_nC, which builds views directly from the source
// (no ETaccessor). The source is an Eigen::Tensor or a true scalar.
namespace as_nC_detail {
  // TensorMap with nDim dimensions, set from the source's dimensions by the flex rule
  // (see set_output_dims). A true scalar has no dimensions, so its map has extents of 1.
  template<int nDim, typename T>
  Eigen::TensorMap<Eigen::Tensor<flex_scalar_t<T>, nDim> > map(T &x) {
    typedef Eigen::TensorMap<Eigen::Tensor<flex_scalar_t<T>, nDim> > TM;
    typename TM::Dimensions outDim;
    if constexpr (std::is_same_v<type_category_t<T>, trueScalar>) {
      Eigen::array<Eigen::Index, 0> inDim;
      set_output_dims(inDim, outDim, nDim);
      return TM(&x, outDim);
    } else {
      set_output_dims(x.dimensions(), outDim, nDim);
      return TM(x.data(), outDim);
    }
  }

  // Reference to the single element of a true scalar, or of a tensor whose extents are all 1.
  template<typename T>
  flex_scalar_t<T> &scalar(T &x) {
    if constexpr (std::is_same_v<type_category_t<T>, trueScalar>) {
      return x;
    } else {
      const auto &dims = x.dimensions();
      for(size_t i = 0; i < dims.size(); ++i) {
        if(dims[i] != 1)
          Rcpp::stop("Invalid use of as() to a scalar for an object with dimensions not all equal to 1.");
      }
      return *x.data();
    }
  }
}

// Compile-time source (an Eigen::Tensor, a StridedTensorMap, or a true scalar, or any source
// for AsMode::CopyProxy). A StridedTensorMap source is viewed with its own offsets and strides.
// Returns CopyProxy for AsMode::CopyProxy. Otherwise returns EmptyProxy<TM>, EmptyProxy<STM>,
// RHSCastProxy, or CastingProxy for a tensor TargetType, or EmptyScalarProxy or
// CastingScalarProxy for a scalar TargetType.
template<typename TargetType, AsMode mode = AsMode::TM, typename T>
auto as_nC(T& x) {
  if constexpr (mode == AsMode::CopyProxy) {
    return CopyProxy<TargetType>(x);
  } else if constexpr (mode == AsMode::RefProxy) {
    return RefProxy<TargetType, T>(x);
  } else {
    static_assert(!std::is_same_v<type_category_t<T>, eigenOp> || Eigen::is_StridedTensorMap_v<T>,
                  "C++ COMPILATION ERROR: as_nC requires an Eigen::Tensor, a StridedTensorMap, or a scalar as its argument.");
    typedef flex_scalar_t<T> Scalar;
    if constexpr (std::is_same_v<type_category_t<TargetType>, trueScalar>) {
      static_assert(!Eigen::is_StridedTensorMap_v<T>,
                    "C++ COMPILATION ERROR: as_nC to a scalar type from a StridedTensorMap is not supported.");
      typedef TargetType TargetScalar;
      if constexpr (std::is_same_v<TargetScalar, Scalar>) {
        return EmptyScalarProxy<TargetScalar>(as_nC_detail::scalar(x));
      } else {
        return CastingScalarProxy<TargetScalar, Scalar>(as_nC_detail::scalar(x));
      }
    } else {
      typedef flex_scalar_t<TargetType> TargetScalar;
      constexpr int nDim = flex_nDim_v<TargetType>;
      if constexpr (Eigen::is_StridedTensorMap_v<T>) {
        // A view of a view (by the flex rule for dimensions). There is no TensorMap of it,
        // so the TM mode uses it as an STM.
        Eigen::StridedTensorMap<Eigen::Tensor<Scalar, nDim> > stview(x);
        if constexpr (std::is_same_v<TargetScalar, Scalar>) {
          return EmptyProxy<decltype(stview)>(stview);
        } else if constexpr (mode == AsMode::LHS) {
          return CastingProxy<TargetScalar, decltype(stview)>(stview);
        } else {
          return RHSCastProxy<TargetScalar, decltype(stview)>(stview);
        }
      } else {
        auto view = as_nC_detail::map<nDim>(x);
        if constexpr (std::is_same_v<TargetScalar, Scalar>) {
          if constexpr (mode == AsMode::TM)
            return EmptyProxy<decltype(view)>(view);
          else {
            auto stview = Eigen::MakeStridedTensorMap<nDim>::make(view);
            return EmptyProxy<decltype(stview)>(stview);
          }
        } else {
          if constexpr (mode == AsMode::LHS) {
            auto stview = Eigen::MakeStridedTensorMap<nDim>::make(view);
            return CastingProxy<TargetScalar, decltype(stview)>(stview);
          } else if constexpr (mode == AsMode::STM) {
            // Indexed RHS: use STM so that non-contiguous sources (e.g. blockRef)
            // have correct strides in the lazy cast expression.
            auto stview = Eigen::MakeStridedTensorMap<nDim>::make(view);
            return RHSCastProxy<TargetScalar, decltype(stview)>(stview);
          } else {
            return RHSCastProxy<TargetScalar, decltype(view)>(view);
          }
        }
      }
    }
  }
}

// A temporary source (e.g. an expression such as x + 1) can only be copied
// (for AsMode::RefProxy, a copy that is not written back),
// except a temporary StridedTensorMap (e.g. from MakeStridedTensorMap for an indexed argument),
// which is a view of data that outlives it.
template<typename TargetType, AsMode mode = AsMode::TM, typename T>
auto as_nC(const T& x) {
  if constexpr (mode == AsMode::CopyProxy) {
    return CopyProxy<TargetType>(x);
  } else if constexpr (mode == AsMode::RefProxy && !Eigen::is_StridedTensorMap_v<T>) {
    return RefProxy<TargetType, void>(x);
  } else {
    static_assert(Eigen::is_StridedTensorMap_v<T>,
                  "C++ COMPILATION ERROR: as_nC can use a temporary (e.g. an expression) only with AsMode::CopyProxy or AsMode::RefProxy.");
    T view(x); // a copy of the view; the proxies store their own views by value.
    return as_nC<TargetType, mode>(view);
  }
}

// Runtime source: scalar type of acc is unknown at compile time.
// Returns RuntimeCastingProxy. Write-back occurs on destruction iff mode == LHS.
template<typename TargetType, AsMode mode = AsMode::TM>
RuntimeCastingProxy<TargetType> as_nC(ETaccessorBase& acc) {
  return RuntimeCastingProxy<TargetType>(acc, mode == AsMode::LHS);
}

// genericInterfaceBaseC::access() returns a unique_ptr<ETaccessorBase>,
// so this overload allows direct passing of the unique_ptr, for simpler usage and code-generation.
// This takes its argument by value, so that it can be an rvalue (returned from another call at the call site).
template<typename TargetType, AsMode mode = AsMode::TM>
RuntimeCastingProxy<TargetType> as_nC(std::unique_ptr<ETaccessorBase>& acc) {
    return RuntimeCastingProxy<TargetType>(*acc, mode == AsMode::LHS);
}


template<typename TargetType, AsMode mode = AsMode::TM>
RuntimeCastingProxy<TargetType> as_nC(std::unique_ptr<ETaccessorBase>&& acc) {
    return RuntimeCastingProxy<TargetType>(*acc, mode == AsMode::LHS);
}

#endif // NCOMPILER_NC_AS_H_
