#ifndef NCOMPILER_NC_AS_H_
#define NCOMPILER_NC_AS_H_

#include <memory>
#include <type_traits>
#include "../../ET_ext/StridedTensorMap.h"

// ---------------------------------------------------------------------------
// All as_nC<>() calls return a proxy. Every proxy exposes operator()() to
// obtain the underlying tensor expression or reference. This uniform interface
// means callers always write as_nC<T,N,mode>(x)() regardless of mode or
// scalar match.
//
// Four proxy types, selected at compile time by asTyped() / as_nC():
//
//   EmptyProxy<ViewType>               — same-scalar; wraps TM or STM view
//   RHSCastProxy<Src,Tgt,N>            — cross-scalar RHS; lazy, no allocation
//   CastingProxy<Tgt,ViewType>         — cross-scalar LHS; eager copy + write-back
//   RuntimeCastingProxy<Tgt,N>         — runtime-source (ETaccessorBase)
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

// RuntimeCastingProxy<TargetScalar, nDim>
//
// Runtime-source proxy (ETaccessorBase). At construction, dynamic_cast tests
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
template<typename TargetScalar, int nDim>
class RuntimeCastingProxy {
  using TM = Eigen::TensorMap<Eigen::Tensor<TargetScalar, nDim>>;
  using CopyTensor = Eigen::Tensor<TargetScalar, nDim>;

  ETaccessorBase& source_;
  std::unique_ptr<CopyTensor> copy_; // null when same scalar type
  TargetScalar* data_ptr_;
  Eigen::array<Eigen::Index, nDim> dims_;
  bool is_lhs_;

  // Mirrors mapTyped singleton-drop/pad logic from ETaccessorTyped.
  Eigen::array<Eigen::Index, nDim> computeDims(const std::vector<int>& intDims) {
    Eigen::array<Eigen::Index, nDim> outDim;
    int innate_nDim = static_cast<int>(intDims.size());
    if(nDim >= innate_nDim) {
      for(int i = 0; i < innate_nDim; ++i) outDim[i] = intDims[i];
      for(int i = innate_nDim; i < nDim; ++i) outDim[i] = 1;
    } else {
      int i_out = 0;
      for(int i_in = 0; i_in < innate_nDim; ++i_in) {
        if(intDims[i_in] > 1) {
          if(i_out >= nDim)
            Rcpp::stop("RuntimeCastingProxy: too many non-singleton dimensions for requested nDim.");
          outDim[i_out++] = intDims[i_in];
        }
      }
      for(; i_out < nDim; ++i_out) outDim[i_out] = 1;
    }
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
    auto* typed = dynamic_cast<ETaccessorTyped<TargetScalar>*>(&acc);
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

// ---------------------------------------------------------------------------
// as_nC — the single public API emitted by the nCompiler code generator.
// Two overloads: compile-time source (any concrete T) and runtime source
// (ETaccessorBase&, scalar type unknown at C++ compile time).
//
// All overloads return a proxy; caller always writes as_nC<T,N,mode>(x)().
// ---------------------------------------------------------------------------

// Compile-time source: delegates to ETaccessorTyped<Scalar>::asTyped<>().
// Returns EmptyProxy<TM>, EmptyProxy<STM>, RHSCastProxy, or CastingProxy.
template<typename TargetScalar, int nDim, AsMode mode = AsMode::TM, typename T>
auto as_nC(T& x) {
  return ETaccess(x).template asTyped<TargetScalar, nDim, mode>();
}

// Runtime source: scalar type of acc is unknown at compile time.
// Returns RuntimeCastingProxy. Write-back occurs on destruction iff mode == LHS.
template<typename TargetScalar, int nDim, AsMode mode = AsMode::TM>
RuntimeCastingProxy<TargetScalar, nDim> as_nC(ETaccessorBase& acc) {
  return RuntimeCastingProxy<TargetScalar, nDim>(acc, mode == AsMode::LHS);
}

#endif // NCOMPILER_NC_AS_H_
