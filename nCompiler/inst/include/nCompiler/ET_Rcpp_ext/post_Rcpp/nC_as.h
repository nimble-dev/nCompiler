#ifndef NCOMPILER_NC_AS_H_
#define NCOMPILER_NC_AS_H_

#include <memory>
#include <type_traits>

// CastingProxy<TargetScalar, ViewType>
//
// RAII wrapper for cross-scalar-type as() on the LHS. Holds a copy of the
// source view cast to TargetScalar. On destruction, casts the (possibly
// modified) copy back into the original view via ViewType::operator=.
// ViewType should be a StridedTensorMap so that non-contiguous sources
// (e.g. blockRef) are handled correctly. For RHS use, is_lhs = false
// makes the destructor a no-op.
//
// ViewType must expose ::Scalar, ::NumDimensions, and operator= from Eigen.
template<typename TargetScalar, typename ViewType>
class CastingProxy {
  static constexpr int nDim = ViewType::NumDimensions;
  using SourceScalar = typename ViewType::Scalar;
  using CopyTensor = Eigen::Tensor<TargetScalar, nDim>;
  using TM = Eigen::TensorMap<CopyTensor>;

  ViewType view_;   // view into original source data
  CopyTensor copy_; // TargetScalar copy

public:
  explicit CastingProxy(ViewType view)
    : view_(view), copy_(view.template cast<TargetScalar>()) {}

  // Always writes copy_ back to the source on destruction.
  ~CastingProxy() {
    view_ = copy_.template cast<SourceScalar>();
  }

  CastingProxy(const CastingProxy&) = delete;
  CastingProxy& operator=(const CastingProxy&) = delete;

  // Assign an Eigen expression into copy_. cast<TargetScalar>() is a no-op
  // when Rhs already has scalar type TargetScalar.
  template<typename Rhs>
  CastingProxy& operator=(const Rhs& rhs) {
    copy_ = rhs.template cast<TargetScalar>();
    return *this;
  }

  TM map() { return TM(copy_.data(), copy_.dimensions()); }
};

// RuntimeCastingProxy<TargetScalar, nDim>
//
// Used when the source type is only known at runtime (ETaccessorBase).
// At construction, dynamic_cast tests whether the source scalar matches
// TargetScalar:
//   - Same type: map_ points directly into source data (no copy).
//   - Different type: allocates copy_, cast-copies from source; map_ points
//     into copy_->data().
// On destruction, if a copy was made and is_lhs is true, writes copy_ back
// into the source via virtual writeBackFrom* methods.
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

  TM map() { return TM(data_ptr_, dims_); }
};

// ---------------------------------------------------------------------------
// as_nC — the single public API emitted by the nCompiler code generator.
// Two overloads: compile-time source (any concrete T) and runtime source
// (ETaccessorBase&, scalar type unknown at C++ compile time).
// ---------------------------------------------------------------------------

// Compile-time source: delegates to ETaccessorTyped<Scalar>::asTyped<>().
// Returns: TM (TM mode), STM (STM/LHS same-scalar), CastingProxy (LHS cross-
// scalar), or a lazy Eigen cast expression (RHS cross-scalar).
template<typename TargetScalar, int nDim, AsMode mode = AsMode::TM, typename T>
auto as_nC(T& x) {
  return ETaccess(x).template asTyped<TargetScalar, nDim, mode>();
}

// Runtime source: scalar type of acc is unknown at compile time.
// Returns a RuntimeCastingProxy that uses dynamic_cast to avoid copies when
// source scalar already matches TargetScalar, and virtual cast/writeback
// methods otherwise. Write-back occurs on destruction iff mode == LHS.
template<typename TargetScalar, int nDim, AsMode mode = AsMode::TM>
RuntimeCastingProxy<TargetScalar, nDim> as_nC(ETaccessorBase& acc) {
  return RuntimeCastingProxy<TargetScalar, nDim>(acc, mode == AsMode::LHS);
}

#endif // NCOMPILER_NC_AS_H_
