// Eigen::Tensor-argument wrapper around the dmnorm_chol density kernel
// (see dmnorm_chol.h). Templated code like this can only ever be header-
// only -- the compiler must see the definition wherever it's instantiated
// -- so this lives in inst/include regardless of the package-DLL/
// on-the-fly-compilation split that motivates dmnorm_chol.h itself.

#ifndef _NCOMPILER_DISTS_DMNORM_CHOL_TENSOR
#define _NCOMPILER_DISTS_DMNORM_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>
#include "dmnorm_chol.h"

// True only when TensorExpr is exactly Eigen::Tensor<double, Rank>, i.e. an
// already-materialized, densely-packed tensor rather than a lazy expression
// (a block, a cast, an arithmetic chain, a TensorMap, etc).
template<typename TensorExpr, int Rank>
struct is_dense_double_tensor
  : std::is_same<TensorExpr, Eigen::Tensor<double, Rank>> {};

// Returns x itself (no copy) when it is already a concrete
// Eigen::Tensor<double, Rank>; otherwise assigns it into a fresh
// Eigen::Tensor<double, Rank>, which forces Eigen to evaluate the expression
// into a contiguous buffer we can call .data() on. The .cast<double>() is
// required here, not just a safety net: Eigen::Tensor's assignment/
// construction from another tensor expression requires matching Scalar
// types, so a non-double input (e.g. Eigen::Tensor<int, Rank>, or a bool
// expression from a comparison op) would otherwise fail to compile.
template<int Rank, typename TensorExpr>
decltype(auto) asDenseTensor(const TensorExpr &x) {
  if constexpr (is_dense_double_tensor<TensorExpr, Rank>::value) {
    return (x);
  } else {
    return Eigen::Tensor<double, Rank>(x.template cast<double>());
  }
}

// Eigen::Tensor wrapper for dmnorm_chol.
//
// x, mean, and chol are accepted as arbitrary Eigen::Tensor expressions
// built up at the call site. mean and chol are read-only, so asDenseTensor
// reuses them directly (no copy) when they're already concrete tensors, and
// only materializes a copy when the call site passed a lazy expression. x is
// always copied into its own buffer: dmnorm_chol overwrites its working copy
// of x in place, and we must not mutate the caller's tensor as a side
// effect.
template<typename TensorExprX, typename TensorExprMean, typename TensorExprChol>
double dmnorm_chol(const TensorExprX &x, const TensorExprMean &mean,
                    const TensorExprChol &chol, double prec_param, int give_log) {
  const auto &meanEval = asDenseTensor<1>(mean);
  const auto &cholEval = asDenseTensor<2>(chol);
  // x is materialized unconditionally (never via asDenseTensor's reference
  // path), so cast<double>() is needed here too in case TensorExprX has a
  // non-double Scalar.
  Eigen::Tensor<double, 1> xEval = x.template cast<double>();

  int n = static_cast<int>(xEval.dimension(0));

  // meanEval/cholEval may alias the caller's own tensors (when no copy was
  // needed above); dmnorm_chol's double* signature is non-const only
  // because it predates const-correctness conventions here, but it never
  // writes through mean or chol, so this cast is safe.
  return dmnorm_chol(xEval.data(), const_cast<double*>(meanEval.data()),
                      const_cast<double*>(cholEval.data()), n,
                      prec_param, give_log, /*overwrite_inputs=*/1);
}

#endif // _NCOMPILER_DISTS_DMNORM_CHOL_TENSOR
