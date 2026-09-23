// Eigen::Tensor-argument wrapper around the dwish_chol density kernel
// (see dwish_chol.h). Templated code like this can only ever be header-
// only -- the compiler must see the definition wherever it's instantiated
// -- so this lives in inst/include regardless of the package-DLL/
// on-the-fly-compilation split that motivates dwish_chol.h itself.

#ifndef _NCOMPILER_DISTS_DWISH_CHOL_TENSOR
#define _NCOMPILER_DISTS_DWISH_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "dwish_chol.h"

// Eigen::Tensor wrapper for dwish_chol.
//
// Unlike dmnorm_chol/dmvt_chol, the random variable x here is itself a
// p x p matrix (rank-2 tensor), not a vector.
//
// x and chol are accepted as arbitrary Eigen::Tensor expressions built up
// at the call site. chol is read-only, so asDenseTensor reuses it directly
// (no copy) when it's already a concrete tensor, and only materializes a
// copy when the call site passed a lazy expression. x is always copied into
// its own buffer: dwish_chol overwrites its working copy of x in place
// (regardless of scale_param), and we must not mutate the caller's tensor
// as a side effect.
template<typename TensorExprX, typename TensorExprChol>
double dwish_chol(const TensorExprX &x, const TensorExprChol &chol,
                   double df, double scale_param, int give_log) {
  const auto &cholEval = asDenseTensor<2>(chol);
  // x is materialized unconditionally (never via asDenseTensor's reference
  // path), so cast<double>() is needed here too in case TensorExprX has a
  // non-double Scalar.
  Eigen::Tensor<double, 2> xEval = x.template cast<double>();

  int p = static_cast<int>(xEval.dimension(0));

  // cholEval may alias the caller's own tensor (when no copy was needed
  // above); dwish_chol's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through chol, so this cast is safe.
  return dwish_chol(xEval.data(), const_cast<double*>(cholEval.data()), df, p,
                     scale_param, give_log, /*overwrite_inputs=*/1);
}

#endif // _NCOMPILER_DISTS_DWISH_CHOL_TENSOR
