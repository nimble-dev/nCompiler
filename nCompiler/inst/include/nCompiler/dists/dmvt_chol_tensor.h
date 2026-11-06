// Eigen::Tensor-argument wrapper around the dmvt_chol density kernel
// (see dmvt_chol.h). Templated code like this can only ever be header-
// only -- the compiler must see the definition wherever it's instantiated
// -- so this lives in inst/include regardless of the package-DLL/
// on-the-fly-compilation split that motivates dmvt_chol.h itself.

#ifndef _NCOMPILER_DISTS_DMVT_CHOL_TENSOR
#define _NCOMPILER_DISTS_DMVT_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "dmvt_chol.h"

// Eigen::Tensor wrapper for dmvt_chol.
//
// x, mu, and chol are accepted as arbitrary Eigen::Tensor expressions built
// up at the call site. chol is read-only, so asDenseTensor reuses it
// directly (no copy) when it's already a concrete tensor, and only
// materializes a copy when the call site passed a lazy expression. x is
// always copied into its own buffer: dmvt_chol overwrites its working copy
// of x in place, and we must not mutate the caller's tensor as a side
// effect. mu follows nimble's recycling rule (see recycleToLength in
// dists_tensor_utils.h): if it's shorter than x, its elements are reused
// cyclically to fill out x's length, matching the original C_dmvt_chol's
// full_mu construction -- so mu is always materialized to x's length here
// rather than taking asDenseTensor's no-copy path.
template<typename TensorExprX, typename TensorExprMu, typename TensorExprChol>
double dmvt_chol(const TensorExprX &x, const TensorExprMu &mu,
                  const TensorExprChol &chol, double df, double prec_param,
                  int give_log) {
  const auto &cholEval = asDenseTensor<2>(chol);
  // x is materialized unconditionally (never via asDenseTensor's reference
  // path), so cast<double>() is needed here too in case TensorExprX has a
  // non-double Scalar.
  Eigen::Tensor<double, 1> xEval = x.template cast<double>();

  int n = static_cast<int>(xEval.dimension(0));

  Eigen::Tensor<double, 1> muEval =
    recycleToLength(asDenseTensor<1>(mu), n);

  // cholEval may alias the caller's own tensor (when no copy was needed
  // above); dmvt_chol's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through mu or chol, so this cast is safe.
  return dmvt_chol(xEval.data(), muEval.data(),
                    const_cast<double*>(cholEval.data()), df, n,
                    prec_param, give_log, /*overwrite_inputs=*/1);
}

#endif // _NCOMPILER_DISTS_DMVT_CHOL_TENSOR
