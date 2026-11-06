// Eigen::Tensor-argument wrapper around the dmnorm_chol density kernel
// (see dmnorm_chol.h). Templated code like this can only ever be header-
// only -- the compiler must see the definition wherever it's instantiated
// -- so this lives in inst/include regardless of the package-DLL/
// on-the-fly-compilation split that motivates dmnorm_chol.h itself.

#ifndef _NCOMPILER_DISTS_DMNORM_CHOL_TENSOR
#define _NCOMPILER_DISTS_DMNORM_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "dmnorm_chol.h"

// Eigen::Tensor wrapper for dmnorm_chol.
//
// x, mean, and chol are accepted as arbitrary Eigen::Tensor expressions
// built up at the call site. chol is read-only, so asDenseTensor reuses it
// directly (no copy) when it's already a concrete tensor, and only
// materializes a copy when the call site passed a lazy expression. x is
// always copied into its own buffer: dmnorm_chol overwrites its working copy
// of x in place, and we must not mutate the caller's tensor as a side
// effect. mean follows nimble's recycling rule (see recycleToLength in
// dists_tensor_utils.h): if it's shorter than x, its elements are reused
// cyclically to fill out x's length, matching the original C_dmnorm_chol's
// full_mean construction -- so mean is always materialized to x's length
// here rather than taking asDenseTensor's no-copy path.
template<typename TensorExprX, typename TensorExprMean, typename TensorExprChol>
double dmnorm_chol(const TensorExprX &x, const TensorExprMean &mean,
                    const TensorExprChol &chol, double prec_param, int give_log) {
  const auto &cholEval = asDenseTensor<2>(chol);
  // x is materialized unconditionally (never via asDenseTensor's reference
  // path), so cast<double>() is needed here too in case TensorExprX has a
  // non-double Scalar.
  Eigen::Tensor<double, 1> xEval = x.template cast<double>();

  int n = static_cast<int>(xEval.dimension(0));

  Eigen::Tensor<double, 1> meanEval =
    recycleToLength(asDenseTensor<1>(mean), n);

  // cholEval may alias the caller's own tensor (when no copy was needed
  // above); dmnorm_chol's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through mean or chol, so this cast is safe.
  return dmnorm_chol(xEval.data(), meanEval.data(),
                      const_cast<double*>(cholEval.data()), n,
                      prec_param, give_log, /*overwrite_inputs=*/1);
}

#endif // _NCOMPILER_DISTS_DMNORM_CHOL_TENSOR
