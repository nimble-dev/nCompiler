// Eigen::Tensor-argument wrapper around the dmulti density kernel (see
// dmulti.h). Templated code like this can only ever be header-only -- the
// compiler must see the definition wherever it's instantiated -- so this
// lives in inst/include regardless of the package-DLL/on-the-fly-
// compilation split that motivates dmulti.h itself.

#ifndef _NCOMPILER_DISTS_DMULTI_TENSOR
#define _NCOMPILER_DISTS_DMULTI_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "dmulti.h"

// Eigen::Tensor wrapper for dmulti.
//
// prob is read-only, so asDenseTensor reuses it directly (no copy) when
// it's already a concrete tensor, and only materializes a copy when the
// call site passed a lazy expression. x is always copied into its own
// buffer: dmulti unconditionally rounds each entry of its working copy of
// x to the nearest integer in place, and we must not mutate the caller's
// tensor as a side effect.
template<typename TensorExprX, typename TensorExprProb>
double dmulti(const TensorExprX &x, double size, const TensorExprProb &prob,
              int give_log) {
  const auto &probEval = asDenseTensor<1>(prob);
  Eigen::Tensor<double, 1> xEval = x.template cast<double>();

  int K = static_cast<int>(probEval.dimension(0));

  // probEval may alias the caller's own tensor (when no copy was needed
  // above); dmulti's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through prob, so this cast is safe.
  return dmulti(xEval.data(), size, const_cast<double*>(probEval.data()), K,
                give_log);
}

#endif // _NCOMPILER_DISTS_DMULTI_TENSOR
