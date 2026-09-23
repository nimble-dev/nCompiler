// Eigen::Tensor-argument wrapper around the ddirch density kernel (see
// ddirch.h). Templated code like this can only ever be header-only -- the
// compiler must see the definition wherever it's instantiated -- so this
// lives in inst/include regardless of the package-DLL/on-the-fly-
// compilation split that motivates ddirch.h itself.

#ifndef _NCOMPILER_DISTS_DDIRCH_TENSOR
#define _NCOMPILER_DISTS_DDIRCH_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "ddirch.h"

// Eigen::Tensor wrapper for ddirch.
//
// Unlike dmnorm_chol/dmvt_chol/dwish_chol, ddirch never mutates x or alpha
// (no BLAS/LAPACK, no in-place overwrite), so both arguments can use
// asDenseTensor's no-copy reference path when they're already concrete
// tensors, and only get materialized when the call site passed a lazy
// expression.
template<typename TensorExprX, typename TensorExprAlpha>
double ddirch(const TensorExprX &x, const TensorExprAlpha &alpha, int give_log) {
  const auto &xEval = asDenseTensor<1>(x);
  const auto &alphaEval = asDenseTensor<1>(alpha);

  int K = static_cast<int>(alphaEval.dimension(0));

  // xEval/alphaEval may alias the caller's own tensors (when no copy was
  // needed above); ddirch's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through x or alpha, so this cast is safe.
  return ddirch(const_cast<double*>(xEval.data()),
                const_cast<double*>(alphaEval.data()), K, give_log);
}

#endif // _NCOMPILER_DISTS_DDIRCH_TENSOR
