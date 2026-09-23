// Eigen::Tensor-argument wrapper around the rdirch random-generation
// kernel (see rdirch.h). Templated code like this can only ever be
// header-only -- the compiler must see the definition wherever it's
// instantiated -- so this lives in inst/include regardless of the
// package-DLL/on-the-fly-compilation split that motivates rdirch.h itself.

#ifndef _NCOMPILER_DISTS_RDIRCH_TENSOR
#define _NCOMPILER_DISTS_RDIRCH_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "rdirch.h"

// Eigen::Tensor wrapper for rdirch.
//
// alpha is accepted as an arbitrary Eigen::Tensor expression built up at
// the call site. It's read-only (like in ddirch), so asDenseTensor reuses
// it directly (no copy) when it's already a concrete tensor, and only
// materializes a copy when the call site passed a lazy expression. The
// output is always a fresh buffer.
//
// No GetRNGstate()/PutRNGstate() bracketing here, matching the other
// r-function wrappers (rmnorm_chol_tensor.h, rmvt_chol_tensor.h,
// rwish_chol_tensor.h): code that reaches this wrapper is invoked from R
// through Rcpp, which already brackets RNG state around the outer .Call
// boundary (RNGScope). The plain-.Call DLL path (dists.cpp's C_rdirch)
// isn't routed through Rcpp, so it keeps its own bracketing -- that's
// unchanged and still correct.
template<typename TensorExprAlpha>
Eigen::Tensor<double, 1> rdirch(const TensorExprAlpha &alpha) {
  const auto &alphaEval = asDenseTensor<1>(alpha);

  int K = static_cast<int>(alphaEval.dimension(0));

  Eigen::Tensor<double, 1> ans(K);

  // alphaEval may alias the caller's own tensor (when no copy was needed
  // above); rdirch's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through alpha, so this cast is safe.
  rdirch(ans.data(), const_cast<double*>(alphaEval.data()), K);

  return ans;
}

#endif // _NCOMPILER_DISTS_RDIRCH_TENSOR
