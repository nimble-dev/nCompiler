// Eigen::Tensor-argument wrapper around the rmnorm_chol random-generation
// kernel (see rmnorm_chol.h). Templated code like this can only ever be
// header-only -- the compiler must see the definition wherever it's
// instantiated -- so this lives in inst/include regardless of the
// package-DLL/on-the-fly-compilation split that motivates rmnorm_chol.h
// itself.

#ifndef _NCOMPILER_DISTS_RMNORM_CHOL_TENSOR
#define _NCOMPILER_DISTS_RMNORM_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "rmnorm_chol.h"

// Eigen::Tensor wrapper for rmnorm_chol.
//
// mean and chol are accepted as arbitrary Eigen::Tensor expressions built
// up at the call site. chol is read-only, so asDenseTensor reuses it
// directly (no copy) when it's already a concrete tensor, and only
// materializes a copy when the call site passed a lazy expression. mean
// follows nimble's recycling rule (see recycleToLength in
// dists_tensor_utils.h): if it's shorter than the output length n
// (determined by chol's dimension), its elements are reused cyclically to
// fill out length n, matching the original C_rmnorm_chol's full_mean
// construction.
//
// Unlike dmnorm_chol_tensor.h, there's no x/overwrite_inputs concern here:
// the output is always a fresh buffer.
//
// No GetRNGstate()/PutRNGstate() bracketing here, unlike the original
// C_rmnorm_chol: code that reaches this wrapper is invoked from R through
// Rcpp, which brackets RNG state around the outer .Call boundary already
// (RNGScope). Bracketing individual inner draws like this one would be
// both redundant and the wrong altitude for it. The plain-.Call DLL path
// (dists.cpp's C_rmnorm_chol) isn't routed through Rcpp, so it keeps its
// own bracketing -- that's unchanged and still correct.
template<typename TensorExprMean, typename TensorExprChol>
Eigen::Tensor<double, 1> rmnorm_chol(const TensorExprMean &mean,
                                      const TensorExprChol &chol,
                                      double prec_param) {
  const auto &cholEval = asDenseTensor<2>(chol);
  int n = static_cast<int>(cholEval.dimension(0));

  Eigen::Tensor<double, 1> meanEval =
    recycleToLength(asDenseTensor<1>(mean), n);

  Eigen::Tensor<double, 1> ans(n);

  // cholEval may alias the caller's own tensor (when no copy was needed
  // above); rmnorm_chol's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through chol, so this cast is safe.
  rmnorm_chol(ans.data(), meanEval.data(),
              const_cast<double*>(cholEval.data()), n, prec_param);

  return ans;
}

#endif // _NCOMPILER_DISTS_RMNORM_CHOL_TENSOR
