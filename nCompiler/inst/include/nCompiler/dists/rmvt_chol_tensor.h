// Eigen::Tensor-argument wrapper around the rmvt_chol random-generation
// kernel (see rmvt_chol.h). Templated code like this can only ever be
// header-only -- the compiler must see the definition wherever it's
// instantiated -- so this lives in inst/include regardless of the
// package-DLL/on-the-fly-compilation split that motivates rmvt_chol.h
// itself.

#ifndef _NCOMPILER_DISTS_RMVT_CHOL_TENSOR
#define _NCOMPILER_DISTS_RMVT_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "rmvt_chol.h"

// Eigen::Tensor wrapper for rmvt_chol.
//
// mu and chol are accepted as arbitrary Eigen::Tensor expressions built up
// at the call site. chol is read-only, so asDenseTensor reuses it directly
// (no copy) when it's already a concrete tensor, and only materializes a
// copy when the call site passed a lazy expression. mu follows nimble's
// recycling rule (see recycleToLength in dists_tensor_utils.h): if it's
// shorter than the output length n (determined by chol's dimension), its
// elements are reused cyclically to fill out length n, matching the
// original C_rmvt_chol's full_mu construction.
//
// Unlike dmvt_chol_tensor.h, there's no x/overwrite_inputs concern here:
// the output is always a fresh buffer.
//
// No GetRNGstate()/PutRNGstate() bracketing here, matching
// rmnorm_chol_tensor.h: code that reaches this wrapper is invoked from R
// through Rcpp, which already brackets RNG state around the outer .Call
// boundary (RNGScope). The plain-.Call DLL path (dists.cpp's C_rmvt_chol)
// isn't routed through Rcpp, so it keeps its own bracketing -- that's
// unchanged and still correct.
template<typename TensorExprMu, typename TensorExprChol>
Eigen::Tensor<double, 1> rmvt_chol(int n, // ignored for now, to be used in the future.
const TensorExprMu &mu,
                                    const TensorExprChol &chol, double df,
                                    double prec_param) {
  const auto &cholEval = asDenseTensor<2>(chol);
  int n = static_cast<int>(cholEval.dimension(0));

  Eigen::Tensor<double, 1> muEval =
    recycleToLength(asDenseTensor<1>(mu), n);

  Eigen::Tensor<double, 1> ans(n);

  // cholEval may alias the caller's own tensor (when no copy was needed
  // above); rmvt_chol's double* signature is non-const only because it
  // predates const-correctness conventions here, but it never writes
  // through chol, so this cast is safe.
  rmvt_chol(ans.data(), muEval.data(),
            const_cast<double*>(cholEval.data()), df, n, prec_param);

  return ans;
}

#endif // _NCOMPILER_DISTS_RMVT_CHOL_TENSOR
