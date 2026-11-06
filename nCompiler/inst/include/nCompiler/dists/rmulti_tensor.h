// Eigen::Tensor-argument wrapper around the rmulti random-generation
// kernel (see rmulti.h). Templated code like this can only ever be
// header-only -- the compiler must see the definition wherever it's
// instantiated -- so this lives in inst/include regardless of the
// package-DLL/on-the-fly-compilation split that motivates rmulti.h itself.

#ifndef _NCOMPILER_DISTS_RMULTI_TENSOR
#define _NCOMPILER_DISTS_RMULTI_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "rmulti.h"

// Eigen::Tensor wrapper for rmulti.
//
// prob is accepted as an arbitrary Eigen::Tensor expression built up at the
// call site, but -- unlike the read-only prob arguments elsewhere in this
// directory -- rmulti's kernel normalizes its working copy of prob in
// place (prob[i] /= sumProb) before calling Rf_rmultinom. So prob is
// always materialized into a private buffer here (never asDenseTensor's
// no-copy reference path), the same treatment given to arguments that get
// mutated elsewhere (e.g. chol in rwish_chol_tensor.h).
//
// rmulti's raw kernel writes into an int* buffer (Rf_rmultinom requires
// one), but every other wrapper in this directory works in double, so this
// wrapper draws into a temporary int buffer and converts to
// Eigen::Tensor<double, 1> for the result, mapping NA_INTEGER to R_NaN
// (matching the R_NaN used by every other dist kernel here to signal
// invalid input, rather than R's separate NA_REAL) -- matching what
// nimble's own nimArr_rmulti does at its call site.
//
// No GetRNGstate()/PutRNGstate() bracketing here, matching the other
// r-function wrappers: code that reaches this wrapper is invoked from R
// through Rcpp, which already brackets RNG state around the outer .Call
// boundary (RNGScope). The plain-.Call DLL path (dists.cpp's C_rmulti)
// isn't routed through Rcpp, so it keeps its own bracketing -- that's
// unchanged and still correct.
template<typename TensorExprProb>
Eigen::Tensor<double, 1> rmulti(int n, // ignored for now, to be used in the future.
                                double size, const TensorExprProb &prob) {
  Eigen::Tensor<double, 1> probEval = prob.template cast<double>();

  int K = static_cast<int>(probEval.dimension(0));

  Eigen::Tensor<int, 1> ansInt(K);
  rmulti(ansInt.data(), size, probEval.data(), K);

  Eigen::Tensor<double, 1> ans(K);
  for (int i = 0; i < K; i++)
    ans(i) = (ansInt(i) == NA_INTEGER) ? R_NaN : static_cast<double>(ansInt(i));

  return ans;
}

#endif // _NCOMPILER_DISTS_RMULTI_TENSOR
