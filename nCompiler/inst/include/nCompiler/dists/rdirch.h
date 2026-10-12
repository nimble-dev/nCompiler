// The rdirch (Dirichlet random generation) kernel (raw double* arguments),
// ported from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// Like ddirch, alpha is never mutated (no overwrite_inputs concept), and
// ans is always a fresh output buffer.
//
// See also rdirch_tensor.h for an Eigen::Tensor-argument wrapper around
// this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_RDIRCH
#define _NCOMPILER_DISTS_RDIRCH

#include <R_ext/Random.h> // rgamma is declared here (via Rmath.h too)
#include <Rmath.h>
#include "dists_utils.h"

inline void rdirch(double* ans, double* alpha, int K) {
  int i, j;

  if (ISNAN_ANY(alpha, K)) {
    for(j = 0; j < K; j++)
      ans[j] = R_NaN;
    return;
  }

  double sum(0.0);
  // Rf_rgamma (not the "rgamma" convenience macro) is used here
  // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
  // undoRmath.h), so this header needs to work whether or not it's
  // included after Rcpp.h has already run.
  for(i = 0; i < K; i++) {
    if(alpha[i] <= 0.0) {
      for(j = 0; j < K; j++)
        ans[j] = R_NaN;
      return;
    }
    ans[i] = Rf_rgamma(alpha[i], 1);
    sum += ans[i];
  }
  for(i = 0; i < K; i++) {
    ans[i] /= sum;
  }
}

#endif // _NCOMPILER_DISTS_RDIRCH
