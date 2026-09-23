// The ddirch (Dirichlet) density kernel (raw double* arguments), ported
// from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// Unlike the other dist kernels ported so far, ddirch never mutates x or
// alpha (no BLAS/LAPACK, no in-place overwrite), so there's no
// overwrite_inputs parameter here, and the Eigen::Tensor wrapper
// (ddirch_tensor.h) can use the no-copy reference path for both arguments.
//
// See also ddirch_tensor.h for an Eigen::Tensor-argument wrapper around
// this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_DDIRCH
#define _NCOMPILER_DISTS_DDIRCH

#include <cfloat> // DBL_EPSILON
#include <Rmath.h>
#include "dists_utils.h"

inline double ddirch(double* x, double* alpha, int K, int give_log) {
  double sumAlpha(0.0);
  double sumX(0.0);
  double dens(0.0);

  if (R_IsNA_ANY(x, K) || R_IsNA_ANY(alpha, K))
    return NA_REAL;
  if (R_IsNaN_ANY(x, K) || R_IsNaN_ANY(alpha, K))
    return R_NaN;

  for(int i = 0; i < K; i++) {
    if(alpha[i] <= 0.0) ML_ERR_return_NAN;
    if(x[i] < 0.0 || x[i] > 1.0) return give_log ? R_NegInf : 0.0;
    // Rf_lgammafn (not the "lgammafn" convenience macro) is used here
    // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
    // undoRmath.h, to avoid polluting the global namespace with names like
    // beta/choose/gamma), so this header needs to work whether or not it's
    // included after Rcpp.h has already run.
    dens += (alpha[i]-1) * log(x[i]) - Rf_lgammafn(alpha[i]);
    sumAlpha += alpha[i];
    sumX += x[i];
  }
  if(sumX > 1.0 + 10*DBL_EPSILON || sumX < 1.0 - 10*DBL_EPSILON) {
    return give_log ? R_NegInf : 0.0;
  }

  dens += Rf_lgammafn(sumAlpha);
  return give_log ? dens : exp(dens);
}

#endif // _NCOMPILER_DISTS_DDIRCH
