// The dmulti (multinomial) density kernel (raw double* arguments), ported
// from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// dmulti always mutates x in place (rounding each entry to the nearest
// integer via R_D_forceint), unconditionally -- there's no
// overwrite_inputs parameter here, unlike dmnorm_chol/dmvt_chol/dwish_chol
// -- so the Eigen::Tensor wrapper (dmulti_tensor.h) must always materialize
// its own private copy of x.
//
// See also dmulti_tensor.h for an Eigen::Tensor-argument wrapper around
// this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_DMULTI
#define _NCOMPILER_DISTS_DMULTI

#include <cfloat> // DBL_EPSILON
#include <Rmath.h>
#include "dists_utils.h"

// Calling functions need to copy first arg to int if needed.
inline double dmulti(double* x, double size, double* prob, int K, int give_log) {
  double sumProb(0.0);
  double sumX(0.0);
  double logSumProb;

  if (R_IsNA_ANY(x, K) || R_IsNA_ANY(prob, K) || R_IsNA(size))
    return NA_REAL;
  if (R_IsNaN_ANY(x, K) || R_IsNaN_ANY(prob, K) || R_IsNaN(size))
    return R_NaN;

  if(R_D_negInonint(size))
    ML_ERR_return_NAN;
  size = R_D_forceint(size);

  // Rf_lgammafn (not the "lgammafn" convenience macro) is used here
  // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
  // undoRmath.h, to avoid polluting the global namespace with names like
  // beta/choose/gamma), so this header needs to work whether or not it's
  // included after Rcpp.h has already run.
  double dens = Rf_lgammafn(size + 1);
  for(int i = 0; i < K; i++) {
    if (prob[i] < 0) ML_ERR_return_NAN;
    if (R_D_nonint(x[i])) {
      MATHLIB_WARNING("non-integer x = %f", x[i]);
      return give_log ? R_NegInf : 0.0;
    }
    if (x[i] < 0 || !R_FINITE(x[i])) return give_log ? R_NegInf : 0.0;

    x[i] = R_D_forceint(x[i]);
    sumProb += prob[i];
    sumX += x[i];
  }
  logSumProb = log(sumProb);

  for(int i = 0; i < K; i++) {
    if(!(x[i] == 0.0 && prob[i] == 0.0))
      dens += x[i]*(log(prob[i]) - logSumProb) - Rf_lgammafn(x[i] + 1);
  }

  if(sumX > size + 10*DBL_EPSILON || sumX < size - 10*DBL_EPSILON) {
    return give_log ? R_NegInf : 0.0;
  }

  return give_log ? dens : exp(dens);
}

#endif // _NCOMPILER_DISTS_DMULTI
