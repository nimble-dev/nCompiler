// The dcat (categorical) density kernel (raw double* arguments), ported
// from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// x is a scalar category (1, ..., K). prob is never mutated.
//
// See also dcat_tensor.h for an Eigen::Tensor-argument wrapper around this
// kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_DCAT
#define _NCOMPILER_DISTS_DCAT

#include <Rmath.h>
#include "dists_utils.h"

inline double dcat(double x, double* prob, int K, int give_log) {
  if (R_IsNA(x) || R_IsNA_ANY(prob, K))
    return NA_REAL;
  if (R_IsNaN(x) || R_IsNaN_ANY(prob, K))
    return R_NaN;

  double sumProb(0.0);
  for(int i = 0; i < K; i++) {
    if(prob[i] < 0) ML_ERR_return_NAN;
    sumProb += prob[i];
  }

  // Expanded from Utils.h's R_D_nonint_check and R_D__0 macros.
  if (R_D_nonint(x)) {
    MATHLIB_WARNING("non-integer x = %f", x);
    return give_log ? R_NegInf : 0.0;
  }
  x = R_D_forceint(x);

  if(x > K || x < 1) return give_log ? R_NegInf : 0.0;
  return give_log ? log(prob[(int) x - 1]) - log(sumProb) : prob[(int) x - 1]/sumProb;
}

#endif // _NCOMPILER_DISTS_DCAT
