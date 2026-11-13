// The rcat (categorical random generation) kernel (raw double* arguments),
// ported from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// Returns a category in 1, ..., K as a double. prob is never mutated.
//
// See also rcat_tensor.h for an Eigen::Tensor-argument wrapper around this
// kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_RCAT
#define _NCOMPILER_DISTS_RCAT

#include <R_ext/Random.h> // unif_rand
#include <Rmath.h>
#include "dists_utils.h"

inline double rcat(double* prob, int K) {
  double prob_cum = prob[0];
  int value = 1;

  if (ISNAN_ANY(prob, K) )
    return R_NaN; // but casting to integer in creates NA

  double sumProb(0.0);
  for(int i = 0; i < K; i++) {
    if(prob[i] < 0) return NA_REAL;  // rpois, rbinom give NA with invalid params; rnorm gives NaN
    sumProb += prob[i];
  }

  double u = unif_rand() * sumProb;
  while(u > prob_cum && value < K) {
    prob_cum += prob[value];
    value++;
  }

  return (double) value;
}

#endif // _NCOMPILER_DISTS_RCAT
