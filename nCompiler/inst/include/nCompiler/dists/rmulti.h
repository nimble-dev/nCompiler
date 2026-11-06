// The rmulti (multinomial random generation) kernel (raw arguments), ported
// from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// Unlike every other dist kernel ported so far, the output here is int*,
// not double*: Rf_rmultinom (the underlying Rmath routine) requires an int
// output buffer. As the original comment below notes, calling functions
// need to convert to/from double themselves -- see rmulti_tensor.h, which
// does exactly that to match the double-based convention of every other
// wrapper in this directory (matching how nimble's own nimArr_rmulti also
// converts).

#ifndef _NCOMPILER_DISTS_RMULTI
#define _NCOMPILER_DISTS_RMULTI

#include <R_ext/Random.h> // Rf_rmultinom
#include <Rmath.h>
#include "dists_utils.h"

// Calling functions need to copy first arg back and forth to double if
// needed.
// IMPORTANT: ans and size are int when sent to Rf_rmultinom, since Rmath's
// rmultinom has these types.
inline void rmulti(int* ans, double size, double* prob, int K) {
  double sumProb = 0.0;
  int i;

  // The original nimble code assigned R_NaN (a double) directly into this
  // int* array here, relying on the resulting int-truncation-of-NaN to
  // land on NA_INTEGER -- which it reliably does on the platforms nimble
  // targets, but is technically undefined behavior in C++. NA_INTEGER is
  // used explicitly instead, which is well-defined and produces the exact
  // same value (NA_INTEGER is INT_MIN, same as the original cast's result).
  if (ISNAN_ANY(prob, K) || ISNAN(size)) {
    for(i = 0; i < K; i++)
      ans[i] = NA_INTEGER;
    return;
  }

  for(i = 0; i < K; i++) {
    if(prob[i] < 0) {
      for(i = 0; i < K; i++)
        ans[i] = NA_INTEGER;
      return;
    }
    sumProb += prob[i];
  }
  if (sumProb <= 0.0) {   // given above check for neg probs, this now will only catch '== 0' cases
    for(i = 0; i < K; i++)
      ans[i] = NA_INTEGER;
    return;
  }
  for(i = 0; i < K; i++)
    prob[i] /= sumProb;
  // Rf_rmultinom (not the "rmultinom" convenience macro) is used here
  // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
  // undoRmath.h), so this header needs to work whether or not it's
  // included after Rcpp.h has already run.
  Rf_rmultinom((int) size, prob, K, ans);
}

#endif // _NCOMPILER_DISTS_RMULTI
