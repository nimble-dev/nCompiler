// The rmvt_chol (multivariate t random generation) kernel (raw double*
// arguments), ported from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// Like rmnorm_chol, there's no overwrite_inputs concept here: ans is
// always a fresh output buffer, never aliased to an input.
//
// See also rmvt_chol_tensor.h for an Eigen::Tensor-argument wrapper around
// this kernel, for use from nCompile-generated code. That wrapper (like the
// original C_rmvt_chol R-facing function) applies the recycling rule for
// mu, which doesn't live in this raw kernel, matching how the original
// C_rmvt_chol handled it at the R-call boundary rather than inside
// rmvt_chol itself.

#ifndef _NCOMPILER_DISTS_RMVT_CHOL
#define _NCOMPILER_DISTS_RMVT_CHOL

// Based on Writing R Extensions 6.6.1 and mgcv's mgcv.h
#define USE_FC_LEN_T
#include <Rconfig.h>
#include <R_ext/BLAS.h>
/* If we are compiling with a version of R before FCONE and the explicit
   supplying of extra arguments was introduced, then FCONE has to be
   defined */
#ifndef FCONE
#define FCONE
#endif

#include <R_ext/Random.h> // norm_rand
#include <Rmath.h>
#include "dists_utils.h"

inline void rmvt_chol(double *ans, double* mu, double* chol, double df,
                       int n, double prec_param) {
  char uplo('U');
  char transPrec('N');
  char transCov('T');
  char diag('N');
  int lda(n);
  int incx(1);

  int i, j;

  if (ISNAN_ANY(mu, n) || ISNAN_ANY(chol, n*n) || ISNAN(df) || ISNAN(prec_param)) {
    for(j = 0; j < n; j++)
      ans[j] = R_NaN;
    return;
  }

  if(!R_FINITE_ANY(chol, n*n)) {
    for(j = 0; j < n; j++)
      ans[j] = R_NaN;
    return;
  }

  for(i = 0; i < n; i++)
    ans[i] = norm_rand();

  // sample from chi-squared and calculate scaling factor.
  // Rf_rchisq (not the "rchisq" convenience macro) is used here
  // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
  // undoRmath.h), so this header needs to work whether or not it's
  // included after Rcpp.h has already run.
  double scaling = sqrt(df / Rf_rchisq(df));

  // do upper-triangular solve or (transpose) multiply
  // dtr{s,m}v is a BLAS level-2 function
  if(prec_param) F77_CALL(dtrsv)(&uplo, &transPrec, &diag, &n, chol, &lda, ans, &incx FCONE FCONE FCONE);
  else F77_CALL(dtrmv)(&uplo, &transCov, &diag, &n, chol, &lda, ans, &incx FCONE FCONE FCONE);

  for(i = 0; i < n; i++)
    ans[i] = mu[i] + ans[i] * scaling;
}

#endif // _NCOMPILER_DISTS_RMVT_CHOL
