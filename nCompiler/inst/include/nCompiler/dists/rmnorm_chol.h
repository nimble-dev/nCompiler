// The rmnorm_chol (multivariate normal random generation) kernel (raw
// double* arguments), ported from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// Unlike dmnorm_chol, there's no overwrite_inputs concept here: ans is
// always a fresh output buffer, never aliased to an input.
//
// See also rmnorm_chol_tensor.h for an Eigen::Tensor-argument wrapper
// around this kernel, for use from nCompile-generated code. That wrapper
// (like the original C_rmnorm_chol R-facing function) also brackets the
// call with GetRNGstate()/PutRNGstate() and applies the recycling rule for
// mean -- neither of those live in this raw kernel, matching how the
// original C_rmnorm_chol handled them at the R-call boundary rather than
// inside rmnorm_chol itself.

#ifndef _NCOMPILER_DISTS_RMNORM_CHOL
#define _NCOMPILER_DISTS_RMNORM_CHOL

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

#include <R_ext/Random.h> // GetRNGstate, PutRNGstate, norm_rand
#include <Rmath.h>
#include "dists_utils.h"

inline void rmnorm_chol(double *ans, double* mean, double* chol, int n,
                         double prec_param) {
  char uplo('U');
  char transPrec('N');
  char transCov('T');
  char diag('N');
  int lda(n);
  int incx(1);

  int i, j;

  if (ISNAN_ANY(mean, n) || ISNAN_ANY(chol, n*n) || ISNAN(prec_param)) {
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

  // do upper-triangular solve or (transpose) multiply
  // dtr{s,m}v is a BLAS level-2 function
  if(prec_param) F77_CALL(dtrsv)(&uplo, &transPrec, &diag, &n, chol, &lda, ans, &incx FCONE FCONE FCONE);
  else F77_CALL(dtrmv)(&uplo, &transCov, &diag, &n, chol, &lda, ans, &incx FCONE FCONE FCONE);

  for(i = 0; i < n; i++)
    ans[i] += mean[i];
}

#endif // _NCOMPILER_DISTS_RMNORM_CHOL
