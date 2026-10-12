// The dmvt_chol (multivariate t) density kernel (raw double* arguments),
// ported from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// See also dmvt_chol_tensor.h for an Eigen::Tensor-argument wrapper around
// this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_DMVT_CHOL
#define _NCOMPILER_DISTS_DMVT_CHOL

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

#include <Rmath.h>
#include "dists_utils.h"

inline double dmvt_chol(double* x, double* mu, double* chol, double df,
                         int n, double prec_param, int give_log,
                         int overwrite_inputs) {
  char uplo('U');
  char transPrec('N');
  char transCov('T');
  char diag('N');
  int lda(n);
  int incx(1);
  double* xCopy;

  // Rf_lgammafn (not the "lgammafn" convenience macro) is used here
  // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
  // undoRmath.h, to avoid polluting the global namespace with names like
  // beta/choose/gamma), so this header needs to work whether or not it's
  // included after Rcpp.h has already run.
  double dens = Rf_lgammafn((df + n) / 2) - Rf_lgammafn(df / 2) - n * M_LN_SQRT_PI - n * log(df) / 2;
  int i;

  if (R_IsNA_ANY(x, n) || R_IsNA_ANY(mu, n) || R_IsNA_ANY(chol, n*n) || R_IsNA(df) || R_IsNA(prec_param))
    return NA_REAL;
  if (R_IsNaN_ANY(x, n) || R_IsNaN_ANY(mu, n) || R_IsNaN_ANY(chol, n*n) || R_IsNA(df) || R_IsNaN(prec_param))
    return R_NaN;

  if(!R_FINITE_ANY(x, n) || !R_FINITE_ANY(mu, n) || !R_FINITE_ANY(chol, n*n))
    return give_log ? R_NegInf : 0.0;

  // add diagonals of Cholesky
  if(prec_param) {
    for(i = 0; i < n*n; i += n + 1)
      dens += log(chol[i]);
  } else {
    for(i = 0; i < n*n; i += n + 1)
      dens -= log(chol[i]);
  }

  if(overwrite_inputs) {
    xCopy = x;
    for(i = 0; i < n; i++)
      xCopy[i] -= mu[i];
  } else {
    xCopy = new double[n];
    for(i = 0; i < n; i++)
      xCopy[i] = x[i] - mu[i];
  }

  // do matrix-vector multiply with upper-triangular matrix stored column-wise as full n x n matrix (prec parameterization)
  // or upper-triangular (transpose) solve (cov parameterization)
  // dtr{m,s}v is a BLAS level-2 function
  if(prec_param) F77_CALL(dtrmv)(&uplo, &transPrec, &diag, &n, chol, &lda, xCopy, &incx FCONE FCONE FCONE);
  else F77_CALL(dtrsv)(&uplo, &transCov, &diag, &n, chol, &lda, xCopy, &incx FCONE FCONE FCONE);

  // sum of squares to calculate quadratic form
  double tmp = 0.0;
  for(i = 0; i < n; i++)
    tmp += xCopy[i] * xCopy[i];

  dens += -0.5 * (df + n) * log(1 + tmp / df);

  if(!overwrite_inputs)
    delete [] xCopy;

  return give_log ? dens : exp(dens);
}

#endif // _NCOMPILER_DISTS_DMVT_CHOL
