// The dinvwish_chol (inverse Wishart) density kernel (raw double* arguments),
// ported from nimble's dists.cpp. The random variable x is a p x p matrix.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// See also dinvwish_chol_tensor.h for an Eigen::Tensor-argument wrapper
// around this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_DINVWISH_CHOL
#define _NCOMPILER_DISTS_DINVWISH_CHOL

// Based on Writing R Extensions 6.6.1 and mgcv's mgcv.h
#define USE_FC_LEN_T
#include <Rconfig.h>
#include <R_ext/BLAS.h>
#include <R_ext/Lapack.h>
/* If we are compiling with a version of R before FCONE and the explicit
   supplying of extra arguments was introduced, then FCONE has to be
   defined */
#ifndef FCONE
#define FCONE
#endif

#include <Rmath.h>
#include "dists_utils.h"

// Rf_lgammafn (not the "lgammafn" convenience macro) is used here
// deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
// undoRmath.h), so this header needs to work whether or not it's included
// after Rcpp.h has already run.
inline double dinvwish_chol(double* x, double* chol, double df, int p, double scale_param, int give_log, int overwrite_inputs) {
  char uplo('U');
  char sideL('L');
  char sideR('R');
  char diag('N');
  char transT('T');
  char transN('N');
  int info(0);
  double alpha(1.0);
  double* xChol;

  int i, j;

  if (R_IsNA_ANY(x, p*p) || R_IsNA_ANY(chol, p*p) || R_IsNA(df) || R_IsNA(scale_param))
    return NA_REAL;
  if (R_IsNaN_ANY(x, p*p) || R_IsNaN_ANY(chol, p*p) || R_IsNaN(df) || R_IsNaN(scale_param))
    return R_NaN;

  // also covers df < 0
  if(df < (double) p) ML_ERR_return_NAN;

  if(!R_FINITE_ANY(x, p*p) || !R_FINITE_ANY(chol, p*p)) return R_D__0;

  double dens = -(df*p/2 * M_LN2 + p*(p-1)*M_LN_SQRT_PI/2);
  for(i = 0; i < p; i++)
    dens -= Rf_lgammafn((df - i) / 2);

  // determinant of S or R
  if(scale_param) {
    for(i = 0; i < p*p; i += p + 1)
      dens += df * log(chol[i]);
  } else {
    for(i = 0; i < p*p; i += p + 1)
      dens -= df * log(chol[i]);
  }

  // determinant of x using Cholesky:
  if(overwrite_inputs)
    xChol = x;
  else {
    xChol = new double[p*p];
    for(i = 0; i < p*p; i++)
      xChol[i] = x[i];
  }
  F77_CALL(dpotrf)(&uplo, &p, xChol, &p, &info FCONE);
  for(i = 0; i < p*p; i += p + 1)
    dens -= (df + p + 1) * log(xChol[i]);

  double tmp_dens = 0.0;

  // do upper-triangular solve for scale parameterization
  // or upper-triangular multiplies for rate parameterization

  // S %*% inverse(x) = crossprod(chol %*% inverse(chol(x))
  // inverse(R) %*% inverse(x) = crossproduct(solve(t(chol(x)), inverse(chol)))
  // dtr{m,s}m is a BLAS level-3 function
  if(scale_param) {
    double* cholCopy;
    if(overwrite_inputs)
      cholCopy = chol;
    else {
      cholCopy = new double[p*p];
      for(i = 0; i < p*p; i++)
        cholCopy[i] = chol[i];
    }
    // chol %*% inverse(chol(x))
    F77_CALL(dtrsm)(&sideR, &uplo, &transN, &diag, &p, &p, &alpha,
           xChol, &p, cholCopy, &p FCONE FCONE FCONE FCONE);
    // trace of crossproduct is sum of elements squared
    for(j = 0; j < p; j++) {
      for(i = 0; i <= j; i++) {
        tmp_dens += cholCopy[j*p+i] * cholCopy[j*p+i];
      }
    }
    if(!overwrite_inputs)
      delete [] cholCopy;
  } else {
    double* iden = new double[p*p];
    for(j = 0; j < p; j++)
      for(i = 0; i < p; i++)
        if(i == j) iden[j*p+i] = 1.0; else iden[j*p+i] = 0.0;
    // inverse of chol
    F77_CALL(dtrsm)(&sideL, &uplo, &transN, &diag, &p, &p, &alpha,
                    chol, &p, iden, &p FCONE FCONE FCONE FCONE);
    // solve(t(chol(x)), result)
    F77_CALL(dtrsm)(&sideL, &uplo, &transT, &diag, &p, &p, &alpha,
                    xChol, &p, iden, &p FCONE FCONE FCONE FCONE);
    // trace of crossproduct is sum of elements squared
    for(j = 0; j < p; j++) {
      for(i = 0; i < p; i++) {
        tmp_dens += iden[j*p+i] * iden[j*p+i];
      }
    }
    delete [] iden;
  }
  if(!overwrite_inputs)
    delete [] xChol;
  dens += -0.5 * tmp_dens;
  return give_log ? dens : exp(dens);
}

#endif // _NCOMPILER_DISTS_DINVWISH_CHOL
