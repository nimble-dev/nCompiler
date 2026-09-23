// The dwish_chol (Wishart) density kernel (raw double* arguments), ported
// from nimble's dists.cpp. Unlike dmnorm_chol/dmvt_chol, the random
// variable x here is itself a p x p matrix, not a vector.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// See also dwish_chol_tensor.h for an Eigen::Tensor-argument wrapper around
// this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_DWISH_CHOL
#define _NCOMPILER_DISTS_DWISH_CHOL

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

inline double dwish_chol(double* x, double* chol, double df, int p,
                          double scale_param, int give_log,
                          int overwrite_inputs) {
  char uplo('U');
  char sideL('L');
  char sideR('R');
  char diag('N');
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

  if(!R_FINITE_ANY(x, p*p) || !R_FINITE_ANY(chol, p*p))
    return give_log ? R_NegInf : 0.0;

  // Rf_lgammafn (not the "lgammafn" convenience macro) is used here
  // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
  // undoRmath.h, to avoid polluting the global namespace with names like
  // beta/choose/gamma), so this header needs to work whether or not it's
  // included after Rcpp.h has already run.
  double dens = -(df*p/2 * M_LN2 + p*(p-1)*M_LN_SQRT_PI/2);
  for(i = 0; i < p; i++)
    dens -= Rf_lgammafn((df - i) / 2);

  if(scale_param) {
    for(i = 0; i < p*p; i += p + 1)
      dens -= df * log(chol[i]);
  } else {
    for(i = 0; i < p*p; i += p + 1)
      dens += df * log(chol[i]);
  }

  // determinant of x using Cholesky:
  if(overwrite_inputs && (int) scale_param)  // if !scale_param we need x below
    xChol = x;
  else {
    xChol = new double[p*p];
    // only need upper triangle for dpotrf chol calculation
    for(j = 0; j < p; j++)
      for(i = 0; i <= j; i++)
        xChol[j*p+i] = x[j*p+i];
  }
  F77_CALL(dpotrf)(&uplo, &p, xChol, &p, &info FCONE);
  for(i = 0; i < p*p; i += p + 1)
    dens += (df - p - 1) * log(xChol[i]);

  // R %*% x = t(chol) %*% chol %*% x (could also do with chol(x) but no more efficient
  // solve(S, x) = crossproduct( chol(x) %*% inverse(chol) )

  // dtr{m,s}m is a BLAS level-3 function
  double tmp_dens = 0.0;
  if(scale_param) {
    // chol(x) %*% inverse(chol)
    // need lower triangle of xChol to have zeros as dtrsm assumes it is full matrix
    for(j = 0; j < p-1; j++)
      for(i = j+1; i < p; i++)
        xChol[j*p+i] = 0.0;
    F77_CALL(dtrsm)(&sideR, &uplo, &transN, &diag, &p, &p, &alpha,
                    chol, &p, xChol, &p FCONE FCONE FCONE FCONE);
    // trace of crossproduct of result is sum of squares of elements
    for(j = 0; j < p; j++)
      for(i = 0; i <= j; i++)
        tmp_dens += xChol[j*p+i]*xChol[j*p+i];
  } else {
    double* xCopy;
    if(overwrite_inputs)
      xCopy = x;
    else {
      xCopy = new double[p*p];
      for(i = 0; i < p*p; i++)
        xCopy[i] = x[i];
    }
    // chol %*% x
    F77_CALL(dtrmm)(&sideL, &uplo, &transN, &diag, &p, &p, &alpha,
           chol, &p, xCopy, &p FCONE FCONE FCONE FCONE);
    // trace crossproduct of t(chol) with result is sum of product of upper-triangular elements
    for(j = 0; j < p; j++) {
      for(i = 0; i <= j; i++) {
        tmp_dens += xCopy[j*p+i] * chol[j*p+i];
      }
    }
    if(!overwrite_inputs)
      delete [] xCopy;
  }

  if(!(overwrite_inputs && (int) scale_param))
    delete [] xChol;

    // attempt to improve above calcs by doing efficient U^T U multiply followed by direct product multiply, however this would not make use of threading provided by BLAS and even with one thread seems to be no faster
    // U^T*U directly followed by direct product with x
    /*
    double tmp_summand;
    int minij;
    for(j = 0; j < p; j++)
      for(i = 0; i < p; i++) {
        tmp_summand = 0.0;
        minij = i <= j ? i : j;
        for(int k = 0; k < minij; k++)
          tmp_summand += chol[j*p+k]*chol[i*p+k]; // U^T U
        // double if not on diagonal to account for direct product of lower triangle too
        if(i != j) tmp_summand *= 2;
        tmp_dens += xCopy[j*p+i] * tmp_summand;
      }
    */

  dens += -0.5 * tmp_dens;

  return give_log ? dens : exp(dens);
}

#endif // _NCOMPILER_DISTS_DWISH_CHOL
