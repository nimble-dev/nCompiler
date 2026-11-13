// The rinvwish_chol (inverse Wishart random generation) kernel (raw double*
// arguments), ported from nimble's dists.cpp.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// See also rinvwish_chol_tensor.h for an Eigen::Tensor-argument wrapper
// around this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_RINVWISH_CHOL
#define _NCOMPILER_DISTS_RINVWISH_CHOL

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

#include <R_ext/Random.h> // norm_rand
#include <Rmath.h>
#include "dists_utils.h"

// Rf_rchisq (not the "rchisq" convenience macro) is used here deliberately:
// Rcpp's sugar layer #undefs that macro (Rcpp/sugar/undoRmath.h), so this
// header needs to work whether or not it's included after Rcpp.h has
// already run.
inline void rinvwish_chol(double *Z, double* chol, double df, int p, double scale_param, int overwrite_inputs) {
  char uploU('U');
  char uploL('L');
  char sideL('L');
  char diag('N');
  char transT('T');
  char transN('N');
  double alpha(1.0);
  double beta(0.0);

  // calculations here generate W ~ Wi(S) for scale_param = 1 and W ~ Wi(S^-1) for scale_param = 0,
  // and then invert the result, giving InvWi(S^-1) for scale_param = 1 and InvWi(S) for scale_param = 0,
  // because scale for Wishart is inverse of scale for inverse-Wishart
  // therefore generate under opposite parameterization
  scale_param = 1 - scale_param;


  double* cholCopy;
  int i, j, uind, lind;

  if (ISNAN_ANY(chol, p*p) || ISNAN(df) || ISNAN(scale_param)) {
    for(j = 0; j < p*p; j++)
      Z[j] = R_NaN;
    return;
  }

  // also covers df < 0
  if(df < (double) p) {
    for(j = 0; j < p*p; j++)
      Z[j] = R_NaN;
    return;
  }

  // fill diags with sqrts of chi-squares and upper triangle (for scale_param) with std normals - crossproduct of result is standardized Wishart; based on rWishart in stats package
  for(j = 0; j < p; j++) {
    Z[j*p + j] = sqrt(Rf_rchisq(df - (double) j));
    for(i = 0; i < j; i++) {
      uind = i + j * p, /* upper triangle index */
      lind = j + i * p; /* lower triangle index */
      Z[(scale_param ? uind : lind)] = norm_rand();
      Z[(scale_param ? lind : uind)] = 0;
    }
  }

  if(overwrite_inputs)
    cholCopy = chol;
  else {
    cholCopy = new double[p*p];
    for(i = 0; i < p*p; i++)
      cholCopy[i] = chol[i];
  }

  // with S parameterization: t(chol)%*%t(Z)%*%Z%*%chol is Wishart-distributed, so take inverse based on pieces
  // with R parameterization: inverse(chol)%*%Z%*%t(Z)%*%inverse(t(chol)) is Wishart-distributed, again inverse of pieces
  if(scale_param) {
    // Z %*% chol
    F77_CALL(dtrmm)(&sideL, &uploU, &transN, &diag, &p, &p, &alpha, Z, &p, cholCopy, &p FCONE FCONE FCONE FCONE);
    double* iden = new double[p*p];
    for(j = 0; j < p; j++)
      for(i = 0; i < p; i++)
        if(i == j) iden[j*p+i] = 1.0; else iden[j*p+i] = 0.0;
    // inverse(Z %*% chol)
    F77_CALL(dtrsm)(&sideL, &uploU, &transN, &diag, &p, &p, &alpha, cholCopy, &p, iden, &p FCONE FCONE FCONE FCONE);
    // crossproduct of result
    F77_CALL(dgemm)(&transN, &transT, &p, &p, &p, &alpha, iden, &p, iden, &p, &beta, Z, &p FCONE FCONE);
    delete [] iden;
  } else {
    // solve(Z, chol)
    F77_CALL(dtrsm)(&sideL, &uploL, &transN, &diag, &p, &p, &alpha, Z, &p, cholCopy, &p FCONE FCONE FCONE FCONE);
    // crossproduct of result
    F77_CALL(dgemm)(&transT, &transN, &p, &p, &p, &alpha, cholCopy, &p, cholCopy, &p, &beta, Z, &p FCONE FCONE);
  }

  if(!overwrite_inputs)
    delete [] cholCopy;
}

#endif // _NCOMPILER_DISTS_RINVWISH_CHOL
