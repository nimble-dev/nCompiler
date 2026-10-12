// The dmnorm_chol density kernel (raw double* arguments), ported from
// nimble's dists.cpp.
//
// This is deliberately header-only and self-contained (rather than living
// only in src/dists.cpp) so that the identical implementation can be
// compiled both into the package DLL (for the R-facing C_dmnorm_chol /
// .Call path) and into code nCompile generates and compiles on a user's
// machine. Those are two separate shared objects, and a package DLL (often
// built by CRAN) cannot be relied on to link against code compiled locally
// on a user's computer, across all supported OSes/compilers. Rather than
// linking across that boundary, each side just compiles its own copy of
// this same source, the way header-only libraries like Eigen already do.
//
// See also dmnorm_chol_tensor.h for an Eigen::Tensor-argument wrapper
// around this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_DMNORM_CHOL
#define _NCOMPILER_DISTS_DMNORM_CHOL

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

inline double dmnorm_chol(double* x, double* mean, double* chol, int n,
                           double prec_param, int give_log,
                           int overwrite_inputs) {
  char uplo('U');
  char transPrec('N');
  char transCov('T');
  char diag('N');
  int lda(n);
  int incx(1);
  double* xCopy;

  double dens = -n * M_LN_SQRT_2PI;
  int i;
  // add diagonals of Cholesky

  if (R_IsNA_ANY(x, n) || R_IsNA_ANY(mean, n) || R_IsNA_ANY(chol, n*n) || R_IsNA(prec_param))
    return NA_REAL;
  if (R_IsNaN_ANY(x, n) || R_IsNaN_ANY(mean, n) || R_IsNaN_ANY(chol, n*n) || R_IsNaN(prec_param))
    return R_NaN;

  if(!R_FINITE_ANY(x, n) || !R_FINITE_ANY(mean, n) || !R_FINITE_ANY(chol, n*n))
    return give_log ? R_NegInf : 0.0;

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
      xCopy[i] -= mean[i];
  } else {
    xCopy = new double[n];
    for(i = 0; i < n; i++)
      xCopy[i] = x[i] - mean[i];
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

  dens += -0.5 * tmp;

  if(!overwrite_inputs)
    delete [] xCopy;

  return give_log ? dens : exp(dens);
}

#endif // _NCOMPILER_DISTS_DMNORM_CHOL
