// The rwish_chol (Wishart random generation) kernel (raw double*
// arguments), ported from nimble's dists.cpp. Like dwish_chol, the output
// Z here is itself a p x p matrix, not a vector.
//
// Header-only and self-contained for the same reason as dmnorm_chol.h: the
// package DLL and code nCompile generates and compiles on a user's machine
// are separate shared objects that cannot reliably link against each other,
// so each side compiles its own copy of this source instead.
//
// Unlike rmnorm_chol/rmvt_chol, this kernel does have an overwrite_inputs
// parameter: it controls whether chol itself is used as scratch space
// during the computation (overwrite_inputs = 1) or an internal copy is
// allocated (overwrite_inputs = 0). The original C_rwish_chol always
// passed 0, since chol there is the user's own R object and must not be
// mutated. See rwish_chol_tensor.h for how the wrapper handles this.
//
// See also rwish_chol_tensor.h for an Eigen::Tensor-argument wrapper
// around this kernel, for use from nCompile-generated code.

#ifndef _NCOMPILER_DISTS_RWISH_CHOL
#define _NCOMPILER_DISTS_RWISH_CHOL

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

inline void rwish_chol(double *Z, double* chol, double df, int p,
                        double scale_param, int overwrite_inputs) {
  char uplo('U');
  char sideL('L');
  char diag('N');
  char transT('T');
  char transN('N');
  double alpha(1.0);
  double beta(0.0);

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
  //
  // Rf_rchisq (not the "rchisq" convenience macro) is used here
  // deliberately: Rcpp's sugar layer #undefs that macro (Rcpp/sugar/
  // undoRmath.h), so this header needs to work whether or not it's
  // included after Rcpp.h has already run.
  for(j = 0; j < p; j++) {
    Z[j*p + j] = sqrt(Rf_rchisq(df - (double) j));
    for(i = 0; i < j; i++) {
      uind = i + j * p; /* upper triangle index */
      lind = j + i * p; /* lower triangle index */
      Z[(scale_param ? uind : lind)] = norm_rand();
      Z[(scale_param ? lind : uind)] = 0;
    }
  }

  // multiply Z*chol, both upper triangular or solve(chol, Z^T)
  // would be more efficient if make use of fact that right-most matrix is triangular, but no available BLAS routine and hand-coding would eliminate use of threading and might well not be faster
  if(overwrite_inputs)
    cholCopy = chol;
  else {
    cholCopy = new double[p*p];
    if(scale_param)
      for(i = 0; i < p*p; i++)
        cholCopy[i] = chol[i];
  }
  if(scale_param) F77_CALL(dtrmm)(&sideL, &uplo, &transN, &diag, &p, &p, &alpha, Z, &p, cholCopy, &p FCONE FCONE FCONE FCONE);
  else F77_CALL(dtrsm)(&sideL, &uplo, &transN, &diag, &p, &p, &alpha, chol, &p, Z, &p FCONE FCONE FCONE FCONE);

  // cp result to Z or chol so can be used as matrix to multiply against and overwrite
  if(scale_param) {
    for(j = 0; j < p*p; j++)
      Z[j] = cholCopy[j];
  } else {
    for(j = 0; j < p*p; j++)
      cholCopy[j] = Z[j];
  }

  // do crossprod of result
  // for dtrmm call, again this would be more efficient if use fact that RHS upper triangular, but no available BLAS routine and hand-coding would eliminate use of threading and might well not be faster
  if(scale_param) F77_CALL(dtrmm)(&sideL, &uplo, &transT, &diag, &p, &p, &alpha, cholCopy, &p, Z, &p FCONE FCONE FCONE FCONE);
  else F77_CALL(dgemm)(&transN, &transT, &p, &p, &p, &alpha, cholCopy, &p, cholCopy, &p, &beta, Z, &p FCONE FCONE);
  if(!overwrite_inputs)
    delete [] cholCopy;
}

#endif // _NCOMPILER_DISTS_RWISH_CHOL
