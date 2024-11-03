#ifndef __EigenDecomp_CPP_PREDEF
#define __EigenDecomp_CPP_PREDEF
// preamble to enable Eigen errors
#ifndef NCOMPILER_EIGEN_ERRORS_ENABLED
#define NCOMPILER_EIGEN_ERRORS_ENABLED

// temporarily disable NDEBUG to let Eigen load with error support
#ifdef NDEBUG
# define NCOMPILER_EIGEN_NDEBUG_DISABLED
# undef NDEBUG
#endif

// check-flag so we only throw the primary Eigen error, instead of deep errors
static bool eigen_did_assert = false;

// flag must be manually reset before running Eigen code to enable trapping
#define RESET_EIGEN_ERRORS {eigen_did_assert = false;}

// custom definition of eigen_assert
#define eigen_assert(X) {if(!eigen_did_assert && !(X)) { eigen_did_assert = true; throw std::runtime_error(#X); }}

#include <RcppEigen.h>
#include <Rcpp.h>

// re-enable NDEBUG if it was originally enabled
#ifdef NCOMPILER_EIGEN_NDEBUG_DISABLED
# define NDEBUG
#endif

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

#endif // NCOMPILER_EIGEN_ERRORS_ENABLED

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include <nCompiler/nCompiler_core.h>
#include "EigenDecomp_c_.h"
#include <nCompiler/nCompiler_Eigen_fxns.h>
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppEigenAD)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]



// [[Rcpp::export]]
SEXP  new_EigenDecomp (  )  {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(EigenDecomp);
}

// [[Rcpp::export]]
void  set_CnClass_env_EigenDecomp ( SEXP env )  {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(EigenDecomp, env);
}

NCOMPILER_INTERFACE(
EigenDecomp,
NCOMPILER_FIELDS(
field("values", &EigenDecomp::values),
field("vectors", &EigenDecomp::vectors)
),
NCOMPILER_METHODS()
)
#endif
