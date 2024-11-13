#ifndef __test_predefined_CPP2
#define __test_predefined_CPP2
#define NCOMPILER_HANDLE_EIGEN_ERRORS
#define NCOMPILER_USES_EIGEN
#define NCOMPILER_USES_NCLASS_INTERFACE
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include <nCompiler/nCompiler_omnibus_first_cpp.h>
#include "test_predefined_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppEigenAD)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

  test_predefined::test_predefined (  )  {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export]]
SEXP  new_test_predefined (  )  {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(test_predefined);
}

// [[Rcpp::export]]
void  set_CnClass_env_test_predefined ( SEXP env )  {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(test_predefined, env);
}

NCOMPILER_INTERFACE(
test_predefined,
NCOMPILER_FIELDS(
field("a", &test_predefined::a)
),
NCOMPILER_METHODS()
)
#endif
