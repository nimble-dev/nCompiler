/* OPENER (Do not edit this comment) */
#ifndef __sparseCholFactor_CPP
#define __sparseCholFactor_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "sparseCholFactor_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      sparseCholFactor::sparseCholFactor (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "sparseCholFactor_new")]]
    SEXP  new_sparseCholFactor (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(sparseCholFactor);;
}

// [[Rcpp::export(name = "set_CnClass_env_sparseCholFactor_new")]]
    void  set_CnClass_env_sparseCholFactor ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(sparseCholFactor, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_sparseCholFactor_new")]]
    Rcpp::Environment  get_CnClass_env_sparseCholFactor (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(sparseCholFactor);;
}

NCOMPILER_INTERFACE(
sparseCholFactor,
NCOMPILER_FIELDS(),
NCOMPILER_METHODS()
)
#endif
