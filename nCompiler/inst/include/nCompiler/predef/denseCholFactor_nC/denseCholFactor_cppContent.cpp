/* OPENER (Do not edit this comment) */
#ifndef __denseCholFactor_CPP
#define __denseCholFactor_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "denseCholFactor_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      denseCholFactor::denseCholFactor (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "denseCholFactor_new")]]
    SEXP  new_denseCholFactor (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(denseCholFactor);;
}

// [[Rcpp::export(name = "set_CnClass_env_denseCholFactor_new")]]
    void  set_CnClass_env_denseCholFactor ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(denseCholFactor, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_denseCholFactor_new")]]
    Rcpp::Environment  get_CnClass_env_denseCholFactor (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(denseCholFactor);;
}

NCOMPILER_INTERFACE(
denseCholFactor,
NCOMPILER_FIELDS(),
NCOMPILER_METHODS()
)
#endif
