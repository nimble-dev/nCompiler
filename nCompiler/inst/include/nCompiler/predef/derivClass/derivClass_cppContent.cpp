/* OPENER (Do not edit this comment) */
#ifndef __derivClass_CPP
#define __derivClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "derivClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      derivClass::derivClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "derivClass_new")]]
    SEXP  new_derivClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(derivClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_derivClass_new")]]
    void  set_CnClass_env_derivClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(derivClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_derivClass_new")]]
    Rcpp::Environment  get_CnClass_env_derivClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(derivClass);;
}

NCOMPILER_INTERFACE(
derivClass,
NCOMPILER_FIELDS(
field("value", &derivClass::value),
field("gradient", &derivClass::gradient),
field("hessian", &derivClass::hessian)
),
NCOMPILER_METHODS()
)
#endif
