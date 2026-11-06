/* OPENER (Do not edit this comment) */
#ifndef __EigenDecomp_CPP
#define __EigenDecomp_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "EigenDecomp_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      EigenDecomp::EigenDecomp (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "EigenDecomp_new")]]
    SEXP  new_EigenDecomp (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(EigenDecomp);;
}

// [[Rcpp::export(name = "set_CnClass_env_EigenDecomp_new")]]
    void  set_CnClass_env_EigenDecomp ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(EigenDecomp, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_EigenDecomp_new")]]
    Rcpp::Environment  get_CnClass_env_EigenDecomp (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(EigenDecomp);;
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
