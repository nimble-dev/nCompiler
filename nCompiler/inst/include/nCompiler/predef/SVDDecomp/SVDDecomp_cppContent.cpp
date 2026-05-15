/* OPENER (Do not edit this comment) */
#ifndef __SVDDecomp_CPP
#define __SVDDecomp_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "SVDDecomp_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      SVDDecomp::SVDDecomp (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "SVDDecomp_new")]]
    SEXP  new_SVDDecomp (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(SVDDecomp);;
}

// [[Rcpp::export(name = "set_CnClass_env_SVDDecomp_new")]]
    void  set_CnClass_env_SVDDecomp ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(SVDDecomp, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_SVDDecomp_new")]]
    Rcpp::Environment  get_CnClass_env_SVDDecomp (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(SVDDecomp);;
}

NCOMPILER_INTERFACE(
SVDDecomp,
NCOMPILER_FIELDS(
field("d", &SVDDecomp::d),
field("v", &SVDDecomp::v),
field("u", &SVDDecomp::u)
),
NCOMPILER_METHODS()
)
#endif
