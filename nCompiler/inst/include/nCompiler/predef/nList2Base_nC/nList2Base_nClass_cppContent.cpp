/* OPENER (Do not edit this comment) */
#ifndef __nList2Base_nClass_CPP
#define __nList2Base_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "nList2Base_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

    bool  nList2Base_nClass::ping (  ) {
RESET_EIGEN_ERRORS
return(true);
}
      nList2Base_nClass::nList2Base_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "set_CnClass_env_nList2Base_nClass_new")]]
    void  set_CnClass_env_nList2Base_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(nList2Base_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_nList2Base_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_nList2Base_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(nList2Base_nClass);;
}

NCOMPILER_INTERFACE(
nList2Base_nClass,
NCOMPILER_FIELDS(),
NCOMPILER_METHODS(
method("ping", &nList2Base_nClass::ping, args({{}}))
)
)
#endif
