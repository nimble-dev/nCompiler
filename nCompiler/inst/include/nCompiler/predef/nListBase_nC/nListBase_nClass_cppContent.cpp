/* OPENER (Do not edit this comment) */
#ifndef __nListBase_nClass_CPP
#define __nListBase_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "nListBase_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

    bool  nListBase_nClass::ping (  ) {
RESET_EIGEN_ERRORS
return(true);
}
      nListBase_nClass::nListBase_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "set_CnClass_env_nListBase_nClass_new")]]
    void  set_CnClass_env_nListBase_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(nListBase_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_nListBase_nClass_new")]]
    Rcpp::Environment  get_CnClass_env_nListBase_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(nListBase_nClass);;
}

NCOMPILER_INTERFACE(
nListBase_nClass,
NCOMPILER_FIELDS(),
NCOMPILER_METHODS(
method("ping", &nListBase_nClass::ping, args({{}}))
)
)
#endif
