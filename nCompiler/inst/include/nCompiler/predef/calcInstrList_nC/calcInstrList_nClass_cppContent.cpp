/* OPENER (Do not edit this comment) */
#ifndef __calcInstrList_nClass_CPP
#define __calcInstrList_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "calcInstrList_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      calcInstrList_nClass::calcInstrList_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "new_calcInstrList_nClass")]]
    SEXP  new_calcInstrList_nClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(calcInstrList_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_new_calcInstrList_nClass")]]
    void  set_CnClass_env_calcInstrList_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(calcInstrList_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_new_calcInstrList_nClass")]]
    Rcpp::Environment  get_CnClass_env_calcInstrList_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(calcInstrList_nClass);;
}

NCOMPILER_INTERFACE(
calcInstrList_nClass,
NCOMPILER_FIELDS(
field("calcInstrList", &calcInstrList_nClass::calcInstrList)
),
NCOMPILER_METHODS()
)
#endif
