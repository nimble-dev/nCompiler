/* OPENER (Do not edit this comment) */
#ifndef __calcInstrList_nC_CPP
#define __calcInstrList_nC_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "calcInstrList_nC_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      calcInstrList_nC::calcInstrList_nC (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "new_calcInstrList_nC")]]
    SEXP  new_calcInstrList_nC (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(calcInstrList_nC);;
}

// [[Rcpp::export(name = "set_CnClass_env_new_calcInstrList_nC")]]
    void  set_CnClass_env_calcInstrList_nC ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(calcInstrList_nC, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_new_calcInstrList_nC")]]
    Rcpp::Environment  get_CnClass_env_calcInstrList_nC (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(calcInstrList_nC);;
}

NCOMPILER_INTERFACE(
calcInstrList_nC,
NCOMPILER_FIELDS(
field("calcInstrList", &calcInstrList_nC::calcInstrList)
),
NCOMPILER_METHODS()
)
#endif
