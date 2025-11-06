/* OPENER (Do not edit this comment) */
#ifndef __calcInstr_nClass_CPP
#define __calcInstr_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "calcInstr_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      calcInstr_nClass::calcInstr_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "new_calcInstr_nClass")]]
    SEXP  new_calcInstr_nClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(calcInstr_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_new_calcInstr_nClass")]]
    void  set_CnClass_env_calcInstr_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(calcInstr_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_new_calcInstr_nClass")]]
    Rcpp::Environment  get_CnClass_env_calcInstr_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(calcInstr_nClass);;
}

NCOMPILER_INTERFACE(
calcInstr_nClass,
NCOMPILER_FIELDS(
field("nodeIndex", &calcInstr_nClass::nodeIndex),
field("nodeInstrVec", &calcInstr_nClass::nodeInstrVec)
),
NCOMPILER_METHODS()
)
#endif
