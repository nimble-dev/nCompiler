/* OPENER (Do not edit this comment) */
#ifndef __nodeInstr_nClass_CPP
#define __nodeInstr_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "nodeInstr_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      nodeInstr_nClass::nodeInstr_nClass (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "new_nodeInstr_nClass")]]
    SEXP  new_nodeInstr_nClass (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(nodeInstr_nClass);;
}

// [[Rcpp::export(name = "set_CnClass_env_new_nodeInstr_nClass")]]
    void  set_CnClass_env_nodeInstr_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(nodeInstr_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_new_nodeInstr_nClass")]]
    Rcpp::Environment  get_CnClass_env_nodeInstr_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(nodeInstr_nClass);;
}

NCOMPILER_INTERFACE(
nodeInstr_nClass,
NCOMPILER_FIELDS(
field("methodInstr", &nodeInstr_nClass::methodInstr),
field("indsInstrVec", &nodeInstr_nClass::indsInstrVec)
),
NCOMPILER_METHODS()
)
#endif
