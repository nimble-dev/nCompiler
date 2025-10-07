/* OPENER (Do not edit this comment) */
#ifndef __nodeFxnBase_nClass_CPP
#define __nodeFxnBase_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "nodeFxnBase_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

    bool  nodeFxnBase_nClass::ping (  ) {
RESET_EIGEN_ERRORS
return(true);
}
    double  nodeFxnBase_nClass::calculate ( Eigen::Tensor<int, 1> to_do ) {
RESET_EIGEN_ERRORS
return(0.0);
}
      nodeFxnBase_nClass::nodeFxnBase_nClass (  ) {
RESET_EIGEN_ERRORS
}

nodeFxnBase_nClass::~nodeFxnBase_nClass() {};


// [[Rcpp::export(name = "set_CnClass_env_new_nodeFxnBase_nClass")]]
    void  set_CnClass_env_nodeFxnBase_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(nodeFxnBase_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_new_nodeFxnBase_nClass")]]
    Rcpp::Environment  get_CnClass_env_nodeFxnBase_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(nodeFxnBase_nClass);;
}

NCOMPILER_INTERFACE(
nodeFxnBase_nClass,
NCOMPILER_FIELDS(),
NCOMPILER_METHODS(
method("ping", &nodeFxnBase_nClass::ping, args({{}})),
method("calculate", &nodeFxnBase_nClass::calculate, args({{arg("to_do",copy)}}))
)
)
#endif
