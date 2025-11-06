/* OPENER (Do not edit this comment) */
#ifndef __modelBase_nClass_CPP
#define __modelBase_nClass_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "modelBase_nClass_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

    bool  modelBase_nClass::ping (  ) {
RESET_EIGEN_ERRORS
return(true);
}
    double  modelBase_nClass::calculate ( std::shared_ptr<calcInstr_nClass> calcInstr ) {
RESET_EIGEN_ERRORS
Rprintf("modelBase_nClass calculate (should not see this)\n");;
return(0.0);
}
      modelBase_nClass::modelBase_nClass (  ) {
RESET_EIGEN_ERRORS
}

modelBase_nClass::~modelBase_nClass () {};

// [[Rcpp::export(name = "set_CnClass_env_new_modelBase_nClass")]]
    void  set_CnClass_env_modelBase_nClass ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(modelBase_nClass, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_new_modelBase_nClass")]]
    Rcpp::Environment  get_CnClass_env_modelBase_nClass (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(modelBase_nClass);;
}

NCOMPILER_INTERFACE(
modelBase_nClass,
NCOMPILER_FIELDS(),
NCOMPILER_METHODS(
method("ping", &modelBase_nClass::ping, args({{}})),
method("calculate", &modelBase_nClass::calculate, args({{arg("calcInstr",copy)}}))
)
)


#endif
