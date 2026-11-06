/* OPENER (Do not edit this comment) */
#ifndef __OptimResultList_CPP
#define __OptimResultList_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "OptimResultList_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

      OptimResultList::OptimResultList (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "OptimResultList_new")]]
    SEXP  new_OptimResultList (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(OptimResultList);;
}

// [[Rcpp::export(name = "set_CnClass_env_OptimResultList_new")]]
    void  set_CnClass_env_OptimResultList ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(OptimResultList, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_OptimResultList_new")]]
    Rcpp::Environment  get_CnClass_env_OptimResultList (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(OptimResultList);;
}

NCOMPILER_INTERFACE(
OptimResultList,
NCOMPILER_FIELDS(
field("par", &OptimResultList::par),
field("value", &OptimResultList::value),
field("hessian", &OptimResultList::hessian),
field("counts", &OptimResultList::counts),
field("convergence", &OptimResultList::convergence),
field("message", &OptimResultList::message)
),
NCOMPILER_METHODS()
)
#endif
