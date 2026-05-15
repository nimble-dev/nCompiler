/* OPENER (Do not edit this comment) */
#ifndef __OptimControlList_CPP
#define __OptimControlList_CPP
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "OptimControlList_c_.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

    void  OptimControlList::nFun_2 (  ) {
RESET_EIGEN_ERRORS
flex_(trace) = 0.0;
flex_(fnscale) = 1.0;
parscale = nCompiler::nEval_<Eigen::Tensor<double, 1> >::go(createTensor<double, 1>(1.0, 1.0));
ndeps = nCompiler::nEval_<Eigen::Tensor<double, 1> >::go(createTensor<double, 1>(0.001, 1.0));
flex_(abstol) = -((1 * std::numeric_limits<double>::infinity()));
reltol = std::sqrt(std::numeric_limits<double>::epsilon());;
maxit = NA_INTEGER;;
flex_(alpha) = 1.0;
flex_(beta) = 0.5;
flex_(gamma) = 2.0;
flex_(REPORT) = 10.0;
flex_(type) = 1.0;
flex_(lmm) = 5.0;
flex_(factr) = 1e+07;
flex_(pgtol) = 0.0;
flex_(tmax) = 10.0;
flex_(temp) = 10.0;
}
      OptimControlList::OptimControlList (  ) {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export(name = "OptimControlList_new")]]
    SEXP  new_OptimControlList (  ) {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(OptimControlList);;
}

// [[Rcpp::export(name = "set_CnClass_env_OptimControlList_new")]]
    void  set_CnClass_env_OptimControlList ( SEXP env ) {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(OptimControlList, env);;
}

// [[Rcpp::export(name = "get_CnClass_env_OptimControlList_new")]]
    Rcpp::Environment  get_CnClass_env_OptimControlList (  ) {
RESET_EIGEN_ERRORS
return GET_CNCLASS_ENV(OptimControlList);;
}

NCOMPILER_INTERFACE(
OptimControlList,
NCOMPILER_FIELDS(
field("trace", &OptimControlList::trace),
field("fnscale", &OptimControlList::fnscale),
field("parscale", &OptimControlList::parscale),
field("ndeps", &OptimControlList::ndeps),
field("maxit", &OptimControlList::maxit),
field("abstol", &OptimControlList::abstol),
field("reltol", &OptimControlList::reltol),
field("alpha", &OptimControlList::alpha),
field("beta", &OptimControlList::beta),
field("gamma", &OptimControlList::gamma),
field("REPORT", &OptimControlList::REPORT),
field("type", &OptimControlList::type),
field("lmm", &OptimControlList::lmm),
field("factr", &OptimControlList::factr),
field("pgtol", &OptimControlList::pgtol),
field("tmax", &OptimControlList::tmax),
field("temp", &OptimControlList::temp)
),
NCOMPILER_METHODS(
method("initToDefaults", &OptimControlList::nFun_2, args({{}}))
)
)
#endif
