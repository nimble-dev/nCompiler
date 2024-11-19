#define NCOMPILER_USES_EIGEN
#define NCOMPILER_USES_NCLASS_INTERFACE

#include <nCompiler/nCompiler_omnibus_first_cpp.h>

#include <Rmath.h>
#include <math.h>
// #include <nCompiler/cWiseUnary_external.cpp>
#include <iostream>
#include <Rinternals.h>

using namespace Rcpp;
// In nimbleCompiler, nimblePlugin turns on cpp11. Not sure if following is needed:
/* Not needed (should be set by nimble plugins) [[Rcpp::plugins(cpp11)]] */ 
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppEigenAD)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
