#define NCOMPILER_USES_EIGEN
#define NCOMPILER_USES_NCLASS_INTERFACE

#include <nCompiler/nCompiler_omnibus.h>

#include <Rmath.h>
#include <math.h>
// #include <nCompiler/cWiseUnary_external.cpp>
#include <iostream>
#include <Rinternals.h>

using namespace Rcpp;
// In nimbleCompiler, nimblePlugin turns on cpp11. Not sure if following is needed:
/* Not needed (should be set by nimble plugins) [[Rcpp::plugins(cpp11)]] */ 
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
/* Not needed, will be replaced [[Rcpp::depends(RcppEigenAD)]] */
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]
