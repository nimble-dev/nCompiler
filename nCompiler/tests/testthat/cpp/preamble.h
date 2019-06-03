#include <RcppEigen.h>
#include <Rcpp.h>

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <nCompiler/nCompiler_Eigen.h>
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
