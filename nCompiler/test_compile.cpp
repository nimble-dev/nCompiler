#ifndef __nFun_2_NFID_2_CPP
#define __nFun_2_NFID_2_CPP
#define NCOMPILER_HANDLE_EIGEN_ERRORS
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include "test_compile.h"
#include <nCompiler/nCompiler_omnibus_first_cpp.h>
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppEigenAD)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

// [[Rcpp::export]]
double  nFun_2_NFID_2 ( double x )  {
RESET_EIGEN_ERRORS
return(x+100.0);
}
#endif
