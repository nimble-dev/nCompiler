#ifndef __OptimControlList_CPP_PREDEF
#define __OptimControlList_CPP_PREDEF
// preamble to enable Eigen errors
#ifndef NCOMPILER_EIGEN_ERRORS_ENABLED
#define NCOMPILER_EIGEN_ERRORS_ENABLED

// temporarily disable NDEBUG to let Eigen load with error support
#ifdef NDEBUG
# define NCOMPILER_EIGEN_NDEBUG_DISABLED
# undef NDEBUG
#endif

// check-flag so we only throw the primary Eigen error, instead of deep errors
static bool eigen_did_assert = false;

// flag must be manually reset before running Eigen code to enable trapping
#define RESET_EIGEN_ERRORS {eigen_did_assert = false;}

// custom definition of eigen_assert
#define eigen_assert(X) {if(!eigen_did_assert && !(X)) { eigen_did_assert = true; throw std::runtime_error(#X); }}

#include <RcppEigen.h>
#include <Rcpp.h>

// re-enable NDEBUG if it was originally enabled
#ifdef NCOMPILER_EIGEN_NDEBUG_DISABLED
# define NDEBUG
#endif

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

#endif // NCOMPILER_EIGEN_ERRORS_ENABLED

#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include <nCompiler/nCompiler_core.h>
#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/nCompiler_TBB.h>
#include <Rmath.h>
#include <math.h>
#include "OptimControlList.h"
#include <nCompiler/nCompiler_Eigen_fxns.h>
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppEigenAD)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

void  OptimControlList::nFun_1_NFID_1 (  )  {
RESET_EIGEN_ERRORS
flex_(trace) = 0.0;
flex_(fnscale) = 1.0;
parscale = createTensor<double, 1>(1.0, 1.0);
ndeps = createTensor<double, 1>(0.001, 1.0);
flex_(abstol) = -((1 * std::numeric_limits<double>::infinity()));
reltol = std::sqrt(std::numeric_limits<double>::epsilon());
maxit = NA_INTEGER;
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

// [[Rcpp::export]]
SEXP  new_OptimControlList (  )  {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(OptimControlList);
}

// [[Rcpp::export]]
void  set_CnClass_env_OptimControlList ( SEXP env )  {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(OptimControlList, env);
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
method("initToDefaults", &OptimControlList::nFun_1_NFID_1)
)
)
#endif
