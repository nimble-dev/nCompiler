#ifndef __nFun_1_NFID_1_CPP
#define __nFun_1_NFID_1_CPP
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
#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/nCompiler_TBB.h>
#include <Rmath.h>
#include <math.h>
#include <iostream>
#include <nCompiler/nCompiler_core.h>
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppEigenAD)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

// build a crude stack trace of nFunctions.  each level of the trace captures
// the function name in [StringPair].first and the step being executed in
// [StringPair].second.  The step may be updated several times within a single
// function call, as the function executes.
typedef std::pair<std::string, std::string> StringPair;
static std::vector<StringPair> msgs;
#define NC_TRY_PUSHFUN(x) { msgs.emplace_back(StringPair(x, "")); }
#define NC_TRY_SETSTEP(x) { msgs.back().second = x; }
#define NC_TRY_POPFUN { msgs.pop_back(); }

// [[Rcpp::export]]
Eigen::Tensor<double, 2>  nFun_1_NFID_1 ( Eigen::Tensor<double, 2> x, Eigen::Tensor<double, 2> y )  {
RESET_EIGEN_ERRORS
try{
    // TODO: insert NC_TRY_PUSHFUN into nFunction code during nCompiler debugging stage
    NC_TRY_PUSHFUN("nf");  // nf is the nFunction object's name in R
    Eigen::Tensor<double, 2> z;
    // TODO: insert NC_TRY_SETSTEP into nFunction code during nCompiler debugging stage,
    //   but do not do this recursively; only insert NC_TRY_SETSTEP calls on
    //   top-level code$arg[[i]] entries
    NC_TRY_SETSTEP("z = x+y");
    z = x+y;
    // TODO: insert NC_TRY_POPFUN into nFunction code during nCompiler debugging stage;
    //   the code tree will need to be recursively searched to hunt for return
    //   statements nested within code since return statements can be used to
    //   end processing "early".
    NC_TRY_POPFUN;
    return(z);
    // TODO: insert NC_TRY_POPFUN as the last code$arg element in nFunction
    //  code during nCompiler debugging stage; this is only necessary for an
    //  nFunction that does not end with a return statement, as may occur if
    //  an nFunction is used with reference semantics to modify an external
    //  object
    NC_TRY_POPFUN;
} catch(const std::exception & e) {
    std::string s;
    std::string indent = "    ";
    for (const auto &msg : msgs) {
        s += "Error in " + msg.first + " at \"" + msg.second + "\":\n";
        indent += "  ";
    }
    s += indent + e.what();
    throw std::runtime_error(s);
}
}
#endif
#ifndef __loadedObjectEnv_CPP
#define __loadedObjectEnv_CPP
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif

#ifndef _ONTHEFLY_LOADEDOBJECTENV
#define _ONTHEFLY_LOADEDOBJECTENV
SEXP loadedObjectEnv(SEXP Xptr) {
Rcpp::Environment nc("package:nCompiler");
Rcpp::Function newLOE = nc["new.loadedObjectEnv"];
return newLOE(Xptr);
}
#endif

#endif
