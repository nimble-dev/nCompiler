#ifndef PREAMBLE_
#define PREAMBLE_

// WORKAROUND: Only enable errors if requested via compiler flag
#ifdef NCOMPILER_ENABLE_EIGEN_ERRORS

// Background:
//   - Review of Eigen's source code shows that Eigen uses eigen_assert to 
//     trigger most runtime errors, including errors where users try to perform 
//     numerical operations on non-conformable tensors and matrices, such as 
//     binary operations a + b, and matrix multiplications a %*% b (R notation).
//   - Eigen only uses throw in a handful of places, limited to memory errors.
//
// Problem:
//   - by default, R (and Rcpp by extension) compiles code with the macro NDEBUG
//     defined, which causes Eigen to silence/disable eigen_assert.
//
// Strategy: 
//  - load Eigen with NDEBUG undefined, then re-enable NDEBUG
//  - define custom eigen_assert to throw errors instead of causing crashes
//    via calls to abort (which is Eigen's default behavior)
//
// Risks:
//  - if any internal Eigen code fails to use eigen_assert to intercept runtime 
//    conditions that would trigger lower-level, system calls to abort, then 
//    code generated using this workaround will cause R to crash.
//
// Source: 
//  https://stackoverflow.com/questions/56209693/is-there-any-way-to-make-rcpp-stop-defining-ndebug
//  https://stackoverflow.com/questions/24275714/how-to-override-dndebug-compile-flag-when-building-cython-module/24275850#24275850
//  https://cran.r-project.org/doc/manuals/R-exts.html

// temporarily disable NDEBUG
#ifdef NDEBUG
# define NDEBUG_DISABLED
# undef NDEBUG
#endif

//
// custom definition for eigen_assert 
//

// check-flag so we only throw the primary Eigen error, instead of deep errors
static bool eigen_did_assert = false;

// flag must be manually reset before running Eigen code to enable trapping
#define RESET_EIGEN_ERRORS {eigen_did_assert = false;}

// custom definition of eigen_assert
#define eigen_assert(X) {if(!eigen_did_assert && !(X)) { eigen_did_assert = true; throw std::runtime_error(#X); }}

// load Eigen
#include <RcppEigen.h>
#include <Rcpp.h>

// re-enable NDEBUG if it was originally enabled
#ifdef NDEBUG_DISABLED
# define NDEBUG
#endif

// END WORKAROUND
#endif NCOMPILER_ENABLE_EIGEN_ERRORS

// keep downstream code running if WORKAROUND is disabled
#ifndef NCOMPILER_ENABLE_EIGEN_ERRORS
# define RESET_EIGEN_ERRORS
// load Eigen normally
#include <RcppEigen.h>
#include <Rcpp.h>
#endif

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

#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
#include <iostream>
#include <nCompiler/nCompiler_Eigen.h>
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

#endif
