//GENERATED BY generatedPredefinedCpp.R. DO NOT EDIT BY HAND
//--------------------------------------
//--------------------------------------
// test_predefined
//--------------------------------------
#ifndef __test_predefined_H
#define __test_predefined_H
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
#include <Rinternals.h>
#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/nCompiler_TBB.h>
#include <nCompiler/nCompiler_core.h>
#include <nCompiler/nCompiler_class_factory.h>
#include <nCompiler/nCompiler_class_interface.h>

class test_predefined : public genericInterfaceC<test_predefined> {
public:
  double a;
};

SEXP  new_test_predefined (  );


#endif
//--------------------------------------
//--------------------------------------
// nC_derivClass
//--------------------------------------
#ifndef __nC_derivClass_H
#define __nC_derivClass_H
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
#include <Rinternals.h>
#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/nCompiler_TBB.h>
#include <nCompiler/nCompiler_core.h>
#include <nCompiler/nCompiler_class_factory.h>
#include <nCompiler/nCompiler_class_interface.h>

class nC_derivClass : public genericInterfaceC<nC_derivClass> {
public:
  Eigen::Tensor<double, 1> value;
  Eigen::Tensor<double, 2> gradient;
  Eigen::Tensor<double, 3> hessian;
};

SEXP  new_nC_derivClass (  );


#endif
//--------------------------------------
//--------------------------------------
// EigenDecomp
//--------------------------------------
#ifndef __EigenDecomp_H
#define __EigenDecomp_H
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
#include <Rinternals.h>
#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/nCompiler_TBB.h>
#include <nCompiler/nCompiler_core.h>
#include <nCompiler/nCompiler_class_factory.h>
#include <nCompiler/nCompiler_class_interface.h>

class EigenDecomp : public genericInterfaceC<EigenDecomp> {
public:
  Eigen::Tensor<double, 1> values;
  Eigen::Tensor<double, 2> vectors;
};

SEXP  new_EigenDecomp (  );


#endif
