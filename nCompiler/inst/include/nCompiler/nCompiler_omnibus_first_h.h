// This file will manage includes that need to happen first,
// based on flags that have been set.
//
// Such use of flags may seem to risk complication, but in this
// case it simplifies what needs to be generated on the fly,
// and we hope the flags are readable.
//
// Both Rcpp and cereal have needs for the order of inclusion
// so that all template overloads are forward-declared before use.
// Rcpp manages this by saying to include RcppCommon.h first,
// then add any templates such as wrap or Exporter, then
// include Rcpp.h.
//
// It is assumed that Rcpp support is ALWAYS needed.
// Others (nClass interface, Eigen, cereal, TBB, and CppAD) are managed by flags.
//
// Note that aside from the sequencing needed for template purposes just stated,
// and for keeping simple the on-the-fly generated code,
// we attempt to make each header self-contained. It is hoped this practice
// will keep things a bit more sane and clear and avoid more complicated
// assumptions or schemes about order of includes.

//#ifndef NCOMPILER_OMNIBUS_FIRST_H_
// #define NCOMPILER_OMNIBUS_FIRST_H_

 #include <RcppCommon.h>

// BEGIN EIGEN
 #ifdef NCOMPILER_USES_EIGEN

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
    #ifdef NCOMPILER_HANDLE_EIGEN_ERRORS
      #define eigen_assert(X) {if(!eigen_did_assert && !(X)) { eigen_did_assert = true; throw std::runtime_error(#X); }}
    #endif

    #include <RcppEigenForward.h>
    #include <nCompiler/EigenTensor_extensions/EigenTensor_extensions_pre_Rcpp.h>
    #include <nCompiler/EigenTensor_Rcpp_extensions/EigenTensor_Rcpp_extensions_pre_Rcpp.h>

    #ifdef NCOMPILER_EIGEN_NDEBUG_DISABLED
      # define NDEBUG
    #endif


  #endif //NCOMPILER_EIGEN_ERRORS_ENABLED (can likely be moved up)
 #endif //NCOMPILER_USES_EIGEN
// END EIGEN_FIRST

#ifdef NCOMPILER_USES_CEREAL
#include <nCompiler/nClass_cereal/nClass_cereal_pre_Rcpp.h>
#endif

#ifdef NCOMPILER_USES_NCLASS_INTERFACE
#include <nCompiler/nClass_interface_Rcpp_extensions/nClass_interface_Rcpp_extensions_pre_Rcpp.h>
#include <nCompiler/nClass_interface/nClass_interface_pre_Rcpp.h>
#endif

// Main RCPP include
// #include <Rcpp.h>
 // We will want to remove using namespace statements.
// using namespace Rcpp;

 // I believe the next two are defensive, so that if for some reason
 // Rcpp is not in use but we code assuming it is with these macros appearing
 // they will just be replaced with nothing.
 // I think eventually we should be able to remove these because it should never be needed.
 // #ifndef BEGIN_RCPP
 //   #define BEGIN_RCPP
 // #endif

 // #ifndef END_RCPP
 //   #define END_RCPP
 // #endif

 #ifndef RESET_EIGEN_ERRORS // similar purpose: make any appearance of this macro harmless if not in use
   #define RESET_EIGEN_ERRORS
 #endif
// END Main Rcpp include

// Note that _INCLUDE_SERIALIZE_AND_DESERIALIZE_FUNCTIONS will become NCOMPILER_USES_CEREAL

#ifdef NCOMPILER_USES_TBB
#include "nCompiler_TBB.h"
#endif

// Main Rinternals include
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
// End Rinternals include


//


//#endif // NCOMPILER_OMNIBUS_FIRST_H_
