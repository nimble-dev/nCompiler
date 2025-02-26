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

//#ifndef NCOMPILER_OMNIBUS_FIRST_CPP_
// #define NCOMPILER_OMNIBUS_FIRST_CPP_

#include "nCompiler_omnibus_pre_Rcpp.h" // should always be redundant, but it is here to be clear

// We shall see if these two "first" files (_h and _cpp) are really needed or can be consolidated.

// Main RCPP include
#include <Rcpp.h>
// We will want to remove using namespace statements.
#ifdef NCOMPILER_USES_EIGEN
#include <RcppEigenWrap.h>
#endif
using namespace Rcpp;

 // I believe the next two are defensive, so that if for some reason
 // Rcpp is not in use but we code assuming it is with these macros appearing
 // they will just be replaced with nothing.
 // I think eventually we should be able to remove these because it should never be needed.
 #ifndef BEGIN_RCPP
   #define BEGIN_RCPP
 #endif

 #ifndef END_RCPP
   #define END_RCPP
 #endif

#ifdef NCOMPILER_USES_EIGEN
#include <nCompiler/EigenTensor_extensions/EigenTensor_extensions_post_Rcpp.h>
#include <nCompiler/EigenTensor_Rcpp_extensions/EigenTensor_Rcpp_extensions_post_Rcpp.h>
#endif


#ifdef NCOMPILER_USES_TBB
#include <nCompiler/parallel/nCompiler_TBB.h>
#endif

#include <Rmath.h>
#include <math.h>
#include <iostream>

#include "omnibus_fxns.h" // What everything should see or need.

#ifdef NCOMPILER_USES_CEREAL
#include <nCompiler/nClass_cereal/nClass_cereal_post_Rcpp.h>
#endif

#ifdef NCOMPILER_USES_NCLASS_INTERFACE
#include <nCompiler/nClass_interface/nClass_interface_post_Rcpp.h>
#include <nCompiler/nClass_interface_Rcpp_extensions/nClass_interface_Rcpp_extensions_post_Rcpp.h>
#endif

#ifdef NCOMPILER_USES_NLIST
#include <nCompiler/nList/nList_post_Rcpp.h>
#endif

// Main Rinternals include
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
// End Rinternals include


//


//#endif // NCOMPILER_OMNIBUS_FIRST_H_
