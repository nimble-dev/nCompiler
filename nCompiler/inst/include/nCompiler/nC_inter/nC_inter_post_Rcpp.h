//#ifndef NCLASS_INTERFACE_POST_RCPP_H_
//#define NCLASS_INTERFACE_POST_RCPP_H_

//#include "nC_inter_pre_Rcpp.h"
// process_call_args includes one non-templated function, so to avoid
// duplicate symbol errors from multiple compilation units, it is included
// in the interfaces cppDef managed from code-generation from R.
//#include <nCompiler/nC_inter/post_Rcpp/process_call_args.h>
#include <nCompiler/nC_inter/post_Rcpp/loadedObjectHookC_impl.h>
#include <nCompiler/nC_inter/post_Rcpp/generic_class_interface_Rcpp_steps.h>
//#include <nCompiler/nC_inter_Rcpp_ext/shared_ptr_as_wrap.h>

// I think the following will be moved to a nimble 2.0 package but is drafted here for now
#include <nCompiler/nC_inter/post_Rcpp/nCompiler_compileNimble_support.h>

//#endif // NCLASS_INTERFACE_POST_RCPP_H_
