//#ifndef NCLASS_INTERFACE_POST_RCPP_H_
//#define NCLASS_INTERFACE_POST_RCPP_H_

//#include "nClass_interface_pre_Rcpp.h"
// process_call_args includes one non-templated function, so to avoid
// duplicate symbol errors from multiple compilation units, it is included
// in the interfaces cppDef managed from code-generation from R.
//#include <nCompiler/nClass_interface/post_Rcpp/process_call_args.h>
#include <nCompiler/nClass_interface/post_Rcpp/loadedObjectHookC_impl.h>
#include <nCompiler/nClass_interface/post_Rcpp/generic_class_interface_Rcpp_steps.h>
//#include <nCompiler/nClass_interface_Rcpp_extensions/shared_ptr_as_wrap.h>

//#endif // NCLASS_INTERFACE_POST_RCPP_H_
