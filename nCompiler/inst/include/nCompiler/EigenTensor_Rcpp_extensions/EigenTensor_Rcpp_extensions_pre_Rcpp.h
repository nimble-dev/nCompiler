//#ifndef NCOMPILER_TENSOR_RCPP_FORWARD_DECLARATIONS_H_
//#define NCOMPILER_TENSOR_RCPP_FORWARD_DECLARATIONS_H_

// If Rcpp.h has been included prior to this, we may have problems.
// It's unfortunate but necessary to have this ordering requirement.

#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS

//#include <RcppCommon.h>
#include "EigenTensor_Rcpp_extensions_forward_declarations.h"
//#include <nCompiler/EigenTensor_extensions/EigenTensor_extensions_pre_Rcpp.h>
#include "ETaccessor.h"

//#endif // NCOMPILER_TENSOR_RCPP_FORWARD_DECLARATIONS_H_
