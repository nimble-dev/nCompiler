//#ifndef __NCOMPILER_EIGEN_FXNS
//#define __NCOMPILER_EIGEN_FXNS

//#include "EigenTensor_Rcpp_extensions_forward_declarations.h"
//#include <nCompiler/EigenTensor_Rcpp_extensions/EigenTensor_Rcpp_extensions_post_Rcpp.h>

// These are going in a post_Rcpp directory.
// However, any of them could be pre_Rcpp. They could be either.
#include <nCompiler/EigenTensor_extensions/post_Rcpp/typedefs.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/setWhich.h>
//#include "repClass.h"
//#include "seqClass.h"
#include <nCompiler/EigenTensor_extensions/post_Rcpp/recyclingRule.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensorCreation.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensorFlex.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensorOperations.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensorIndexingOps.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensorUtils.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensor_seq_op.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensor_rep_op.h>
#include <nCompiler/EigenTensor_extensions/post_Rcpp/tensor_cat_op.h>

//#endif
