//#ifndef __NCOMPILER_EIGEN_FXNS
//#define __NCOMPILER_EIGEN_FXNS

//#include "EigenTensor_Rcpp_extensions_forward_declarations.h"
//#include <nCompiler/EigenTensor_Rcpp_extensions/EigenTensor_Rcpp_extensions_post_Rcpp.h>

// These should maybe be in a post_Rcpp directory.
// However, any of them could be pre_Rcpp. They could be either.
#include "StridedTensorMap.h" // This is pre_Rcpp
#include "StridedTensorMapInfo.h"
#include "typedefs.h"
#include "setWhich.h"
//#include "repClass.h"
//#include "seqClass.h"
#include "recyclingRule.h"
#include "tensorCreation.h"
#include "tensorFlex.h"
#include "tensorOperations.h"
#include "tensorIndexingOps.h"
#include "tensorUtils.h"
#include "tensor_seq_op.h"
#include "tensor_rep_op.h"
#include "tensor_cat_op.h"

//#endif
