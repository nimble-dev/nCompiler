// Eigen::Tensor-argument wrapper around the rwish_chol random-generation
// kernel (see rwish_chol.h). Templated code like this can only ever be
// header-only -- the compiler must see the definition wherever it's
// instantiated -- so this lives in inst/include regardless of the
// package-DLL/on-the-fly-compilation split that motivates rwish_chol.h
// itself.

#ifndef _NCOMPILER_DISTS_RWISH_CHOL_TENSOR
#define _NCOMPILER_DISTS_RWISH_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "rwish_chol.h"

// Eigen::Tensor wrapper for rwish_chol.
//
// chol is accepted as an arbitrary Eigen::Tensor expression built up at the
// call site. Unlike the read-only chol arguments in the density-function
// wrappers, rwish_chol can use chol itself as scratch space when
// overwrite_inputs = 1 (see rwish_chol.h), so chol is always materialized
// into a private buffer here (never asDenseTensor's no-copy reference
// path) before being handed to the kernel with overwrite_inputs = 1 --
// matching how the original C_rwish_chol instead passed overwrite_inputs =
// 0 to force the kernel to make its own internal copy, since there chol
// aliased the user's own R object and could not be touched.
//
// The output p x p matrix Z is always a fresh buffer.
//
// No GetRNGstate()/PutRNGstate() bracketing here, matching
// rmnorm_chol_tensor.h/rmvt_chol_tensor.h: code that reaches this wrapper
// is invoked from R through Rcpp, which already brackets RNG state around
// the outer .Call boundary (RNGScope). The plain-.Call DLL path
// (dists.cpp's C_rwish_chol) isn't routed through Rcpp, so it keeps its
// own bracketing -- that's unchanged and still correct.
template<typename TensorExprChol>
Eigen::Tensor<double, 2> rwish_chol(int n, // ignored for now, to be used in the future.
const TensorExprChol &chol, double df,
                                     double scale_param) {
  Eigen::Tensor<double, 2> cholEval = chol.template cast<double>();

  int p = static_cast<int>(cholEval.dimension(0));

  Eigen::Tensor<double, 2> Z(p, p);

  rwish_chol(Z.data(), cholEval.data(), df, p, scale_param,
             /*overwrite_inputs=*/1);

  return Z;
}

#endif // _NCOMPILER_DISTS_RWISH_CHOL_TENSOR
