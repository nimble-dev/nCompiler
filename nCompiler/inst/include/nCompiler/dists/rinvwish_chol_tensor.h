// Eigen::Tensor-argument wrapper around the rinvwish_chol random-generation
// kernel (see rinvwish_chol.h). Templated code like this can only ever be
// header-only -- the compiler must see the definition wherever it's
// instantiated -- so this lives in inst/include regardless of the
// package-DLL/on-the-fly-compilation split that motivates rinvwish_chol.h
// itself.

#ifndef _NCOMPILER_DISTS_RINVWISH_CHOL_TENSOR
#define _NCOMPILER_DISTS_RINVWISH_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "rinvwish_chol.h"

// Eigen::Tensor wrapper for rinvwish_chol.
//
// As in rwish_chol_tensor.h, chol is always materialized into a private
// buffer, since the kernel uses it as scratch space with overwrite_inputs =
// 1. The output p x p matrix Z is always a fresh buffer.
//
// No GetRNGstate()/PutRNGstate() bracketing here, matching
// rwish_chol_tensor.h: code that reaches this wrapper is invoked from R
// through Rcpp, which already brackets RNG state around the outer .Call
// boundary (RNGScope).
template<typename TensorExprChol>
Eigen::Tensor<double, 2> rinvwish_chol(int n, // ignored for now, to be used in the future.
                                       const TensorExprChol &chol, double df,
                                       double scale_param) {
  Eigen::Tensor<double, 2> cholEval = chol.template cast<double>();

  int p = static_cast<int>(cholEval.dimension(0));

  Eigen::Tensor<double, 2> Z(p, p);

  rinvwish_chol(Z.data(), cholEval.data(), df, p, scale_param,
                /*overwrite_inputs=*/1);

  return Z;
}

#endif // _NCOMPILER_DISTS_RINVWISH_CHOL_TENSOR
