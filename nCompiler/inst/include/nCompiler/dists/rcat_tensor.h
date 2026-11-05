// Eigen::Tensor-argument wrapper around the rcat random-generation kernel
// (see rcat.h). Templated code like this can only ever be header-only --
// the compiler must see the definition wherever it's instantiated -- so
// this lives in inst/include regardless of the package-DLL/on-the-fly-
// compilation split that motivates rcat.h itself.

#ifndef _NCOMPILER_DISTS_RCAT_TENSOR
#define _NCOMPILER_DISTS_RCAT_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "rcat.h"

// Eigen::Tensor wrapper for rcat.
//
// prob is read-only, so asDenseTensor reuses it directly (no copy) when
// it's already a concrete tensor. The result is a scalar category.
//
// No GetRNGstate()/PutRNGstate() bracketing here, matching the other
// r-function wrappers: code that reaches this wrapper is invoked from R
// through Rcpp, which already brackets RNG state around the outer .Call
// boundary (RNGScope). The plain-.Call DLL path (dists.cpp's C_rcat) isn't
// routed through Rcpp, so it keeps its own bracketing.
template<typename TensorExprProb>
double rcat(int n, // ignored for now, to be used in the future.
            const TensorExprProb &prob) {
  const auto &probEval = asDenseTensor<1>(prob);

  int K = static_cast<int>(probEval.dimension(0));

  return rcat(const_cast<double*>(probEval.data()), K);
}

#endif // _NCOMPILER_DISTS_RCAT_TENSOR
