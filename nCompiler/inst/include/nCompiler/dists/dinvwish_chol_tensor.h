// Eigen::Tensor-argument wrapper around the dinvwish_chol density kernel
// (see dinvwish_chol.h). Templated code like this can only ever be header-
// only -- the compiler must see the definition wherever it's instantiated
// -- so this lives in inst/include regardless of the package-DLL/
// on-the-fly-compilation split that motivates dinvwish_chol.h itself.

#ifndef _NCOMPILER_DISTS_DINVWISH_CHOL_TENSOR
#define _NCOMPILER_DISTS_DINVWISH_CHOL_TENSOR

#include <unsupported/Eigen/CXX11/Tensor>
#include "dists_tensor_utils.h"
#include "dinvwish_chol.h"

// Eigen::Tensor wrapper for dinvwish_chol.
//
// As in dwish_chol_tensor.h, x is a p x p matrix. x and chol are both
// always copied into private buffers here: with overwrite_inputs = 1,
// dinvwish_chol factors its working copy of x in place, and (with
// scale_param) also uses chol as scratch space, and we must not mutate the
// caller's tensors as a side effect.
template<typename TensorExprX, typename TensorExprChol>
double dinvwish_chol(const TensorExprX &x, const TensorExprChol &chol,
                     double df, double scale_param, int give_log) {
  Eigen::Tensor<double, 2> cholEval = chol.template cast<double>();
  Eigen::Tensor<double, 2> xEval = x.template cast<double>();

  int p = static_cast<int>(xEval.dimension(0));

  return dinvwish_chol(xEval.data(), cholEval.data(), df, p,
                       scale_param, give_log, /*overwrite_inputs=*/1);
}

#endif // _NCOMPILER_DISTS_DINVWISH_CHOL_TENSOR
