#ifndef NCOMPILER_TENSOR_STRING_NO_TILING_H_
#define NCOMPILER_TENSOR_STRING_NO_TILING_H_

#include <unsupported/Eigen/CXX11/Tensor>
#include <string>

// Disable Eigen's tiled (block-based) tensor evaluation for std::string scalars.
//
// Eigen intends block access only for arithmetic scalars (TensorEvaluator<Tensor>
// sets BlockAccess = is_arithmetic<Scalar>), but some evaluators turn it on from
// RawAccess alone, regardless of scalar type: the lvalue TensorChippingOp
// evaluator and the TensorReshapingOp evaluators. Hence e.g. (from y[1, 1:3] <- z[2, 1:3])
//   y.chip<0>(0).slice(...) = z.chip<0>(1).slice(...);
// is evaluated in tiles when the scalar is std::string. Tiled evaluation
// materializes non-contiguous blocks in scratch memory from a raw allocation,
// with no constructors (or destructors) run, and then assigns std::string's into
// that memory. That is undefined behavior. It happens to work with libc++ (macOS),
// where zeroed memory is a valid empty std::string, but segfaults with libstdc++
// (Linux), where a std::string holds a pointer to its own buffer.
//
// This partial specialization replaces Eigen's primary IsTileable for DefaultDevice
// (the only device nCompiler uses). The formula is copied from Eigen's primary
// template (TensorForwardDeclarations.h), with the std::string condition added,
// so the result is identical to Eigen's for every other scalar type.
//
// It must be seen before any tensor assignment is instantiated, so it is
// included right after the Tensor module in ET_ext_pre_Rcpp.h.
//
// The copied formula is only verified against Eigen 3.4. If Eigen is upgraded,
// check IsTileable in TensorForwardDeclarations.h and update the version guard.

#if !(EIGEN_VERSION_AT_LEAST(3,4,0) && !EIGEN_VERSION_AT_LEAST(3,4,90))
#warning "nCompiler: tensorStringNoTiling.h has only been verified for Eigen 3.4.x. Check Eigen::internal::IsTileable and update this file."
#endif

namespace Eigen {
namespace internal {

template <typename Expression>
struct IsTileable<DefaultDevice, Expression> {
  typedef typename remove_const<typename traits<Expression>::Scalar>::type Scalar;
  // Same as Eigen's primary template, plus the std::string condition.
  static const bool BlockAccess =
      TensorEvaluator<Expression, DefaultDevice>::BlockAccess &&
      TensorEvaluator<Expression, DefaultDevice>::PreferBlockAccess &&
      !is_same<Scalar, std::string>::value;

  static const TiledEvaluation value =
      BlockAccess ? TiledEvaluation::On : TiledEvaluation::Off;
};

} // namespace internal
} // namespace Eigen

#endif // NCOMPILER_TENSOR_STRING_NO_TILING_H_
