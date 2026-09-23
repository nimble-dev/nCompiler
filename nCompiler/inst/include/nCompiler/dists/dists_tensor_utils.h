// Shared Eigen::Tensor materialization helpers for nCompiler's dist
// wrappers (e.g. dmnorm_chol_tensor.h, dmvt_chol_tensor.h). Split out into
// its own header, rather than being duplicated in each *_tensor.h, because
// multiple such wrappers get included together (via dists_post_Rcpp.h) and
// would otherwise redefine the same templates in one translation unit.

#ifndef _NCOMPILER_DISTS_TENSOR_UTILS
#define _NCOMPILER_DISTS_TENSOR_UTILS

#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>

// True only when TensorExpr is exactly Eigen::Tensor<double, Rank>, i.e. an
// already-materialized, densely-packed tensor rather than a lazy expression
// (a block, a cast, an arithmetic chain, a TensorMap, etc).
template<typename TensorExpr, int Rank>
struct is_dense_double_tensor
  : std::is_same<TensorExpr, Eigen::Tensor<double, Rank>> {};

// Returns x itself (no copy) when it is already a concrete
// Eigen::Tensor<double, Rank>; otherwise assigns it into a fresh
// Eigen::Tensor<double, Rank>, which forces Eigen to evaluate the expression
// into a contiguous buffer we can call .data() on. The .cast<double>() is
// required here, not just a safety net: Eigen::Tensor's assignment/
// construction from another tensor expression requires matching Scalar
// types, so a non-double input (e.g. Eigen::Tensor<int, Rank>, or a bool
// expression from a comparison op) would otherwise fail to compile.
template<int Rank, typename TensorExpr>
decltype(auto) asDenseTensor(const TensorExpr &x) {
  if constexpr (is_dense_double_tensor<TensorExpr, Rank>::value) {
    return (x);
  } else {
    return Eigen::Tensor<double, Rank>(x.template cast<double>());
  }
}

// Applies nimble's/R's recycling rule for a "mean"-like argument that may
// be shorter than the target length n: if v has fewer than n elements, its
// elements are reused cyclically to fill out length n (matching the
// full_mean/full_mu construction in dists.cpp's C_dmnorm_chol,
// C_rmnorm_chol, C_dmvt_chol, and C_rmvt_chol). If v already has at least n
// elements, it's returned unchanged (extra elements beyond n are simply
// never read by the kernels that consume it, same as the original code).
//
// Unlike asDenseTensor, this always returns by value: whether recycling is
// needed is a runtime property (v's length vs. n), not something knowable
// from the type system, so there's no way to give the "no recycling
// needed" case a reference-typed fast path here without splitting the
// return type per branch.
inline Eigen::Tensor<double, 1> recycleToLength(const Eigen::Tensor<double, 1> &v, int n) {
  int m = static_cast<int>(v.dimension(0));
  if (m >= n) return v;
  Eigen::Tensor<double, 1> out(n);
  int j = 0;
  for (int i = 0; i < n; i++) {
    out(i) = v(j++);
    if (j == m) j = 0;
  }
  return out;
}

#endif // _NCOMPILER_DISTS_TENSOR_UTILS
