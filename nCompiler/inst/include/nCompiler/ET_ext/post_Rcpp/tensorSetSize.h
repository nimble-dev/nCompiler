#ifndef _NCOMPILER_TENSOR_SET_SIZE
#define _NCOMPILER_TENSOR_SET_SIZE

#include <unsupported/Eigen/CXX11/Tensor>
#include <algorithm>
#include <string>
#include <type_traits>
#include "recyclingRule.h"  // TypeLike
#include "tensorCreation.h" // tensor_creation_detail::dim_is_expr and get_dims
#include "tensorFlex.h"     // scalar_cast_
#include "tensorUtils.h"    // nDimTraits2_size

// setSize_(x, size, copy = true, fill = true [, value])
//
// Resize x, optionally preserving values and initializing new elements.
// - size: a scalar (only for nDim == 1) or a 1-D tensor/expression of sizes
//   (any numeric scalar type).
// - copy: preserve existing values. This is done in flattened (column-major)
//   order, regardless of the new shape.
// - fill: initialize elements not covered by copied values (all elements if
//   copy is false) with value.
// - value: fill value, which may be a scalar, a literal, or a 0-dimensional
//   tensor or expression (e.g. sum(y)). If omitted, Scalar{} is used:
//   0, 0, false or "". NA is never used.
// If fill is false, uninitialized elements are indeterminate for numeric
// types (and "" for std::string, which is always default-constructed).
// Returns size (by value, since it is often a temporary), like R's `length<-`
// returning its value, to allow chained calls.
template<typename Scalar, int nDim, typename SizeT, typename ValueT>
SizeT setSize_(Eigen::Tensor<Scalar, nDim> &x,
              const SizeT &size,
              bool copy,
              bool fill,
              const ValueT &value) {
  using namespace nCompiler::tensor_creation_detail;
  typedef typename Eigen::Tensor<Scalar, nDim>::Index Index;
  static_assert(nDim >= 1, "setSize_ requires an object with at least one dimension.");
  constexpr bool sizeIsExpr = dim_is_expr<SizeT>::value;
  static_assert(sizeIsExpr || nDim == 1,
                "setSize_: a single size can only be used for a 1-dimensional object.");
  if constexpr (sizeIsExpr) {
    if(nDimTraits2_size(size) != nDim)
      Rcpp::stop("setSize_: number of sizes provided (%i) does not match number of dimensions (%i).",
                 static_cast<int>(nDimTraits2_size(size)), nDim);
  }
  std::array<Index, nDim> newDims =
    get_dims<Index, nDim>(std::integral_constant<bool, sizeIsExpr>(), size);
  Index newSize = 1;
  for(int i = 0; i < nDim; ++i) {
    if(newDims[i] < 0) Rcpp::stop("setSize_: sizes must be non-negative.");
    newSize *= newDims[i];
  }
  const Index oldSize = x.size();

  Scalar fillValue;
  if constexpr (std::is_convertible_v<const ValueT&, Scalar>) {
    // scalar variable or literal, including a string literal for std::string
    fillValue = static_cast<Scalar>(value);
  } else {
    // 0-dimensional tensor or expression, e.g. sum(y)
    if constexpr (std::is_same_v<Scalar, std::string>) {
      static_assert(std::is_same_v<std::remove_const_t<typename TypeLike<ValueT>::Scalar>, std::string>,
                    "setSize_: the fill value for a character object must be character. No casting is done to or from character.");
    }
    fillValue = scalar_cast_<Scalar>::cast(value);
  }

  if(newSize == oldSize) {
    // Same total size: Eigen's resize only updates the dimensions (no
    // reallocation), so values are preserved in flattened column-major order.
    x.resize(newDims);
    if(!copy && fill) x.setConstant(fillValue);
    return size;
  }
  if(!copy) {
    // Eigen's resize reallocates and does not preserve contents.
    x.resize(newDims);
    if(fill) x.setConstant(fillValue);
    return size;
  }
  Eigen::Tensor<Scalar, nDim> ans(newDims);
  const Index nKeep = std::min(oldSize, newSize);
  std::move(x.data(), x.data() + nKeep, ans.data());
  if(fill) std::fill(ans.data() + nKeep, ans.data() + newSize, fillValue);
  x = std::move(ans);
  return size;
}

// Version without value: fill with Scalar{} (0, 0, false or "").
template<typename Scalar, int nDim, typename SizeT>
SizeT setSize_(Eigen::Tensor<Scalar, nDim> &x,
              const SizeT &size,
              bool copy = true,
              bool fill = true) {
  return setSize_(x, size, copy, fill, Scalar{});
}

#endif // _NCOMPILER_TENSOR_SET_SIZE
