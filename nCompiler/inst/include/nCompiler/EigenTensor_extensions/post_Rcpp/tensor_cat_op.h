#ifndef __TENSOR_CAT_OP_H_
#define __TENSOR_CAT_OP_H_

#include <unsupported/Eigen/CXX11/Tensor>
#include "tensorUtils.h"

template<typename T1, typename T2>
auto concat_(const T1 &x1, const T2 &x2) -> decltype(Eigen::as_1D_tensor(x1).concatenate(Eigen::as_1D_tensor(x2),0)) {
  return Eigen::as_1D_tensor(x1).concatenate(Eigen::as_1D_tensor(x2),0);
}

template<typename T1, typename T2>
auto nC(const T1 & t1, const T2 & t2) -> decltype(
  t1.concatenate(t2, 0)
) {
  return t1.concatenate(t2, 0);
}

template<typename T1, typename T2, typename... TT>
auto nC(const T1 & t1, const T2 & t2, const TT&... tt) -> decltype(
  nC(t1.concatenate(t2, 0), tt...)
) {
  return nC(t1.concatenate(t2, 0), tt...);
}

#endif // __TENSOR_CAT_OP_H_
