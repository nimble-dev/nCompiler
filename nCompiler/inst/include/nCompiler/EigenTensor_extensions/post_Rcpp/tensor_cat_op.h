#ifndef __TENSOR_CAT_OP_H_
#define __TENSOR_CAT_OP_H_

#include <unsupported/Eigen/CXX11/Tensor>
#include "tensorUtils.h"

template<typename T1, typename T2>
auto concat_(const T1 &x1, const T2 &x2) -> decltype(Eigen::as_1D_tensor(x1).concatenate(Eigen::as_1D_tensor(x2),0)) {
  return Eigen::as_1D_tensor(x1).concatenate(Eigen::as_1D_tensor(x2),0);
}

/**
 * Base template required to avoid redefining partial specializations
 */
template<typename... Args>
struct nC_impl;

/**
 * Partial specialization implementing final concatenate operation recursion
 */
template<typename T1, typename T2>
struct nC_impl<T1, T2> {
  static auto run(const T1 & t1, const T2 & t2) -> decltype(
    t1.concatenate(t2, 0)
  ) {
    return t1.concatenate(t2, 0);
  }
};


/**
 * Partial specialization implementing concatenate operation recursion
 */
template<typename T1, typename T2, typename... TT>
struct nC_impl<T1, T2, TT...> {

  /**
   * only used to make decltype well defined.  struct nC_impl is not intended 
   * to be instantiated
   */
  const T1 & m_t1;
  const T2 & m_t2;

  /**
   * Determine return type here to simplify recursive variadic template coding 
   * patterns with auto return types
   */
  typedef decltype(m_t1.concatenate(m_t2,0)) IntermediateConcat;
  
  static auto run(const T1 & t1, const T2 & t2, const TT&... tt) -> decltype(
    nC_impl<IntermediateConcat, TT...>::run(t1.concatenate(t2, 0), tt...)
  ) {
    return nC_impl<IntermediateConcat, TT...>::run(
      t1.concatenate(t2, 0), tt...
    );
  }
};

/**
 * Convenience wrapper to generate tensor concatenations
 */
template<typename... Args>
auto nC(const Args&... args) -> decltype(nC_impl<Args...>::run(args...)) {
  return nC_impl<Args...>::run(args...);
}

#endif // __TENSOR_CAT_OP_H_
