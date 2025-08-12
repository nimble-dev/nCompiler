#ifndef __TENSOR_CAT_OP_H_
#define __TENSOR_CAT_OP_H_

#include <unsupported/Eigen/CXX11/Tensor>
#include "tensorUtils.h"
#include "tensorFlex.h"

// template<typename T1, typename T2>
// auto concat_(const T1 &x1, const T2 &x2) {
//   typedef std::common_type_t<typename TypeLike<T1>::Scalar, typename TypeLike<T2>::Scalar> CommonType;
//   return flex_cast<CommonType>(Eigen::as_1D_tensor(x1)).concatenate(
//          flex_cast<CommonType>(Eigen::as_1D_tensor(x2)), 0);
// }

/**
 * Base template required to avoid redefining partial specializations
 */
template<typename... Args>
struct nC_impl;

/**
 * Partial specialization for empty case: nC() returns empty 1D tensor
 */
template<typename CAST_TYPE>
struct nC_impl<CAST_TYPE> {
  static auto run() {
    return Eigen::Tensor<CAST_TYPE, 1>(0);  // Empty 1D tensor
  }
};

/**
 * Partial specialization for singleton case: nC(x) returns x as 1D tensor
 */
template<typename CAST_TYPE, typename T1>
struct nC_impl<CAST_TYPE, T1> {
  //typedef Eigen::Tensor<CAST_TYPE, 1> ReturnType;
  static auto run(const T1 & t1) {
    return flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(t1));
  }
};

/**
 * Partial specialization implementing final concatenate operation recursion
 */
template<typename CAST_TYPE, typename T1, typename T2>
struct nC_impl<CAST_TYPE, T1, T2> {
  //typedef Eigen::Tensor<CAST_TYPE, 1> ReturnType;
  static auto run(const T1 & t1, const T2 & t2) {
    // Rcpp::Rcout<<"going through two-argument implementation"<<std::endl;
//    return flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(t1)).eval().concatenate(flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(t2)).eval(), 0).eval();
    return flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(t1)).concatenate(flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(t2)), 0).eval();
  }
};


/**
 * Partial specialization implementing concatenate operation recursion
 */
template<typename CAST_TYPE, typename T1, typename T2, typename... TT>
struct nC_impl<CAST_TYPE, T1, T2, TT...> {  
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
  typedef decltype(
    flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(m_t1)).concatenate(flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(m_t2)), 0).eval()
  ) IntermediateConcat;

  static auto run(const T1 & t1, const T2 & t2, const TT&... tt) {
    return nC_impl<CAST_TYPE, IntermediateConcat, TT...>::run(
      flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(t1)).concatenate(flex_cast<CAST_TYPE, true>(Eigen::as_1D_tensor(t2)), 0).eval(),
      tt...
    );
  }
};

  // typedef Eigen::Tensor<CAST_TYPE, 1> ReturnType;
  // typedef ReturnType IntermediateType;
  // static ReturnType run(const T1 & t1, const T2 & t2, const TT&... tt) {
  //  Rcpp::Rcout<<"going through multiple arguments implementation"<<std::endl;
  //   return nC_impl<CAST_TYPE, IntermediateType, TT...>::run(
  //     ReturnType(flex_cast<CAST_TYPE>(Eigen::as_1D_tensor(t1)).concatenate(flex_cast<CAST_TYPE>(Eigen::as_1D_tensor(t2)), 0)), 
  //     tt...
  //   );
  // }
  //};

/**
 * Convenience wrapper to generate tensor concatenations
 */
// Empty case: nC() - default to double
auto nC() -> Eigen::Tensor<double, 1> {
  return nC_impl<double>::run();
}

template<typename... Args>
using nC_common_type_t = std::common_type_t<typename TypeLike<Args>::Scalar...>;

// Single argument case: nC(x)
template<typename T1>
auto nC(const T1& t1) -> Eigen::Tensor<nC_common_type_t<T1>, 1> {
  typedef typename TypeLike<T1>::Scalar CommonType;
  return nC_impl<CommonType, T1>::run(t1);
}

// Multiple arguments case: nC(x, y, ...)
template<typename T1, typename T2, typename... Args>
auto nC(const T1& t1, const T2& t2, const Args&... args) -> Eigen::Tensor<nC_common_type_t<T1, T2, Args...>, 1> {
  // Rcpp::Rcout<<"going through multiple arguments cast"<<std::endl;
  return nC_impl<nC_common_type_t<T1, T2, Args...>, T1, T2, Args...>::run(t1, t2, args...);
}

#endif // __TENSOR_CAT_OP_H_
