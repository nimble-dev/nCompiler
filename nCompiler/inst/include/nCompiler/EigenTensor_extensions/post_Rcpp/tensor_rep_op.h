#ifndef __TENSOR_REP_OP_H_
#define __TENSOR_REP_OP_H_

#include <unsupported/Eigen/CXX11/Tensor>
#include "tensorIndexingOps.h"
#include "tensorUtils.h"
#include "recyclingRule.h"

template<typename T>
struct repTypes {
  typedef typename T::Index Index;
  typedef Eigen::array<Index, 1> IndexArray;
  typedef Eigen::array<Index, 2> IndexArray2;
};

template<typename RepType, typename LenType>
struct RepLenImpl {

  /**
   * If LenType is a Tensor or TensorExpression, unwrap the underlying Scalar 
   * type, otherwise (if LenType is a native scalar) effectively yield
   * "typedef LenType LenScalar;"
   * 
   * TypeLike uses SFINAE under the hood to extract LenType::Scalar if the 
   * member type exists and not cause compilation errors otherwise (i.e., if 
   * LenType = double, etc.)
   */
  typedef typename TypeLike<LenType>::Scalar LenScalar;

  /**
   * only used to make decltype well defined.  struct RepLenImpl is not 
   * intended to be instantiated
   */
  const RepType & dummy_xpr;

  /**
   * Determine return type here to simplify SFINAE coding patterns with auto 
   * return types,
   */
  typedef decltype(
    nCompiler::IndexBySeqs<1>::go(
      {{{0, 0, 1}}} ,
      Eigen::as_1D_tensor(
        dummy_xpr, 
        typename repTypes<RepType>::Index(1)
      ).broadcast(
        typename repTypes<RepType>::IndexArray(
          {{ typename repTypes<RepType>::Index(1)  }}
        )
      )
    )
  ) ReturnType;

  static ReturnType run_impl(const RepType & xpr, const LenScalar & length) {
    typedef typename repTypes<RepType>::Index Index;
    double dlength = static_cast<double>(length);
    if(dlength < 0) {
      Rcpp::stop("Invalid length in rep.");
    }
    Index xsize =  nDimTraits2_size(xpr);
    double dtimes = ceil(dlength / static_cast<double>(xsize));
    if(dtimes < 0) {
      Rcpp::stop("Invalid inputs in rep.");
    }
    Index utimes = static_cast<Index>(floor(dtimes));
    Index ulength = static_cast<Index>(floor(dlength));
    return nCompiler::IndexBySeqs<1>::go(
      {{{0, 0, ulength-1}}} ,
      Eigen::as_1D_tensor(xpr, xsize).broadcast(
        typename repTypes<RepType>::IndexArray({{utimes}})
      )
    );
  }

  /**
   * Partial specialization for when LenType is a scalar.  This is the case 
   * run_impl supports, so this function works as a pass-through.
   */
  template<
    typename T = LenType,
    typename std::enable_if<TypeLike<T>::true_scalar, bool>::type = true
  >
  static ReturnType run(const RepType & xpr, const LenType & length) {
    return run_impl(xpr, length);
  }

  /**
   * Partial specialization for when LenType is implied to be a tensor or 
   * tensor expression.  
   * 
   * To match R's implementation, only uses the first element of argument length
   */
  template<
    typename T = LenType,
    typename std::enable_if<!TypeLike<T>::true_scalar, bool>::type = true
  >
  static ReturnType run(const RepType & xpr, const LenType & each) {
    const Eigen::TensorRef<LenType> len_ref(each);
    return run_impl(xpr, len_ref.coeff(0));
  }

};

// The types T can never be scalars because then we'd
// pack it into a tensor that would be local in scope and
// go out of scope for the returned op.

template<typename T, typename Length_>
auto repLen(const T &xpr, const Length_ &length) ->
  decltype(RepLenImpl<T, Length_>::run(xpr, length)) {
    return RepLenImpl<T, Length_>::run(xpr, length);
  }

template<typename RepType, typename TimesType>
struct RepTimesImpl {

  /**
   * If TimesType is a Tensor or TensorExpression, unwrap the underlying Scalar 
   * type, otherwise (if TimesType is a native scalar) effectively yield
   * "typedef TimesType TimesScalar;"
   * 
   * TypeLike uses SFINAE under the hood to extract TimesType::Scalar if the 
   * member type exists and not cause compilation errors otherwise (i.e., if 
   * TimesType = double, etc.)
   */
  typedef typename TypeLike<TimesType>::Scalar TimesScalar;

  /**
   * only used to make decltype well defined.  struct RepTimesImpl is not 
   * intended to be instantiated
   */
  const RepType & dummy_xpr;

  /**
   * Determine return type here to simplify SFINAE coding patterns with auto 
   * return types,
   */
  typedef decltype(
    Eigen::as_1D_tensor(dummy_xpr).broadcast(
      typename repTypes<RepType>::IndexArray(
        {{ typename repTypes<RepType>::Index(1)  }}
      )
    )
  ) ReturnType;

  static ReturnType run_impl(const RepType & xpr, const TimesScalar & times) {
    double dtimes = static_cast<double>(times);
    if(dtimes < 0) {
      Rcpp::stop("Invalid times in rep.");
    }
    typedef typename repTypes<RepType>::Index Index;
    Index utimes = static_cast<Index>(floor(dtimes));
    return Eigen::as_1D_tensor(xpr).broadcast(
      typename repTypes<RepType>::IndexArray({{utimes}})
    );
  }

  /**
   * Partial specialization for when TimesType is a scalar.  This is the case 
   * run_impl supports, so this function works as a pass-through.
   */
  template<
    typename T = TimesType,
    typename std::enable_if<TypeLike<T>::true_scalar, bool>::type = true
  >
  static ReturnType run(const RepType & xpr, const TimesType & times) {
    return run_impl(xpr, times);
  }

  /**
   * Partial specialization for when TimesType is implied to be a tensor or 
   * tensor expression.  
   * 
   * To match test cases, only uses the first element of argument length
   */
  template<
    typename T = TimesType,
    typename std::enable_if<!TypeLike<T>::true_scalar, bool>::type = true
  >
  static ReturnType run(const RepType & xpr, const TimesType & each) {
    const Eigen::TensorRef<TimesType> times_ref(each);
    return run_impl(xpr, times_ref.coeff(0));
  }

};

template<typename T, typename Scalar_>
auto repTimes(const T &xpr, const Scalar_ &times) ->
  decltype(RepTimesImpl<T, Scalar_>::run(xpr, times)) {
    return RepTimesImpl<T, Scalar_>::run(xpr, times);
}

// repTimesLen does not make sense because length.out always moots (trumps) times

template<typename RepType, typename EachType>
struct RepEachImpl {

  /**
   * If EachType is a Tensor or TensorExpression, unwrap the underlying Scalar 
   * type, otherwise (if EachType is a native scalar) effectively yield
   * "typedef EachType EachScalar;"
   * 
   * TypeLike uses SFINAE under the hood to extract EachType::Scalar if the 
   * member type exists and not cause compilation errors otherwise (i.e., if 
   * EachType = double, etc.)
   */
  typedef typename TypeLike<EachType>::Scalar EachScalar;

  /**
   * only used to make decltype well defined.  struct RepEachImpl is not 
   * intended to be instantiated
   */
  const RepType & dummy_xpr;

  /**
   * Determine return type here to simplify SFINAE coding patterns with auto 
   * return types,
   */
  typedef decltype(
    Eigen::as_1D_tensor(
      Eigen::as_2D_flat_tensor(dummy_xpr).broadcast(
        typename repTypes<RepType>::IndexArray2(
          {{ typename repTypes<RepType>::Index(1),  
             typename repTypes<RepType>::Index(1)  }}
        )
      )
    )
  ) ReturnType;
   
  static ReturnType run_impl(const RepType & xpr, const EachScalar & each) {
    double deach = static_cast<double>(each);
    if(deach < 0) {
      Rcpp::stop("Invalid times in rep.");
    }
    typedef typename repTypes<RepType>::Index Index;
    Index ueach = static_cast<Index>(floor(deach));
    return Eigen::as_1D_tensor(
      Eigen::as_2D_flat_tensor(xpr).broadcast(
        typename repTypes<RepType>::IndexArray2(
          {{ueach, typename repTypes<RepType>::Index(1) }}
        )
      )
    );
  }

  /**
   * Partial specialization for when EachType is a scalar.  This is the case 
   * run_impl supports, so this function works as a pass-through.
   */
  template<
    typename T = EachType,
    typename std::enable_if<TypeLike<T>::true_scalar, bool>::type = true
  >
  static ReturnType run(const RepType & xpr, const EachType & each) {
    return run_impl(xpr, each);
  }

  /**
   * Partial specialization for when EachType is implied to be a tensor or 
   * tensor expression.  
   * 
   * To match R's implementation, only uses the first element of argument "each"
   */
  template<
    typename T = EachType,
    typename std::enable_if<!TypeLike<T>::true_scalar, bool>::type = true
  >
  static ReturnType run(const RepType & xpr, const EachType & each) {
    const Eigen::TensorRef<EachType> each_ref(each);
    return run_impl(xpr, each_ref.coeff(0));
  }

};

template<typename T, typename Scalar_>
auto repEach(const T &xpr, const Scalar_ &each) ->
  decltype(RepEachImpl<T, Scalar_>::run(xpr, each)) {
    return RepEachImpl<T, Scalar_>::run(xpr, each);
  }

template<typename T, typename Scalar_, typename Each_>
auto repTimesEach(const T &xpr, const Scalar_ &times, const Each_ &each) ->
  decltype(repTimes(repEach(xpr, each), times)) {
  return repTimes(repEach(xpr, each), times);
}

template<typename T, typename Length_, typename Scalar_>
auto repLenEach(const T &xpr, const Length_ &length, const Scalar_ &each) ->
  decltype( repLen( repEach(xpr, each), length ) ) {
  return repLen( repEach(xpr, each), length );
}

#endif // __TENSOR_REP_OP_H_
