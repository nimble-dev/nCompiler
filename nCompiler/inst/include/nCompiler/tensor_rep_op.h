#ifndef __TENSOR_REP_OP_H_
#define __TENSOR_REP_OP_H_

template<typename T>
struct repTypes {
  typedef typename T::Index Index;
  typedef Eigen::array<Index, 1> IndexArray;
  typedef Eigen::array<Index, 2> IndexArray2;
};

// The types T can never be scalars because then we'd
// pack it into a tensor that would be local in scope and
// go out of scope for the returned op.

template<typename T, typename Length_>
auto repLen(const T &xpr, const Length_ &length) ->
  decltype(nCompiler::IndexBySeqs<1>::go({{{0, 0, 1}}} , Eigen::as_1D_tensor(xpr, typename repTypes<T>::Index(1)).broadcast(typename repTypes<T>::IndexArray({{ typename repTypes<T>::Index(1)  }})))) {
  typedef typename repTypes<T>::Index Index;

  double dlength = static_cast<double>(length);
  if(dlength < 0) {
    Rcpp::stop("Invalid length in rep.");
  }
  Index xsize =  nDimTraits2_size(xpr);//Eigen::internal::array_prod( nDimTraits<typename std::remove_reference<T>::type >::getEvaluator(xpr).dimensions() );

  double dtimes = ceil(dlength / static_cast<double>(xsize));
  if(dtimes < 0) {
    Rcpp::stop("Invalid inputs in rep.");
  }
  Index utimes = static_cast<Index>(floor(dtimes));
  Index ulength = static_cast<Index>(floor(dlength));
  return nCompiler::IndexBySeqs<1>::go({{{0, 0, ulength-1}}} ,Eigen::as_1D_tensor(xpr, xsize).broadcast(typename repTypes<T>::IndexArray({{utimes}})));
  //return as_1D_tensor(xpr).broadcast(typename repTypes<T>::IndexArray({{2}}));
}

template<typename T, typename Scalar_>
auto repTimes(const T &xpr, const Scalar_ &times) ->
  decltype(Eigen::as_1D_tensor(xpr).broadcast(typename repTypes<T>::IndexArray({{ typename repTypes<T>::Index(1)  }}))) {
  double dtimes = static_cast<double>(times);
  if(dtimes < 0) {
    Rcpp::stop("Invalid times in rep.");
  }
  typedef typename repTypes<T>::Index Index;
  Index utimes = static_cast<Index>(floor(dtimes));
  return Eigen::as_1D_tensor(xpr).broadcast(typename repTypes<T>::IndexArray({{utimes}}));
  //return as_1D_tensor(xpr).broadcast(typename repTypes<T>::IndexArray({{2}}));
}

// repTimesLen does not make sense because length.out always moots (trumps) times

template<typename T, typename Scalar_>
auto repEach(const T &xpr, const Scalar_ &each) ->
  decltype(Eigen::as_1D_tensor(Eigen::as_2D_flat_tensor(xpr).broadcast(typename repTypes<T>::IndexArray2({{ typename repTypes<T>::Index(1),  typename repTypes<T>::Index(1)  }})))) {
  double deach = static_cast<double>(each);
  if(deach < 0) {
    Rcpp::stop("Invalid times in rep.");
  }
  typedef typename repTypes<T>::Index Index;
  Index ueach = static_cast<Index>(floor(deach));
  return Eigen::as_1D_tensor(Eigen::as_2D_flat_tensor(xpr).broadcast(typename repTypes<T>::IndexArray2({{ueach, typename repTypes<T>::Index(1) }})));
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
