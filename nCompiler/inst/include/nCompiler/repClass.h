#ifndef _NCOMPILER_REP_CLASS
#define _NCOMPILER_REP_CLASS

template <typename DerivedOut>
struct rep_impl {

  typedef typename DerivedOut::Scalar ScalarType;
  typedef typename DerivedOut::Index IndexType;

  static Eigen::Tensor<ScalarType, 1> maybe_reshape(DerivedOut x) {
    Eigen::Tensor<ScalarType, 1> reshaped;
    if (x.NumDimensions != 1) {
      Eigen::array<IndexType, 1> one_dim{{static_cast<IndexType>(x.size())}};
      reshaped = x.reshape(one_dim);
    } else {
      reshaped = x;
    }
    return(reshaped);
  }

  static Eigen::Tensor<ScalarType, 1> repTimes(DerivedOut x, int times) {
    Eigen::array<IndexType, 1> bcast{{times}};
    // auto here hopefully avoids materializing x too early?
    auto reshaped = maybe_reshape(x);
    return(reshaped.broadcast(bcast));
  }

  static Eigen::Tensor<ScalarType, 1> repTimesEval(DerivedOut x, int times) {
    Eigen::array<IndexType, 1> bcast{{times}};
    auto reshaped = maybe_reshape(x);
    return(reshaped.eval().broadcast(bcast));
  }

  static Eigen::Tensor<ScalarType, 1> repLen(DerivedOut x, int length_out) {
    Eigen::array<IndexType, 1> offset = {0};
    Eigen::array<IndexType, 1> extent = {length_out};
    auto reshaped = maybe_reshape(x);
    if (length_out < x.size()) {
      return(reshaped.slice(offset, extent));
    } else {
      int times = ceil(static_cast<double>(length_out) / x.size());
      Eigen::array<IndexType, 1> bcast{{times}};
      return(reshaped.broadcast(bcast).slice(offset, extent));
    }
  }

  static Eigen::Tensor<ScalarType, 1> repLenEval(DerivedOut x, int length_out) {
    Eigen::array<IndexType, 1> offset = {0};
    Eigen::array<IndexType, 1> extent = {length_out};
    auto reshaped = maybe_reshape(x);
    if (length_out < x.size()) {
      return(reshaped.slice(offset, extent));
    } else {
      int times = ceil(static_cast<double>(length_out) / x.size());
      Eigen::array<IndexType, 1> bcast{{times}};
      return(reshaped.eval().broadcast(bcast).slice(offset, extent));
    }
  }
};

// TODO: support arbitrary NumDimensions
// TODO: implement repEach
// repTimes
#define repTimes1d rep_impl<Eigen1d>::repTimes
#define repTimes2d rep_impl<Eigen2d>::repTimes
#define repTimes3d rep_impl<Eigen3d>::repTimes
#define repTimes1i rep_impl<Eigen1i>::repTimes
#define repTimes2i rep_impl<Eigen2i>::repTimes
#define repTimes3i rep_impl<Eigen3i>::repTimes
#define repTimes1b rep_impl<Eigen1b>::repTimes
#define repTimes2b rep_impl<Eigen2b>::repTimes
#define repTimes3b rep_impl<Eigen3b>::repTimes

// repTimesEval
#define repTimesEval1d rep_impl<Eigen1d>::repTimesEval
#define repTimesEval2d rep_impl<Eigen2d>::repTimesEval
#define repTimesEval3d rep_impl<Eigen3d>::repTimesEval
#define repTimesEval1i rep_impl<Eigen1i>::repTimesEval
#define repTimesEval2i rep_impl<Eigen2i>::repTimesEval
#define repTimesEval3i rep_impl<Eigen3i>::repTimesEval
#define repTimesEval1b rep_impl<Eigen1b>::repTimesEval
#define repTimesEval2b rep_impl<Eigen2b>::repTimesEval
#define repTimesEval3b rep_impl<Eigen3b>::repTimesEval

// repLen
#define repLen1d rep_impl<Eigen1d>::repLen
#define repLen2d rep_impl<Eigen2d>::repLen
#define repLen3d rep_impl<Eigen3d>::repLen
#define repLen1i rep_impl<Eigen1i>::repLen
#define repLen2i rep_impl<Eigen2i>::repLen
#define repLen3i rep_impl<Eigen3i>::repLen
#define repLen1b rep_impl<Eigen1b>::repLen
#define repLen2b rep_impl<Eigen2b>::repLen
#define repLen3b rep_impl<Eigen3b>::repLen

// repLenEval
#define repLenEval1d rep_impl<Eigen1d>::repLenEval
#define repLenEval2d rep_impl<Eigen2d>::repLenEval
#define repLenEval3d rep_impl<Eigen3d>::repLenEval
#define repLenEval1i rep_impl<Eigen1i>::repLenEval
#define repLenEval2i rep_impl<Eigen2i>::repLenEval
#define repLenEval3i rep_impl<Eigen3i>::repLenEval
#define repLenEval1b rep_impl<Eigen1b>::repLenEval
#define repLenEval2b rep_impl<Eigen2b>::repLenEval
#define repLenEval3b rep_impl<Eigen3b>::repLenEval

#endif
