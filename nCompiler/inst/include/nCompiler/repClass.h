#ifndef _NCOMPILER_REP_CLASS
#define _NCOMPILER_REP_CLASS

// let the default be true case
template <typename TensorIn, bool IsVector = (TensorIn::NumDimensions == 1)>
struct rep_impl {

  typedef typename TensorIn::Scalar ScalarType;
  typedef typename TensorIn::Index IndexType;

  static Eigen::Tensor<ScalarType, 1> repTimes(const TensorIn& x, unsigned int times) {
    std::array<IndexType, 1> bcast{{times}};
    return(x.broadcast(bcast));
  }

  static Eigen::Tensor<ScalarType, 1> repLen(const TensorIn& x, unsigned int length_out) {
    std::array<IndexType, 1> offset{{0}};
    std::array<IndexType, 1> extent{{length_out}};
    if (length_out < x.size()) {
      return(x.slice(offset, extent));
    } else {
      int times = ceil(static_cast<double>(length_out) / x.size());
      std::array<IndexType, 1> bcast{{times}};
      return(x.broadcast(bcast).slice(offset, extent));
    }
  }

};

template <typename TensorIn>
struct rep_impl<TensorIn, false> {

  typedef typename TensorIn::Scalar ScalarType;
  typedef typename TensorIn::Index IndexType;

  static Eigen::Tensor<ScalarType, 1> repTimes(const TensorIn& x, unsigned int times) {
    std::array<IndexType, 1> bcast{{times}};
    std::array<IndexType, 1> one_dim{{static_cast<IndexType>(x.size())}};
    return(x.reshape(one_dim).broadcast(bcast));
  }

  static Eigen::Tensor<ScalarType, 1> repLen(const TensorIn& x, unsigned int length_out) {
    std::array<IndexType, 1> offset{{0}};
    std::array<IndexType, 1> extent{{length_out}};
    std::array<IndexType, 1> one_dim{{static_cast<IndexType>(x.size())}};
    auto reshaped = x.reshape(one_dim);
    if (length_out < x.size()) {
      return(reshaped.slice(offset, extent));
    } else {
      int times = ceil(static_cast<double>(length_out) / x.size());
      std::array<IndexType, 1> bcast{{times}};
      return(reshaped.broadcast(bcast).slice(offset, extent));
    }
  }
  
};

template<typename TensorIn>
Eigen::Tensor<typename TensorIn::Scalar, 1> repTimes(const TensorIn& x, unsigned int times) {
  return rep_impl<TensorIn>::repTimes(x, times);
}

template<typename TensorIn>
Eigen::Tensor<typename TensorIn::Scalar, 1> repLen(const TensorIn& x, unsigned int length_out) {
  return rep_impl<TensorIn>::repLen(x, length_out);
}

// TODO: implement repEach

#endif
