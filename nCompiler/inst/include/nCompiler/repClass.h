#ifndef _NCOMPILER_REP_CLASS
#define _NCOMPILER_REP_CLASS

// let the default case be true
template <typename TensorIn, bool IsVector = (TensorIn::NumDimensions == 1)>
struct rep_impl {

  typedef typename TensorIn::Scalar ScalarType;
  typedef typename TensorIn::Index IndexType;

  static Eigen::Tensor<ScalarType, 1> repTimes(const TensorIn& x, unsigned int times, bool eval = false) {
    std::array<IndexType, 1> bcast{{times}};
    if (eval) {
      return(x.eval().broadcast(bcast));
    } else {
      return(x.broadcast(bcast));
    }
  }

  static Eigen::Tensor<ScalarType, 1> repLen(const TensorIn& x, unsigned int length_out, bool eval = false) {
    std::array<IndexType, 1> offset{{0}};
    std::array<IndexType, 1> extent{{length_out}};
    // user TensorRef so we can get x.size() without materializing x
    Eigen::TensorRef<TensorIn> ref = x;
    if (length_out < ref.size()) {
      if (eval) {
	return(x.eval().slice(offset, extent));
      } else {
	return(x.slice(offset, extent));
      }
    } else {
      int times = ceil(static_cast<double>(length_out) / ref.size());
      std::array<IndexType, 1> bcast{{times}};
      if (eval) {
	return(x.eval().broadcast(bcast).slice(offset, extent));
      } else {
	return(x.broadcast(bcast).slice(offset, extent));
      }
    }
  }

};

template <typename TensorIn>
struct rep_impl<TensorIn, false> {

  typedef typename TensorIn::Scalar ScalarType;
  typedef typename TensorIn::Index IndexType;

  static Eigen::Tensor<ScalarType, 1> repTimes(const TensorIn& x, unsigned int times, bool eval = false) {
    Eigen::TensorRef<TensorIn> ref = x;
    std::array<IndexType, 1> bcast{{times}};
    std::array<IndexType, 1> one_dim{{static_cast<IndexType>(ref.size())}};
    if (eval) {
      return(x.eval().reshape(one_dim).broadcast(bcast));
    } else {
      return(x.reshape(one_dim).broadcast(bcast));
    }
  }

  static Eigen::Tensor<ScalarType, 1> repLen(const TensorIn& x, unsigned int length_out, bool eval = false) {
    Eigen::TensorRef<TensorIn> ref = x;
    std::array<IndexType, 1> offset{{0}};
    std::array<IndexType, 1> extent{{length_out}};
    std::array<IndexType, 1> one_dim{{static_cast<IndexType>(ref.size())}};
    if (length_out < ref.size()) {
      if (eval) {
	return(x.eval().reshape(one_dim).slice(offset, extent));
      } else {
	return(x.reshape(one_dim).slice(offset, extent));
      }
    } else {
      int times = ceil(static_cast<double>(length_out) / ref.size());
      std::array<IndexType, 1> bcast{{times}};
      if (eval) {
	return(x.eval().reshape(one_dim).broadcast(bcast).slice(offset, extent));
      } else {
	return(x.reshape(one_dim).broadcast(bcast).slice(offset, extent));
      }      
    }
  }

};

template<typename TensorIn>
Eigen::Tensor<typename TensorIn::Scalar, 1> repTimes(const TensorIn& x, unsigned int times, bool eval = false) {
  return rep_impl<TensorIn>::repTimes(x, times, eval);
}

template<typename TensorIn>
Eigen::Tensor<typename TensorIn::Scalar, 1> repLen(const TensorIn& x, unsigned int length_out, bool eval = false) {
  return rep_impl<TensorIn>::repLen(x, length_out, eval);
}

// TODO: implement repEach

#endif
