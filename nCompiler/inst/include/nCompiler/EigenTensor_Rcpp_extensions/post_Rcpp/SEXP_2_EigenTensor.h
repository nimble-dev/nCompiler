#ifndef SEXP_2_TENSOR_H_
#define SEXP_2_TENSOR_H_

#include <unsupported/Eigen/CXX11/Tensor>

// Copy from SEXP to Eigen Tensor when scalar types are the same
template<class fromT, class toT, int nDim>
Eigen::Tensor<toT, nDim>
castedTensorCopy(const fromT * const from,
                 const Eigen::array<typename Eigen::Tensor<fromT, nDim>::Index, nDim> &indexArray,
                 std::true_type same_types) {
  Eigen::TensorMap< Eigen::Tensor<const fromT, nDim> > xMap(from, indexArray);
  Eigen::Tensor<toT, nDim> to = xMap;
  return to;
}

template<class fromT, class toT, int nDim>
Eigen::Tensor<toT, nDim>
castedTensorCopy(const fromT * const from,
                 const Eigen::array<typename Eigen::Tensor<fromT, nDim>::Index, nDim> &indexArray,
                 std::false_type different_types) {
  Eigen::TensorMap< Eigen::Tensor<const fromT, nDim> > xMap(from, indexArray);
  Eigen::Tensor<toT, nDim> to = xMap.template cast<toT>();
  return to;
}

template<typename Scalar, int nInd> /* Scalar is the target scalar.  The input scalar is determined by TYPEOF(SINPUT). */
struct SEXP_2_EigenTensor {
  template<typename EigenTensorType, typename IndexArray>
  static EigenTensorType copy(SEXP &Sinput,
                              const IndexArray &indexArray) {
    EigenTensorType xCopy;
    typedef typename std::is_same<Scalar, int>::type i_match_type;
    typedef typename std::is_same<Scalar, double>::type d_match_type;
    switch( TYPEOF(Sinput) ) {
    case REALSXP:
      //      std::cout<<"copying from REAL\n";
      xCopy =
        castedTensorCopy<double, Scalar, nInd>(REAL(Sinput),
                                               indexArray,
                                               d_match_type()
                                               );
      break;
    case INTSXP:
      //      std::cout<<"copying from INTEGER\n";
      xCopy =
        castedTensorCopy<int, Scalar, nInd>(INTEGER(Sinput),
                                            indexArray,
                                            i_match_type()
                                            );
      break;
    case LGLSXP:
      //      std::cout<<"copying from LOGICAL\n";
      // R represents logicals as int
      xCopy =
        castedTensorCopy<int, Scalar, nInd>(INTEGER(Sinput),
                                            indexArray,
                                            i_match_type()
                                            );
      break;
    default:
      std::cout<<"Bad type\n"<<std::endl;
    }
    return xCopy; // compiler should use copy elision
  }
};

#endif // SEXP_2_TENSOR_H_
