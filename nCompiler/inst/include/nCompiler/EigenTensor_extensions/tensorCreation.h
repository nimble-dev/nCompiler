#ifndef _NCOMPILER_TENSOR_CREATION
#define _NCOMPILER_TENSOR_CREATION

#include <unsupported/Eigen/CXX11/Tensor>

template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, bool ScalarValue, typename... Dim>
struct create_tensor;

// ScalarValue = true
template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, typename... Dim>
struct create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn, true, Dim...> {

  typedef Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> TensorOut;
  typedef typename TensorOut::Index IndexType;

  static TensorOut createTensor(const DerivedIn& value,  Dim... dim) {
    TensorOut ans(dim...);
    ans.setConstant(value);
    return(ans);
  }

};

// ScalarValue = true and Dim is Eigen::Tensor<int, 1>
template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn>
struct create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn, true, Eigen::Tensor<int, 1> > {

  typedef Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> TensorOut;
  typedef typename TensorOut::Index IndexType;
  
  static TensorOut createTensor(const DerivedIn& value, Eigen::Tensor<int, 1> dim) {
    std::array<IndexType, NumDimensionsOut> dims;
    for (unsigned int i = 0; i < NumDimensionsOut; i++) {
      dims[i] = static_cast<IndexType>(dim[i]);
    }
    TensorOut ans(dims);
    ans.setConstant(value);
    return(ans);
  }

};

// this is the case where DerivedIn is a vector Tensor type (ScalarValue = false)
template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, typename... Dim>
struct create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn, false, Dim...> {

  typedef Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> TensorOut;
  typedef typename TensorOut::Index IndexType;

  static TensorOut createTensor(const DerivedIn& value,  Dim... dim) {
    std::array<IndexType, NumDimensionsOut> new_dim{{static_cast<IndexType>(dim)...}};
    TensorOut ans = value.reshape(new_dim);
    return(ans);
  }
  
};

// ScalarValue = false and Dim is Eigen::Tensor<int, 1>
template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn>
struct create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn, false, Eigen::Tensor<int, 1> > {

  typedef Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> TensorOut;
  typedef typename TensorOut::Index IndexType;

  static TensorOut createTensor(const DerivedIn& value,  Eigen::Tensor<int, 1> dim) {
    std::array<IndexType, NumDimensionsOut> new_dim;
    for (unsigned int i = 0; i < NumDimensionsOut; i++) {
      new_dim[i] = static_cast<IndexType>(dim[i]);
    }
    TensorOut ans = value.reshape(new_dim);
    return(ans);
  }
  
};

template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, typename... Dim>
Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> createTensor(const DerivedIn& value, Dim... dim) {
  return(create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn,
	 std::is_scalar<DerivedIn>::value, Dim...>::createTensor(value, dim...));
}

#endif
