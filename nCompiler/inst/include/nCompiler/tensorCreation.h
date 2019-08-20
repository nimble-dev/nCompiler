#ifndef _NCOMPILER_TENSOR_CREATION
#define _NCOMPILER_TENSOR_CREATION

template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, bool ScalarValue, typename... Dim>
struct create_tensor;

// ScalarValue = true
template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, typename... Dim>
struct create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn, true, Dim...> {

  static Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> createTensor(const DerivedIn& value,  Dim... dim) {
    Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> ans(dim...);
    ans.setConstant(value);
    return(ans);
  }

};

// this is the case where DerivedIn is a vector Tensor type (ScalarValue = false)
template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, typename... Dim>
struct create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn, false, Dim...> {

  typedef typename DerivedIn::Index IndexType;

  static Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> createTensor(const DerivedIn& value,  Dim... dim) {
    std::array<IndexType, NumDimensionsOut> new_dim{{static_cast<IndexType>(dim)...}};
    Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> ans = value.reshape(new_dim);
    return(ans);
  }
  
};

template <typename ScalarTypeOut, int NumDimensionsOut, typename DerivedIn, typename... Dim>
Eigen::Tensor<ScalarTypeOut, NumDimensionsOut> createTensor(const DerivedIn& value, Dim... dim) {
  return(create_tensor<ScalarTypeOut, NumDimensionsOut, DerivedIn,
	 std::is_scalar<DerivedIn>::value, Dim...>::createTensor(value, dim...));
}

#endif
