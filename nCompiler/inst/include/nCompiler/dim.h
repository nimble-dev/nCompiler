#ifndef _NCOMPILER_DIM
#define _NCOMPILER_DIM

template<typename TensorIn>
Eigen::Tensor<int, 1> dim(const TensorIn& x) {
  std::array<long, TensorIn::NumDimensions> map_data = x.dimensions();
  Eigen::TensorMap<Eigen::Tensor<long, 1>> ans(map_data.data(), TensorIn::NumDimensions);
  return(ans.cast<int>());
}

#endif
