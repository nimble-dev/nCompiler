#ifndef _NCOMPILER_SET_WHICH
#define _NCOMPILER_SET_WHICH

#include <unsupported/Eigen/CXX11/Tensor>

template<int NumDims>
struct which_impl {
  static Eigen::Tensor<int, 1> setWhich(const Eigen::Tensor<bool, NumDims> &boolArg) {
    std::vector<int> map_data;
    map_data.reserve(boolArg.size());
    bool nextBool;
    for (unsigned int i = 0; i < boolArg.size(); i++ ) {
      nextBool = boolArg[i];
      // i + 1 makes it one-based indexing, which will then be adjusted back
      // to zero-based when used for indexing something else
      if (nextBool) map_data.push_back(i + 1);
    }
    Eigen::TensorMap<Eigen::Tensor<int, 1>> ans(&map_data[0], map_data.size());
    return(ans);
  }
};

inline Eigen::Tensor<int, 1> setWhich0(bool boolArg) {
  if (boolArg) {
    Eigen::Tensor<int, 1> ans(1);
    ans.setValues({1});
    return(ans);
  } else {
    Eigen::Tensor<int, 1> ans(0);
    return(ans);
  }
}

#define setWhich1 which_impl<1>::setWhich

#endif
