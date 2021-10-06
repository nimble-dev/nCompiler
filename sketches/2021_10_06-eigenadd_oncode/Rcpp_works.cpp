#include "preamble.h"

// This is used as a test that one's system is set up correctly and compiling with Rcpp + Eigen.
// Compile this from work_on_Eigen_addons.R

// [[Rcpp::export]]
Eigen::Tensor<double, 1> vec_plus(Eigen::Tensor<double, 1> a,
				  Eigen::Tensor<double, 1> b)  {
  Eigen::Tensor<double, 1> ans = a + b;
  return ans;
}

