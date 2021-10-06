#include "preamble.h"
#include "eigen_smart_ops.h"

// [[Rcpp::export]]
Eigen::Tensor<double, 2> Rplus_mat_vec_test1(Eigen::Tensor<double, 2> a,
					     Eigen::Tensor<double, 1> b)  {
  Eigen::Tensor<double, 2> ans = Rplus_2_1_(a, b); // alt: Rplus<2, 1>(a, b)
  // We want Rplus_2_1_ to return an Eigen op (delayed evaluation object)
  // in case we have ans = Rplus_2_1_(a, b) + c.
  return ans;
}

