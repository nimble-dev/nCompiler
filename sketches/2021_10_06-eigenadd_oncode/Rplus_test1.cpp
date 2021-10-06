#include "preamble.h"
#include "eigen_smart_ops.h"

// This calls Rplus_, which has a bare bones illustration of how to access dimension information in Eigen.
// We can use that kind of trick for our own run-time dimension checking and dyanmic handling.
// In any dynamic handling, the return type can't be dynamic!

// [[Rcpp::export]]
Eigen::Tensor<double, 1> Rplus_test1(Eigen::Tensor<double, 1> a,
				     Eigen::Tensor<double, 1> b)  {
  Eigen::Tensor<double, 1> ans = Rplus_(a, b);
  return ans;
}

