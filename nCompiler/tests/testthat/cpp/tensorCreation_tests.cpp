#include "preamble.h"

// [[Rcpp::export]]
Eigen::Tensor<int, 1> tensorCreation1(double value, int length) {
  Eigen::Tensor<int, 1> ans = createTensor<int, 1>(value, length);
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<int, 2> tensorCreation2(Eigen::Tensor<double, 1> value) {
  Eigen::Tensor<int, 2> ans = createTensor<int, 2>(value.cast<int>(), 2, 3);
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3> tensorCreation3(Eigen::Tensor<double, 1> value) {
  Eigen::Tensor<double, 3> ans = createTensor<double, 3>(value, 2, 3, 2);
  return(ans);
}
