#include "preamble.h"

// [[Rcpp::export]]
Eigen::Tensor<int, 1> setWhich_test1(bool x) {
  return(setWhich0(x));
}

// [[Rcpp::export]]
Eigen::Tensor<int, 1> setWhich_test2(Eigen::Tensor<bool, 1> x) {
  return(setWhich1(x));
}
