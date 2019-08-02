#include "preamble.h"

// [[Rcpp::export]]
Eigen::Tensor<double, 1> repClass_test1(Eigen::Tensor<double, 1> x) {
  return(repTimes1d(x, 10));
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> repClass_test2(Eigen::Tensor<double, 2> x, int times) {
  return(repTimes2d(x, times));
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> repClass_test3(Eigen::Tensor<double, 1> x, int length_out) {
  return(repLen1d(x, length_out));
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> repClass_test4(Eigen::Tensor<double, 3> x, int length_out) {
  return(repLen3d(x, length_out));
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> repClass_test5(Eigen::Tensor<double, 2> x, int times) {
  return(repTimesEval2d(x, times));
}
