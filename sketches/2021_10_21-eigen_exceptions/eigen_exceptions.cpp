#include "preamble.h"

using namespace Rcpp;

// [[Rcpp::export]]
Eigen::Tensor<double, 1> vec_plus(Eigen::Tensor<double, 1> a,
                                  Eigen::Tensor<double, 1> b)  {
  
  RESET_EIGEN_ERRORS
  
  return a + b;
}

// [[Rcpp::export]]
Eigen::MatrixXd mat_plus(Eigen::MatrixXd a,
                         Eigen::MatrixXd b)  {
  
  RESET_EIGEN_ERRORS
  
  return a + b;
}

// [[Rcpp::export]]
Eigen::MatrixXd mat_mult(Eigen::MatrixXd a,
                         Eigen::MatrixXd b)  {
  
  RESET_EIGEN_ERRORS
  
  return a * b;
}