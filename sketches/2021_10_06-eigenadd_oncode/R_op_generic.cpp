#include "R_op_generic.h"
  
/* 
 * All of the [[Rcpp::export]] functions need to have their inputs and outputs 
 * explicitly written out (i.e., not templated) for Rcpp to build wrappers back 
 * to R
 */

// [[Rcpp::export]]
Eigen::Tensor<double, 2> R_add_mult_test_2_1(
    Eigen::Tensor<double, 2> a,
    Eigen::Tensor<double, 1> b
) {
  auto ans = R_binaryOp_t1_t2(
    a, 
    b, 
    Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
  return R_binaryOp_t1_t2(
    ans, 
    b, 
    Eigen::internal::scalar_product_op<Eigen::Tensor<double, 2>::Scalar>()
  );
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> R_add_test_2_2_default_error(
    Eigen::Tensor<double, 2> a,
    Eigen::Tensor<double, 2> b
) {
  return R_binaryOp_t1_t2(
    a, 
    b, 
    Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3> R_add_test_3_2(
    Eigen::Tensor<double, 3> a,
    Eigen::Tensor<double, 2> b
) {
  return R_binaryOp_t1_t2(
    a, 
    b, 
    Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> R_add_test_2_2(
    Eigen::Tensor<double, 2> a,
    Eigen::Tensor<double, 2> b
) {
  try { 
    return R_binaryOp_t1_t2(
      a, 
      b, 
      Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
    );
  } catch (...) { 
    throw std::runtime_error("a + b failed\n");
  }
  return a;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 5> R_add_test_5_3(
    Eigen::Tensor<double, 5> a,
    Eigen::Tensor<double, 3> b
) {
  return R_binaryOp_t1_t2(
    a, 
    b, 
    Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3> R_add_test_3_1(
    Eigen::Tensor<double, 3> a,
    Eigen::Tensor<double, 1> b
) {
  return R_binaryOp_t1_t2(
    a, 
    b, 
    Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> R_add_test_2_1(
    Eigen::Tensor<double, 2> a,
    Eigen::Tensor<double, 1> b
) {
  return R_binaryOp_t1_t2(
    a, 
    b, 
    Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
}