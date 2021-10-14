#include "R_op_generic.h"
  
// [[Rcpp::export]]
Eigen::Tensor<double, 2> R_op_test(Eigen::Tensor<double, 2> a,
                                   Eigen::Tensor<double, 1> b)  {
  
  // // a binary operator can be passed as a function argument
  // auto ans = R_op_chainable(
  //   a, b, Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  // );
  
  // // Eigen defines many operators in BinaryFunctors.h, and the class def's. 
  // // look simple enough for us to define our own operators
  // auto ans = R_op_chainable(
  //   a, b, Eigen::internal::scalar_product_op<Eigen::Tensor<double, 2>::Scalar>()
  // );
  
  // TODO: demonstrate how to define custom binary operators, such as a 
  //   scalar_power_op class that allows componentwise a^b to be evaluated here.
  
  // // chaining operators works!
  // auto step1 = R_op_chainable(
  //   a, b, Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  // );
  // auto step2 = R_op_chainable(
  //   step1, a, Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  // );
  // auto ans = R_op_chainable(
  //   step1, step2, Eigen::internal::scalar_product_op<Eigen::Tensor<double, 2>::Scalar>()
  // );
  
  // chaining operators works!
  auto ans = R_op_chainable(
    R_op_chainable(
      a, b, Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
    ), b, Eigen::internal::scalar_product_op<Eigen::Tensor<double, 2>::Scalar>()
  );
  
  return ans;
}