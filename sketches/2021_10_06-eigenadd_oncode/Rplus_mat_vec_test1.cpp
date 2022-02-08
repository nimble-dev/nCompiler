#include "preamble.h"
#include "eigen_smart_ops.h"

// [[Rcpp::export]]
Eigen::Tensor<double, 2> Rplus_mat_vec_test1(Eigen::Tensor<double, 2> a,
					     Eigen::Tensor<double, 1> b)  {
  
  // chaining with an explicit operator works!
  auto ans = Rplus_2_1_(a, b) + Rplus_2_1_(a, b);
  
  // // chaining with an explicit operator works!
  // auto ans = Rplus_2_1_(a, b) + a;
  
  // // compilation error when attempting to chain without an explicit operator.
  // // chaining without an explicit operator might be important if we want to 
  // // return unevaluated Eigen op's that use a combination of operators, some 
  // // of which may be user-defined.
  // auto step1 = Rplus_2_1_(a,b);
  // auto step2 = Rplus_2_1_(step1,a);
  
  // We want Rplus_2_1_ to return an Eigen op (delayed evaluation object)
  // in case we have ans = Rplus_2_1_(a, b) + c.
  return ans;
}

