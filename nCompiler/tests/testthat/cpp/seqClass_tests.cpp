#include "preamble.h"

// [[Rcpp::export]]
Eigen::Tensor<double, 1> seqClass_test1(double from, double to, double by) {
  Eigen::Tensor<double, 1> ans = nSeqByD(from, to, by);
  return(ans);
}
