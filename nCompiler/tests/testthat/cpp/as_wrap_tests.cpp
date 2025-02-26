#define NCOMPILER_USES_EIGEN

#include<nCompiler/nCompiler_omnibus.h>

using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

RcppExport SEXP tensor2D_by_copy(SEXP Sx) {
BEGIN_RCPP
  // Exactly the format used by Rcpp
  Rcpp::traits::input_parameter< Eigen::Tensor<double, 3> >::type x(Sx);
// This is necessary to trigger the delayed export of x.
// This would normally be done when calling the actual function of interest.
 Eigen::Tensor<double, 3> y = Eigen::Tensor<double, 3>(x);
 SEXP Sans; // Rcpp:Robject Sans
 Sans = Rcpp::wrap(y);
 return Sans;
END_RCPP
}

RcppExport SEXP tensor2D_by_ref(SEXP Sx) {
BEGIN_RCPP
  // Exactly the format used by Rcpp
  Rcpp::traits::input_parameter< Eigen::Tensor<double, 3>& >::type x(Sx);
// This is necessary to trigger the delayed export of x.
// This would normally be done when calling the actual function of interest.
 Eigen::Tensor<double, 3> y = Eigen::Tensor<double, 3>(x);
 SEXP Sans; // Rcpp:Robject Sans
 Sans = Rcpp::wrap(y);
 return Sans;
END_RCPP
}
