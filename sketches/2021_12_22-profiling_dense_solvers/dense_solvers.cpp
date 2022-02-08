#include <RcppEigen.h>

using namespace Rcpp;

// [[Rcpp::depends(RcppEigen)]]

Eigen::VectorXd forwardSolve(
  const Eigen::MatrixXd & L, const Eigen::VectorXd & b)
{
  return L.triangularView<Eigen::Lower>().solve(b);
}

Eigen::VectorXd backSolve(
  const Eigen::MatrixXd & U, const Eigen::VectorXd & b)
{
  return U.triangularView<Eigen::Upper>().solve(b);
}

// [[Rcpp::export]]
Eigen::VectorXd LLTClassSolver(
  const Eigen::MatrixXd & A, const Eigen::VectorXd & b
) {
  return A.llt().solve(b);
}

// [[Rcpp::export]]
Eigen::VectorXd LowerSolver(
  const Eigen::MatrixXd & A, const Eigen::VectorXd & b
) {
  Eigen::LLT<Eigen::MatrixXd, Eigen::Lower> lltOfA(A);
  Eigen::MatrixXd L = lltOfA.matrixL();
  // this can probably be accelerated by doing in-place solves, but this would
  // be difficult to describe from within R
  Eigen::VectorXd y = forwardSolve(L, b);
  return backSolve(L.transpose(), y);
}

// [[Rcpp::export]]
Eigen::VectorXd UpperSolver(
  const Eigen::MatrixXd & A, const Eigen::VectorXd & b
) {
  Eigen::LLT<Eigen::MatrixXd, Eigen::Upper> lltOfA(A);
  Eigen::MatrixXd Lt = lltOfA.matrixU();
  // this can probably be accelerated by doing in-place solves, but this would
  // be difficult to describe from within R
  Eigen::VectorXd y = forwardSolve(Lt.transpose(), b);
  return backSolve(Lt, y);
}

// [[Rcpp::export]]
Eigen::VectorXd LDLTClassSolver(
  const Eigen::MatrixXd & A, const Eigen::VectorXd & b
) {
  return A.ldlt().solve(b);
}
