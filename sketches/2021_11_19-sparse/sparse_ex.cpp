#include <RcppEigen.h>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

// [[Rcpp::export]]
Eigen::SparseMatrix<double> add(
    Eigen::SparseMatrix<double> x, Eigen::SparseMatrix<double> y
) {
    return x + y;
};

// [[Rcpp::export]]
Eigen::SparseMatrix<double> mult(
        Eigen::SparseMatrix<double> x, Eigen::SparseMatrix<double> y
        ) {
    return x * y;
};

// [[Rcpp::export]]
Rcpp::List chol_eigen(Eigen::SparseMatrix<double> x) {
    Eigen::SimplicialLDLT<Eigen::SparseMatrix<double>> solver(x);
    return List::create(
            Named("perm") = solver.permutationP().indices(),
            Named("perm_inv") = solver.permutationPinv().indices(),
            Named("L") = solver.matrixL(),
            Named("D") = solver.vectorD()
            );
}

// [[Rcpp::export]]
Eigen::MatrixXd asDense(Eigen::SparseMatrix<double> x) {
    return x;
}

// [[Rcpp::export]]
Eigen::SparseMatrix<double> asSparse(Eigen::MatrixXd x) {
    // Eigen::MatrixBase::sparseView can also accept a tolerance argument
    return x.sparseView();
}