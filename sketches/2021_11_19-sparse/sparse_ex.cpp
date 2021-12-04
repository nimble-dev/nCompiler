#include <RcppEigen.h>
#include <Rcpp.h>

#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/tensorOperations.h>

using namespace Rcpp;

// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

Eigen::Tensor<double, 2> operator+(
    const Eigen::Tensor<double, 2> &x,
    const Eigen::SparseMatrix<double> &y
) {
    const Eigen::Tensor<double, 2>::Dimensions& d = x.dimensions();
    const Eigen::Map<const Eigen::MatrixXd> x_mtx(x.data(), d[0], d[1]);
    Eigen::Tensor<double, 2> ans(d[0], d[1]);
    Eigen::Map<Eigen::MatrixXd> ans_mtx(ans.data(), d[0], d[1]);
    ans_mtx = x_mtx + y;
    return ans;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> addTensorSpmat(
    Eigen::Tensor<double, 2> x,
    Eigen::SparseMatrix<double> y
) {
    return x + y;
}

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

// [[Rcpp::export]]
Eigen::SparseMatrix<double> addSparseDense(Eigen::SparseMatrix<double> x,
                                           Eigen::MatrixXd y) {
    Eigen::SparseMatrix<double> res = x + y;
    // prune 0's from sparse matrix on return, if any exist
    return res.pruned();
}