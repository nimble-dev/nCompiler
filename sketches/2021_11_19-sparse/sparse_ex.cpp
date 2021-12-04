#include <RcppEigen.h>
#include <Rcpp.h>

#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/tensorOperations.h>

using namespace Rcpp;

// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

/**
 * Support binary addition between Eigen::Tensor and Eigen::SparseMatrix objects
 * by mapping the Tensor object's data to an Eigen::Matrix object.
 *
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 * @return Assume that x is properly dense, so that return type is also dense
 */
template<typename Scalar>
Eigen::Tensor<Scalar, 2> operator+(
    const Eigen::Tensor<Scalar, 2> &x,
    const Eigen::SparseMatrix<Scalar> &y
) {
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    const typename Eigen::Tensor<Scalar, 2>::Dimensions& d = x.dimensions();
    const Eigen::Map<const MatrixType> xmat(x.data(), d[0], d[1]);
    Eigen::Tensor<Scalar, 2> z(d[0], d[1]);
    Eigen::Map<MatrixType> zmat(z.data(), d[0], d[1]);
    zmat = xmat + y;
    return z;
}

/**
 * Finish supporting binary addition between Eigen::Tensor and
 * Eigen::SparseMatrix objects
 */
template<typename Scalar>
Eigen::Tensor<Scalar, 2> operator+(
        const Eigen::SparseMatrix<Scalar> &x,
        const Eigen::Tensor<Scalar, 2> &y
        ) {
    return y + x;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> addTensorSpmat(
    Eigen::Tensor<double, 2> x,
    Eigen::SparseMatrix<double> y
) {
    return y + x;
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