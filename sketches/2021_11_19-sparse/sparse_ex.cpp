#include <RcppEigen.h>
#include <Rcpp.h>

#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/tensorOperations.h>

using namespace Rcpp;

// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]

/**
 * Evalaute binary operations between Eigen::Tensor and Eigen::SparseMatrix
 * objects by mapping the Tensor object's data to an Eigen::Matrix object.
 *
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 * @tparam OP_ Functor wrapping a binary operator
 * @return Assume that x is properly dense, so that return type is also dense
 */
template<typename Scalar, typename OP_>
Eigen::Tensor<Scalar, 2> binaryOp(
    const Eigen::Tensor<Scalar, 2> &x, const Eigen::SparseMatrix<Scalar> &y
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // Dimensions of input tensor
    const typename Eigen::Tensor<Scalar, 2>::Dimensions& d = x.dimensions();
    // initialize return Tensor
    Eigen::Tensor<Scalar, 2> z(d[0], d[1]);
    // Map Tensors to Matrix classes, for compatibility
    const Eigen::Map<const MatrixType> xmat(x.data(), d[0], d[1]);
    Eigen::Map<MatrixType> zmat(z.data(), d[0], d[1]);
    // perform operation, return results
    zmat = OP_()(xmat, y);
    return z;
}

/**
 * Globally overloaded operators to define x OP y where x (or y) is an
 * Eigen::Tensor<Scalar, 2> object and y (or x) is an
 * Eigen::SparseMatrix<Scalar> object, where Scalar is a template parameter.
 *
 * @param OP The operator to overload, i.e., +, -, /, *
 * @param OP_FNCTR Functor that wraps the binary operation
 */
#define TENSOR_SPMAT_OP(OP, OP_FNCTR)                                          \
template<typename Scalar>                                                      \
Eigen::Tensor<Scalar, 2> operator OP(                                          \
    const Eigen::Tensor<Scalar, 2> &x, const Eigen::SparseMatrix<Scalar> &y    \
) {                                                                            \
    return binaryOp<Scalar, OP_FNCTR>(x,y);                                    \
}                                                                              \
                                                                               \
template<typename Scalar>                                                      \
Eigen::Tensor<Scalar, 2> operator OP(                                          \
    const Eigen::SparseMatrix<Scalar> &x, const Eigen::Tensor<Scalar, 2> &y    \
) {                                                                            \
    return binaryOp<Scalar, nCompiler::reverseOp<OP_FNCTR>>(y,x);              \
}

TENSOR_SPMAT_OP(+, nCompiler::plus)
TENSOR_SPMAT_OP(-, nCompiler::minus)
TENSOR_SPMAT_OP(*, nCompiler::product)
TENSOR_SPMAT_OP(/, nCompiler::divide)
TENSOR_SPMAT_OP(>, nCompiler::gt)
TENSOR_SPMAT_OP(>=, nCompiler::geq)
TENSOR_SPMAT_OP(<, nCompiler::lt)
TENSOR_SPMAT_OP(<=, nCompiler::leq)
TENSOR_SPMAT_OP(&&, nCompiler::logical_and)
TENSOR_SPMAT_OP(||, nCompiler::logical_or)
TENSOR_SPMAT_OP(!=, nCompiler::logical_neq)

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