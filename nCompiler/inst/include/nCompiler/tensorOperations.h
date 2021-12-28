#ifndef _NCOMPILER_TENSOR_OPERATIONS
#define _NCOMPILER_TENSOR_OPERATIONS

/**
 * Generate functors similar to std::binary_function, but where the 
 * return type from operator() is determined automatically.
 */
#define NCOMPILER_BINOP(OPNAME, OP)                                     \
  namespace nCompiler {                                                 \
    struct OPNAME {                                                     \
      template<typename A_, typename B_>                                \
      auto operator()(const A_ &a, const B_ &b) -> decltype(a OP b) {   \
        return a OP b;                                                  \
      }                                                                 \
    };                                                                  \
  }                                                                     \
  
NCOMPILER_BINOP(plus, +)
NCOMPILER_BINOP(minus, -)
NCOMPILER_BINOP(product, *)
NCOMPILER_BINOP(divide, /)
NCOMPILER_BINOP(gt, >)
NCOMPILER_BINOP(geq, >=)
NCOMPILER_BINOP(lt, <)
NCOMPILER_BINOP(leq, <=)
NCOMPILER_BINOP(logical_and, &&)
NCOMPILER_BINOP(logical_or, ||)
NCOMPILER_BINOP(logical_eq, ==)
NCOMPILER_BINOP(logical_neq, !=)

namespace nCompiler {

  /**
   * Templated reshaping to use the Eigen library to lazily evaluate a `op` b 
   * when a and b are specializations of Eigen::TensorBase objects with
   * different dimensions
   * 
   * Eigen::Tensorbase specializations numerically represent tensors 
   * (i.e., via Eigen::Tensor) and also the results of performing mathematical 
   * operations on tensors (i.e., such as a + b via Eigen::TensorCwiseBinaryOp).
   * Eigen uses templates to generate C++ code that exploits compile-time 
   * optimizations that reduce redundant operations and temporary objects.
   * 
   * The arguments a and b must have the same number of elements, otherwise an 
   * error will be thrown.  For example, the intended use is for when a is  
   * either a column or row matrix, and b is a vector of the same length.
   * 
   * The template function relies on automatic template deduction to allow the 
   * function to be called recursively without requiring users to explicitly 
   * declare object types.
   * 
   * @param a specialization of Eigen::TensorBase (i.e., Eigen::Tensor, or 
   *   Eigen::TensorCwiseBinaryOp)
   * @param b specialization of Eigen::TensorBase (i.e., Eigen::Tensor, or 
   *   Eigen::TensorCwiseBinaryOp)
   * @param o a binary operator suitable for use with Eigen::TensorBase objects
   * 
   * @return a `op` b, with dimensions matching those of the argument a
   */
    template<typename OP_, typename A_, typename B_>
    auto binaryOpReshapeRHS(const A_ &a, const B_ &b) -> decltype(
        OP_()(a,
              b.reshape(
                Eigen::TensorRef<
                  Eigen::Tensor<typename A_::Scalar, A_::NumDimensions>
                >(a).dimensions()
              )
        )
    ) {
      // dimensions of a
      Eigen::TensorRef<
        Eigen::Tensor<typename A_::Scalar, A_::NumDimensions>
      > aEval(a);
      auto aDim = aEval.dimensions();
      // dimensions of b
      Eigen::TensorRef<
        Eigen::Tensor<typename B_::Scalar, B_::NumDimensions>
      > bEval(b);
      // throw runtime error if number of elements differ
      if(aEval.size() != bEval.size()) {
        throw std::range_error(
            "nCompiler::binaryOpReshape - Tensors have unequal size.\n"
        );
      }
      // perform a `op` b after reshaping b into a tensor with a's dimensions
      return OP_()(a, b.reshape(aDim));
    }
    
    template<typename OP_>
    struct reverseOp {
       template<typename A_, typename B_>
       auto operator()(const A_ &a, const B_ &b) -> decltype(OP_()(b, a)) {
          return OP_()(b, a);
       }
    };
    
    template<typename OP_, typename A_, typename B_>
    auto binaryOpReshapeLHS(const A_ &a, const B_ &b) -> decltype(
          binaryOpReshapeRHS<reverseOp<OP_>, B_, A_>(b, a)
    ) {
       return binaryOpReshapeRHS<reverseOp<OP_>, B_, A_>(b, a);
    }
    
}

/**
 * Evalaute binary operations between Eigen::Tensor and Eigen::SparseMatrix
 * objects by mapping the Tensor object's data to an Eigen::Matrix object.
 * Currently, only supported for rank 2 tensors.
 *
 * @tparam OP_ Functor wrapping a binary operator
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 * @return Assume that x is properly dense, so that return type is also dense
 */
template<typename OP_, typename Scalar>
Eigen::Tensor<Scalar, 2> binaryOp(
    const Eigen::Tensor<Scalar, 2> &x, const Eigen::SparseMatrix<Scalar> &y
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // input tensor dimensions
    auto xDim = x.dimensions();
    // initialize return Tensor
    Eigen::Tensor<Scalar, 2> z(xDim);
    // Map Tensors to Matrix classes, for compatibility
    const Eigen::Map<const MatrixType> xmat(x.data(), xDim[0], xDim[1]);
    Eigen::Map<MatrixType> zmat(z.data(), xDim[0], xDim[1]);
    // perform operation, return results
    zmat = OP_()(xmat, y);
    return z;
}

/**
 * Evalaute binary operations between Eigen tensor expression objects and
 * Eigen::SparseMatrix objects by first evaluating the tensor expression into a
 * concrete Eigen::Tensor object.
 *
 * @tparam OP_ Functor wrapping a binary operator
 * @tparam TensorExpr type for an unevaluated tensor expression
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 * @return Assume that x is properly dense, so that return type is also dense
 */
template<typename OP_, typename TensorExpr, typename Scalar>
Eigen::Tensor<Scalar, TensorExpr::NumDimensions> binaryOp(
    const TensorExpr &x, const Eigen::SparseMatrix<Scalar> &y
) {
    // evaluate input tensor
    const Eigen::Tensor<Scalar, TensorExpr::NumDimensions> xEval = x;
    // perform operation, return results
    return binaryOp<OP_, Scalar>(xEval, y);
}

/**
 * Globally overloaded operators to define x OP y where x (or y) is an
 * Eigen::Tensor or Tensor expression object (i.e., an object derived from
 * Eigen::TensorBase) and y (or x) is an Eigen::SparseMatrix<Scalar> object,
 * where Scalar is a template parameter.
 *
 * @param OP The operator to overload, i.e., +, -, /, *
 * @param OP_FNCTR Functor that wraps the binary operation
 */
#define TENSOR_SPMAT_OP(OP, OP_FNCTR)                                          \
template<typename TensorExpr, typename Scalar>                                 \
Eigen::Tensor<Scalar, TensorExpr::NumDimensions> operator OP(                  \
    const TensorExpr &x, const Eigen::SparseMatrix<Scalar> &y                  \
) {                                                                            \
    return binaryOp<OP_FNCTR>(x,y);                                            \
}                                                                              \
                                                                               \
template<typename TensorExpr, typename Scalar>                                 \
Eigen::Tensor<Scalar, TensorExpr::NumDimensions> operator OP(                  \
    const Eigen::SparseMatrix<Scalar> &x, const TensorExpr &y                  \
) {                                                                            \
    return binaryOp<nCompiler::reverseOp<OP_FNCTR>>(y,x);                      \
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

/**
 * Convert an Eigen::Tensor or Tensor expression object (i.e., an object derived
 * from Eigen::TensorBase) to an Eigen::SparseMatrix<Scalar> object.
 *
 * @tparam TensorExpr Eigen::Tensor or Tensor expression class
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 * @param x Object to convert to an Eigen::SparseMatrix
 */
template<typename TensorExpr, typename Scalar = typename TensorExpr::Scalar>
Eigen::SparseMatrix<Scalar> asSparse(const TensorExpr &x) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // evaluate input tensor
    const Eigen::Tensor<Scalar, TensorExpr::NumDimensions> xEval = x;
    // map to matrix, sparsify and return
    auto xDim = xEval.dimensions();
    Eigen::Map<const MatrixType> xmat(xEval.data(), xDim[0], xDim[1]);
    return xmat.sparseView();
}

/**
 * Remove additional 0's from Eigen::SparseMatrix<Scalar> object, if requested
 *
 * @tparam Scalar (primitive) type for SparseMatrix entries
 * @param x SparseMatrix object to prune
 * @param prune true to re-compress x by removing 0's from representation
 */
template<typename Scalar>
Eigen::SparseMatrix<Scalar> asSparse(
    Eigen::SparseMatrix<Scalar> &x, bool prune
) {
    if(prune) {
        return x.pruned();
    } else {
        return x;
    }
}
/**
 * Convert an Eigen::SparseMatrix or SparseMatrix expression object (i.e., an
 * object derived from Eigen::SparseMatrixBase) to an Eigen::Tensor<Scalar, 2>
 * object.
 *
 * @tparam SpMatExpr  Eigen::SparseMatrix or Sparse Matrix expression class
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 * @param x Object to convert to an Eigen::Tensor<Scalar, 2>
 */
template<typename SpMatExpr, typename Scalar = typename SpMatExpr::Scalar>
Eigen::Tensor<Scalar, 2> asDense(SpMatExpr &x) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // initialize return tensor
    Eigen::Tensor<Scalar, 2> res(x.rows(), x.cols());
    // Map tensor to matrix class, return results
    Eigen::Map<MatrixType> tmp_mat(res.data(), x.rows(), x.cols());
    tmp_mat = MatrixType(x);
    return res;
}

/**
 * Compute the Cholesky decomposition for a symmetric matrix stored as an
 * Eigen::Tensor<Scalar, 2> object.
 *
 * To be compatible with R's implementation of chol(), this function returns the
 * upper-triangular Cholesky factor.
 *
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 */
template<typename Scalar>
Eigen::Tensor<Scalar, 2> chol(const Eigen::Tensor<Scalar, 2> &x) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // map to matrix
    auto xDim = x.dimensions();
    Eigen::Map<const MatrixType> xmat(x.data(), xDim[0], xDim[1]);
    // initialize Eigen::Tensor to store the decomposition
    Eigen::Tensor<Scalar, 2> res(xDim[0], xDim[1]);
    // decompose
    Eigen::LLT<MatrixType> llt(xmat);
    // extract upper Cholesky factor and return
    Eigen::Map<MatrixType> resMat(res.data(), xDim[0], xDim[1]);
    resMat = llt.matrixU();
    return res;
}

/**
 * Compute the Cholesky decomposition for a symmetric matrix stored as an
 * Eigen::Tensor<Scalar, 2> object, or derived from a Tensor expression
 * object (i.e., an object derived from Eigen::TensorBase).
 *
 * To be compatible with R's implementation of chol(), this function returns the
 * upper-triangular Cholesky factor.
 *
 * @tparam TensorExpr type for an unevaluated tensor expression
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 */
template<typename TensorExpr, typename Scalar = typename TensorExpr::Scalar>
Eigen::Tensor<Scalar, 2> chol(const TensorExpr &x) {
    // evaluate input tensor
    const Eigen::Tensor<Scalar, TensorExpr::NumDimensions> xEval = x;
    // evaluate cholesky for evaluated tensor
    return chol(xEval);
}

/**
 * Extract the primary diagonal from an Eigen::Tensor object, or derived from
 * a Tensor expression object (i.e., an object derived from Eigen::TensorBase).
 *
 * @tparam TensorExpr type for an unevaluated tensor expression
 * @tparam Scalar (primitive) type for Tensor and SparseMatrix entries
 */
template<typename TensorExpr, typename Scalar = typename TensorExpr::Scalar>
Eigen::Tensor<Scalar, 1> nDiag(const TensorExpr &x) {
    // access elements of x without fully evaluating x if a tensor expression
    Eigen::TensorRef<TensorExpr> ref(x);
    // determine dimensions of x, and size of main diagonal
    auto xDim = ref.dimensions();
    auto nDiag = *(std::min_element(xDim.begin(), xDim.end()));
    // initialize and fill output
    Eigen::Tensor<Scalar, 1> res(nDiag);
    Scalar *diagIt = res.data();
    Scalar *diagEnd = diagIt + nDiag;
    auto indexEnd = xDim.end();
    unsigned long i = 0;
    for(diagIt; diagIt != diagEnd; ++diagIt) {
        for(auto index = xDim.begin(); index != indexEnd; ++index)
            *index = i;
        *diagIt = ref.coeff(xDim);
        ++i;
    }
    return res;
}

/**
 * Transpose a tensor by reversing the order of the dimensions via shuffling
 *
 * @tparam TensorExpr type for a tensor or an unevaluated tensor expression
 */
 template<typename TensorExpr>
 auto t(const TensorExpr &x) -> decltype(
     x.shuffle(x.dimensions()) // correct return type, but incorrect output
 ){
    typename TensorExpr::Index i = TensorExpr::NumDimensions;
    typename TensorExpr::Dimensions o;
    auto end = o.end();
    for(auto it = o.begin(); it != end; ++it) {
        *it = --i;
    }
    return x.shuffle(o);
 }

 /**
  * Solve a lower-triangular system when RHS represents a matrix or a vector
  *
  * @tparam Scalar (primitive) type for Matrix entries
  * @tparam RHSType A specialization of Eigen::Matrix<Scalar, xxx, yyy>
  */
template<typename Scalar, typename RHSType>
RHSType forwardsolve(
    const Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> & L,
    const RHSType & b
) {
    return L.template triangularView<Eigen::Lower>().solve(b);
}

/**
 * Solve an upper-triangular system when RHS represents a matrix or a vector
 *
 * @tparam Scalar (primitive) type for Matrix entries
 * @tparam RHSType A specialization of Eigen::Matrix<Scalar, xxx, yyy>
 */
template<typename Scalar, typename RHSType>
RHSType backsolve(
    const Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> & U,
    const RHSType & b
) {
    return U.template triangularView<Eigen::Upper>().solve(b);
}

/**
  * Solve a lower-triangular system when RHS represents a vector,
  * and the inputs are stored in Tensor objects
  *
  * @tparam Scalar (primitive) type for Matrix entries
  */
template<typename Scalar>
Eigen::Tensor<Scalar, 1> forwardsolve(
    const Eigen::Tensor<Scalar, 2> & L , const Eigen::Tensor<Scalar, 1> & b
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, 1> VectorType;
    // Map inputs to matrices
    auto lDim = L.dimensions();
    auto bDim = b.dimensions();
    Eigen::Map<const MatrixType> lmat(L.data(), lDim[0], lDim[1]);
    Eigen::Map<const VectorType> bvec(b.data(), bDim[0], 1);
    // solve system and map to output
    Eigen::Tensor<Scalar, 1> res(bDim[0]);
    Eigen::Map<VectorType> resVec(res.data(), bDim[0]);
    resVec = forwardsolve<Scalar, VectorType>(lmat, bvec);
    return res;
}

/**
  * Solve a lower-triangular system when RHS represents a matrix,
  * and the inputs are stored in Tensor objects
  *
  * @tparam Scalar (primitive) type for Matrix entries
  */
template<typename Scalar>
Eigen::Tensor<Scalar, 2> forwardsolve(
    const Eigen::Tensor<Scalar, 2> & L , const Eigen::Tensor<Scalar, 2> & b
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // Map inputs to matrices
    auto lDim = L.dimensions();
    auto bDim = b.dimensions();
    Eigen::Map<const MatrixType> lmat(L.data(), lDim[0], lDim[1]);
    Eigen::Map<const MatrixType> bmat(b.data(), bDim[0], bDim[1]);
    // solve system and map to output
    Eigen::Tensor<Scalar, 2> res(bDim[0], bDim[1]);
    Eigen::Map<MatrixType> resMat(res.data(), bDim[0], bDim[1]);
    resMat = forwardsolve<Scalar, MatrixType>(lmat, bmat);
    return res;
}

/**
  * Solve a lower-triangular system when RHS represents a matrix or vector,
  * and the inputs are stored as Tensors or as the result of tensor operations
  *
  * @tparam Scalar (primitive) type for Matrix entries
  */
template<typename LHSExpr,
        typename RHSExpr,
        typename Scalar = typename LHSExpr::Scalar>
Eigen::Tensor<Scalar, RHSExpr::NumDimensions> forwardsolve(
    const LHSExpr & L, const RHSExpr & b
) {
    Eigen::Tensor<Scalar, LHSExpr::NumDimensions> lEval(L);
    Eigen::Tensor<Scalar, RHSExpr::NumDimensions> bEval(b);
    return forwardsolve(lEval, bEval);
}

#endif
