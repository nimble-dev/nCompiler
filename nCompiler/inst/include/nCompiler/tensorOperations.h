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
 * Implicitly convert an arbitrary input to an Eigen::Tensor object, as needed
 *
 * The compiler uses implicit conversion to decide how to convert the TensorXpr
 * input x to an Eigen::Tensor<Scalar, NumDimensions> object. Naturally, the
 * conversion process allocates memory for the Tensor, then populates the
 * Tensor's entries.
 *
 * Uses SFINAE to restrict the template function (nominally) to only work with
 * Tensor expression inputs vs. with evaluated, concrete Eigen::Tensor objects.
 *
 * If the input is already an Eigen::Tensor object, the desired behavior is to
 * return a reference to the object (implemented in an overloaded function)
 * because the input is already an Eigen::Tensor object, which is the desired
 * output class.  If we were to call this function on an Eigen::Tensor object
 * (i.e., if SFINAE restriction is removed), it would only consume memory and
 * time by creating a copy of an otherwise reasonable input.
 *
 * @tparam TensorXpr Nominally, an unevaluated tensor expression
 */
template<
    typename TensorXpr,
    typename std::enable_if<
        !std::is_base_of<
            Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions>,
            TensorXpr
        >::value,
        TensorXpr
    >::type* = nullptr
>
Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions> eval(
    TensorXpr & x
) {
    return x;
}

/**
 * Return a reference to an Eigen::Tensor object, avoiding copying the input
 *
 * Uses SFINAE to restrict the template function to only work with Eigen::Tensor
 * inputs vs. with Tensor expression objects.
 *
 * If the input is a Tensor expression object, the desired behavior is to return
 * an Eigen::Tensor object with the results of the evaluated Tensor expression
 * (implemented in an overloaded function).  Calling this function on a Tensor
 * expression object (i.e., if SFINAE restriction is removed) will cause type
 * errors when eval() is nested within other functions that use Tensors, but
 * require evaluated inputs, such as wrappers for matrix multiplication and
 * linear solvers.
 *
 * @tparam TensorXpr An Eigen::Tensor object type
 */
template<
    typename TensorXpr,
    typename std::enable_if<
        std::is_base_of<
            Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions>,
            TensorXpr
        >::value,
        TensorXpr
    >::type* = nullptr
>
TensorXpr& eval(TensorXpr & x) {
    return x;
}

/**
 * Constant version of eval() with Eigen::Tensor inputs.
 *
 * See documentation for eval() with Eigen::Tensor inputs for more details.
 *
 * @tparam TensorXpr An Eigen::Tensor object type
 */
template<
    typename TensorXpr,
    typename std::enable_if<
        std::is_base_of<
            Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions>,
            TensorXpr
        >::value,
        TensorXpr
    >::type* = nullptr
>
const TensorXpr& eval(const TensorXpr & x) {
    return x;
}

/**
 * Create an Eigen::Matrix map view into a constant Eigen::Tensor<Scalar, 1>
 * object.  The Eigen::Matrix is assumed to be a column vector.
 *
 * @tparam Scalar (primitive) type for tensor entries
 */
template<typename Scalar>
Eigen::Map<const Eigen::Matrix<Scalar, Eigen::Dynamic, 1>> matmap(
    const Eigen::Tensor<Scalar, 1> & x
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, 1> MatrixType;
    // input tensor dimensions
    auto xDim = x.dimensions();
    // map tensor
    Eigen::Map<const MatrixType> xmat(x.data(), xDim[0], 1);
    return xmat;
}

/**
 * Create an Eigen::Matrix map view into a non-const Eigen::Tensor<Scalar, 1>
 * object.  The Eigen::Matrix is assumed to be a column vector.
 *
 * @tparam Scalar (primitive) type for tensor entries
 */
template<typename Scalar>
Eigen::Map<Eigen::Matrix<Scalar, Eigen::Dynamic, 1>> matmap(
    Eigen::Tensor<Scalar, 1> & x
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, 1> MatrixType;
    // input tensor dimensions
    auto xDim = x.dimensions();
    // map tensor
    Eigen::Map<MatrixType> xmat(x.data(), xDim[0], 1);
    return xmat;
}

/**
 * Create an Eigen::Matrix map view into a constant Eigen::Tensor<Scalar, 2>
 * object
 *
 * @tparam Scalar (primitive) type for tensor entries
 */
template<typename Scalar>
Eigen::Map<const Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic>> matmap(
    const Eigen::Tensor<Scalar, 2> & x
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // input tensor dimensions
    auto xDim = x.dimensions();
    // map tensor
    Eigen::Map<const MatrixType> xmat(x.data(), xDim[0], xDim[1]);
    return xmat;
}

/**
 * Create an Eigen::Matrix map view into a non-const Eigen::Tensor<Scalar, 1>
 * object
 *
 * @tparam Scalar (primitive) type for tensor entries
 */
template<typename Scalar>
Eigen::Map<Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic>> matmap(
    Eigen::Tensor<Scalar, 2> & x
) {
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // input tensor dimensions
    auto xDim = x.dimensions();
    // map tensor
    Eigen::Map<MatrixType> xmat(x.data(), xDim[0], xDim[1]);
    return xmat;
}

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

class SparseCholesky;

template<typename SparseCholType = SparseCholesky, typename Scalar>
SparseCholType nChol(const Eigen::SparseMatrix<Scalar> &x) {
    Eigen::SimplicialLLT<Eigen::SparseMatrix<Scalar>> llt(x);
    SparseCholType res;
    res.R = llt.matrixU();
    Eigen::Tensor<int, 1> P(x.rows());
    auto pmap = matmap(P);
    pmap = llt.permutationP().indices();
    res.P = P;
    return res;
}

/**
 * Compute the Cholesky decomposition for a symmetric matrix stored as an
 * Eigen::Tensor<Scalar, 2> object, or derived from a Tensor expression object
 * (i.e., an object derived from Eigen::TensorBase).
 *
 * To be compatible with R's implementation of chol(), this function returns the
 * upper-triangular Cholesky factor.
 *
 * @tparam TensorExpr An Eigen::Tensor or tensor expression object type
 * @tparam Scalar (primitive) type for Tensor entries
 */
template<
    typename TensorExpr,
    typename Scalar = typename TensorExpr::Scalar,
    typename std::enable_if<
        !std::is_base_of<
            Eigen::SparseMatrix<Scalar>,
            TensorExpr
        >::value,
        TensorExpr
    >::type* = nullptr
>
Eigen::Tensor<Scalar, 2> nChol(const TensorExpr &x) {
    // evaluate arguments, if necessary
    const auto & x_eval = eval(x);
    // Eigen::Matrix class compatible with function arguments
    typedef Eigen::Matrix<Scalar, Eigen::Dynamic, Eigen::Dynamic> MatrixType;
    // map to matrix
    auto xDim = x_eval.dimensions();
    Eigen::Map<const MatrixType> xmat(x_eval.data(), xDim[0], xDim[1]);
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
 * Extract the primary diagonal from an Eigen::Tensor object, or derived from
 * a Tensor expression object (i.e., an object derived from Eigen::TensorBase).
 *
 * @tparam TensorExpr type for an unevaluated tensor expression
 * @tparam Scalar (primitive) type for Tensor entries
 */
template<
    typename TensorXpr,
    typename Scalar = typename TensorXpr::Scalar,
    typename std::enable_if<
        std::is_base_of<
            Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions>,
            TensorXpr
        >::value,
        TensorXpr
    >::type* = nullptr
>
Eigen::Tensor<Scalar, 1> nDiag(const TensorXpr &x) {
    // access elements of x without fully evaluating x if a tensor expression
    Eigen::TensorRef<TensorXpr> ref(x);
    // determine dimensions of x, and size of main diagonal
    auto xDim = ref.dimensions();
    auto nDiag = *(std::min_element(xDim.begin(), xDim.end()));
    // initialize and fill output
    Eigen::Tensor<Scalar, 1> res(nDiag);
    Scalar *diagIt = res.data();
    Scalar *diagEnd = diagIt + nDiag;
    auto indexEnd = xDim.end();
    unsigned long i = 0;
    for(; diagIt != diagEnd; ++diagIt) {
        for(auto index = xDim.begin(); index != indexEnd; ++index)
            *index = i;
        *diagIt = ref.coeff(xDim);
        ++i;
    }
    return res;
}

template<typename Scalar>
Eigen::Tensor<Scalar, 1> nDiag(const Eigen::SparseMatrix<Scalar> &x) {
    Eigen::Tensor<Scalar, 1> res(x.rows());
    auto diagmap = matmap(res);
    diagmap = x.diagonal();
    return(res);
}

template<
    typename SparseCholType = SparseCholesky,
    typename std::enable_if<
        std::is_base_of<
            SparseCholesky,
            SparseCholType
        >::value,
        SparseCholType
    >::type* = nullptr
>
Eigen::Tensor<double, 1> nDiag(const SparseCholType &x) {
    return nDiag(x.R);
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
 * Initialize an Eigen::Tensor object to store the unknown x in the linear
 * system A %*% x = b
 *
 * In this implementation, x is assumed to be a vector because b is templated to
 * be a vector.
 *
 * Assume all matrix/vector dimensions are conformable, s.t. A has dimensions
 * (m x n), x has dimensions (n x p), and b has dimensions (m x p), with p = 1.
 *
 * @tparam Scalar (primitive) type for tensor entries
 */
template<typename Scalar>
Eigen::Tensor<Scalar, 1> initSolveX(
    Eigen::Tensor<Scalar, 2> const & L,
    Eigen::Tensor<Scalar, 1> const & b
) {
    auto Ldim = L.dimensions();
    return Eigen::Tensor<Scalar, 1>(Ldim[1]);
}

/**
 * Initialize an Eigen::Tensor object to store the unknown x in the linear
 * system A %*% x = b
 *
 * In this implementation, x is assumed to be a matrix because b is templated to
 * be a matrix.
 *
 * Assume all matrix/vector dimensions are conformable, s.t. A has dimensions
 * (m x n), x has dimensions (n x p), and b has dimensions (m x p), with p = 1.
 *
 * @tparam Scalar (primitive) type for tensor entries
 */
template<typename Scalar>
Eigen::Tensor<Scalar, 2> initSolveX(
    Eigen::Tensor<Scalar, 2> const & L,
    Eigen::Tensor<Scalar, 2> const & b
) {
    auto Ldim = L.dimensions();
    auto bdim = b.dimensions();
    return Eigen::Tensor<Scalar, 2>(Ldim[1], bdim[1]);
}

/**
*
* Solve the linear system A %*% x = b when RHS represents a matrix or
* a vector, and A is either a lower or upper-triangular matrix
*
* @tparam UPLO Eigen::Lower is A is a lower-triangular matrix, o/w Eigen::Upper
* @tparam LHS An Eigen::Tensor or tensor expression object type
* @tparam RHS An Eigen::Tensor or tensor expression object type
*/
template<int UPLO, typename LHS, typename RHS>
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> triangularsolve(
    const LHS & A,
    const RHS & b
) {
    // explicit Eigen::Tensor types for inputs
    typedef Eigen::Tensor<typename LHS::Scalar, LHS::NumDimensions> LTensor;
    typedef Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> bTensor;
    // evaluate arguments, if necessary
    const auto & A_eval = eval(A);
    const auto & b_eval = eval(b);
    // initialize storage for solution, given problem dimensions
    bTensor res = initSolveX(A_eval, b_eval);
    // map tensor objects to Eigen::Matrix types
    auto Amap = matmap(A_eval);
    auto bmap = matmap(b_eval);
    auto resMap = matmap(res);
    // solve linear system
    resMap = Amap.template triangularView<UPLO>().solve(bmap);
    return res;
}

/**
*
* Solve the lower-triangular system L %*% x = b when RHS represents a matrix or
* a vector
 *
* @tparam LHS An Eigen::Tensor or tensor expression object type
* @tparam RHS An Eigen::Tensor or tensor expression object type
*/
template<typename LHS, typename RHS>
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> forwardsolve(
    const LHS & L,
    const RHS & b
) {
    return triangularsolve<Eigen::Lower>(L, b);
}

/**
*
* Solve the upper-triangular system U %*% x = b when RHS represents a matrix or
* a vector
 *
* @tparam LHS An Eigen::Tensor or tensor expression object type
* @tparam RHS An Eigen::Tensor or tensor expression object type
*/
template<typename LHS, typename RHS>
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> backsolve(
    const LHS & U,
    const RHS & b
) {
    return triangularsolve<Eigen::Upper>(U, b);
}

/**
 * Matrix multiplication x %*% y when both inputs are matrix-like objects, i.e.,
 * rank 2 Eigen::Tensor objects, or Tensor expressions
 *
 * @tparam Xpr
 * @tparam Ypr
 */
template<
    typename Xpr,
    typename Ypr,
    typename std::enable_if<
        (Xpr::NumDimensions == 2) &&
        (Ypr::NumDimensions == 2),
        Xpr
    >::type* = nullptr
>
Eigen::Tensor<typename Xpr::Scalar, 2> nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // initialize output
    auto xdim = xeval.dimensions();
    auto ydim = yeval.dimensions();
    Eigen::Tensor<typename Xpr::Scalar, 2> res(xdim[0], ydim[1]);
    // map tensor objects to Eigen::Matrix types
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    auto resmap = matmap(res);
    // multiply!
    resmap = xmap * ymap;
    return res;
}

/**
 * Matrix multiplication x %*% y representing an inner-product when both inputs
 * are vector-like objects, i.e., rank 1 Eigen::Tensor objects, or Tensor
 * expressions
 *
 * @tparam Xpr
 * @tparam Ypr
 */
template<
    typename Xpr,
    typename Ypr,
    typename std::enable_if<
        (Xpr::NumDimensions == 1) &&
        (Ypr::NumDimensions == 1),
        Xpr
    >::type* = nullptr
>
Eigen::Tensor<typename Xpr::Scalar, 2> nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // initialize output
    Eigen::Tensor<typename Xpr::Scalar, 2> res(1, 1);
    // map tensor objects to Eigen::Matrix types
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    auto resmap = matmap(res);
    // multiply!
    resmap = xmap.transpose() * ymap;
    return res;
}

/**
 * Matrix multiplication x %*% y when x represents a matrix-like object, i.e.,
 * a rank 2 Eigen::Tensor object, or Tensor expression; and y represents a
 * vector-like object , i.e., a rank 1 Eigen::Tensor object, or Tensor
 * expression.
 *
 * The implementation will treat y as a row/col vector, as appropriate, to make
 * the matrix multiplication conformable.
 *
 * @tparam Xpr
 * @tparam Ypr
 */
template<
    typename Xpr,
    typename Ypr,
    typename std::enable_if<
        (Xpr::NumDimensions == 2) &&
        (Ypr::NumDimensions == 1),
        Xpr
    >::type* = nullptr
>
Eigen::Tensor<typename Xpr::Scalar, 2> nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // map tensor objects to Eigen::Matrix types
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    // initialize and map output
    bool as_col_vec = xmap.cols() > 1;
    Eigen::Tensor<typename Xpr::Scalar, 2> res(
        xmap.rows() ,
        as_col_vec ? 1 : ymap.rows()
    );
    auto resmap = matmap(res);
    // multiply!
    if(as_col_vec) {
        resmap = xmap * ymap;
    } else {
        resmap = xmap * ymap.transpose();
    }
    return res;
}

/**
 * Matrix multiplication x %*% y when x represents a vector-like object, i.e.,
 * a rank 1 Eigen::Tensor object, or Tensor expression; and y represents a
 * matrix-like object , i.e., a rank 2 Eigen::Tensor object, or Tensor
 * expression.
 *
 * The implementation will treat x as a row/col vector, as appropriate, to make
 * the matrix multiplication conformable.
 *
 * @tparam Xpr
 * @tparam Ypr
 */
template<
    typename Xpr,
    typename Ypr,
    typename std::enable_if<
        (Xpr::NumDimensions == 1) &&
        (Ypr::NumDimensions == 2),
        Xpr
    >::type* = nullptr
>
Eigen::Tensor<typename Xpr::Scalar, 2> nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // map tensor objects to Eigen::Matrix types
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    // initialize and map output
    bool as_col_vec = ymap.rows() == 1;
    Eigen::Tensor<typename Xpr::Scalar, 2> res(
        as_col_vec ? xmap.rows() : 1,
        ymap.cols()
    );
    auto resmap = matmap(res);
    // multiply!
    if(as_col_vec) {
        resmap = xmap * ymap;
    } else {
        resmap = xmap.transpose() * ymap;
    }
    return res;
}

/**
 * Sparse matrix multiplication x %*% y when first input is matrix-like, i.e.,
 * a rank 2 Eigen::Tensor object, or Tensor expression
 *
 * @tparam Xpr
 */
template<
    typename Xpr,
    typename std::enable_if<
        Xpr::NumDimensions == 2,
        Xpr
    >::type* = nullptr
>
Eigen::Tensor<typename Xpr::Scalar, 2> nMul(
    const Xpr & x, const Eigen::SparseMatrix<typename Xpr::Scalar> & y
) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    // initialize output
    auto xdim = xeval.dimensions();
    Eigen::Tensor<typename Xpr::Scalar, 2> res(xdim[0], y.cols());
    // map tensor objects to Eigen::Matrix types
    auto xmap = matmap(xeval);
    auto resmap = matmap(res);
    // multiply!
    resmap = xmap * y;
    return res;
}

/**
 * Sparse matrix multiplication x %*% y when first input is vector-like, i.e.,
 * a rank 1 Eigen::Tensor object, or Tensor expression
 *
 * @tparam Xpr
 */
template<
    typename Xpr,
    typename std::enable_if<
        Xpr::NumDimensions == 1,
        Xpr
    >::type* = nullptr
>
Eigen::Tensor<typename Xpr::Scalar, 2> nMul(
    const Xpr & x, const Eigen::SparseMatrix<typename Xpr::Scalar> & y
) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    // map tensor objects to Eigen::Matrix types
    auto xmap = matmap(xeval);
    // initialize and map output
    bool as_col_vec = y.rows() == 1;
    Eigen::Tensor<typename Xpr::Scalar, 2> res(
        as_col_vec ? xmap.rows() : 1,
        y.cols()
    );
    auto resmap = matmap(res);
    // multiply!
    if(as_col_vec) {
        resmap = xmap * y;
    } else {
        resmap = xmap.transpose() * y;
    }
    return res;
}

#endif
