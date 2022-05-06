#ifndef _NCOMPILER_TENSOR_OPERATIONS
#define _NCOMPILER_TENSOR_OPERATIONS

#define QUOTEME(A) #A
#define PREDEFINED_HEADER(PH) QUOTEME(PH.h)

#ifdef PREDEFINED_test_predefined
#include PREDEFINED_HEADER(PREDEFINED_test_predefined)
std::shared_ptr<test_predefined> make_test_predefined() {
  return(std::shared_ptr<test_predefined>(new test_predefined));
}
#endif

#ifdef PREDEFINED_derivClass
#include PREDEFINED_HEADER(PREDEFINED_derivClass)
std::shared_ptr<derivClass> make_derivClass() {
  return(std::shared_ptr<derivClass>(new derivClass));
}
#endif

#ifdef PREDEFINED_EigenDecomp
#include PREDEFINED_HEADER(PREDEFINED_EigenDecomp)
std::shared_ptr<EigenDecomp> make_EigenDecomp() {
  return(std::shared_ptr<EigenDecomp>(new EigenDecomp));
}
#endif

#ifdef PREDEFINED_SVDDecomp
#include PREDEFINED_HEADER(PREDEFINED_SVDDecomp)
std::shared_ptr<SVDDecomp> make_SVDDecomp() {
    return(std::shared_ptr<SVDDecomp>(new SVDDecomp));
}
#endif


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

// forward declaration of nCompiler struct to store Sparse Chol. decompositions
class SparseCholesky;

/**
 * Template meta programming check to see if Class is an Eigen::SparseCholesky
 *
 * @tparam Class type to inspect
 */
template<typename Class>
struct IsSparseCholesky : std::is_base_of<
    SparseCholesky,
    Class
> { };

/**
 * Template meta programming check to see if Class is an Eigen::SparseMatrix
 *
 * @tparam Class type to inspect
 */
template<typename Class,
         typename HasScalarType = int>
struct IsSparseMatrix : std::false_type { };

template<typename Class>
struct IsSparseMatrix<
    Class,
    decltype(sizeof(typename Eigen::internal::traits<Class>::Scalar), 0)
> : std::is_base_of<
    Eigen::SparseMatrix<typename Eigen::internal::traits<Class>::Scalar>,
    Class
> { };

/**
 * Template meta programming check to see if Class is an Eigen::SparseVector
 *
 * @tparam Class type to inspect
 */
template<typename Class,
         typename HasScalarType = int>
struct IsSparseVector : std::false_type { };

template<typename Class>
struct IsSparseVector<
    Class,
    decltype(sizeof(typename Eigen::internal::traits<Class>::Scalar), 0)
> : std::is_base_of<
    Eigen::SparseVector<typename Eigen::internal::traits<Class>::Scalar>,
    Class
> { };

/**
 * Template meta programming check to see if Class is a sparse Eigen object
 *
 * @tparam Class type to inspect
 */
template<typename Class>
struct IsSparseType : std::conditional<
    IsSparseMatrix<Class>::value || IsSparseVector<Class>::value,
    std::true_type,
    std::false_type
>::type { };

/**
 * Template meta programming check to see if Class is an Eigen::Tensor type.
 *
 * Implementation strategy uses partial specialization with SFINAE in case type
 * Class does not have a member named NumDimensions.  SFINAE is used because
 * Eigen::Tensor is an incomplete type, requiring a scalar type and number of
 * number of dimensions, which may in general be arbitrary.
 *
 * @tparam Class type to inspect
 */
template<typename Class,
         typename HasScalarType = int,
         typename HasNumDimensionsMember = int>
struct IsTensor : std::false_type { };

// partial specialization
template<typename Class>
struct IsTensor<Class,
                decltype(sizeof(Class::Scalar), 0),
                decltype(Class::NumDimensions, 0)> :
std::is_base_of<
  Eigen::Tensor<typename Class::Scalar, Class::NumDimensions>,
  Class
> { };

/**
 * Template meta programming check to see if Class is an Eigen object for which
 * coefficients are immediately accessible, unlike unevaluated Tensor
 * operations.
 *
 * @tparam Class type to inspect
 */
template<typename Class>
struct IsEvaluatedType : std::conditional<
    IsSparseType<Class>::value || IsTensor<Class>::value ||
    IsSparseCholesky<Class>::value || std::is_arithmetic<Class>::value,
    std::true_type,
    std::false_type
>::type { };

/**
 * Template meta programming check to see if Class is an unevaluated Eigen
 * Tensor expression, such as the result of operations like
 * "Eigen::Tensor + Eigen::Tensor".
 *
 * Unevaluated Eigen Tensor expressions are difficult to explicitly identify
 * in template meta programming because unevaluated expressions are template
 * classes which are specialized according to the arguments of the operators.
 * As a result, it is difficult to design a meta programming function that
 * checks Class against explicit Eigen types that represent Tensor expressions.
 * Instead, we deduce that Class is an unevaluated Tensor expression if Class
 * does not match a known, evaluated type used with nCompiler.
 *
 * @tparam Class type to inspect
 */
 template<typename Class>
 struct IsTensorExpression : std::conditional<
     IsEvaluatedType<Class>::value,
     std::false_type,
     std::true_type
 >:: type { };

 /**
  * Returns true if template Class has N dimensions
  *
  * Intended to be used as a template metaprogramming aid.
  *
  * @tparam Class Type to inspect, implicitly restricted to Eigen::Tensor or
  *   TensorExpression types by SFINAE by checking for a NumDimensions member
  * @tparam N Number of dimensions to test for
  */
 template<typename Class, int N>
 constexpr typename std::enable_if<
   IsTensorExpression<Class>::value || IsTensor<Class>::value,
   bool
 >::type
   HasNumDimensionsN() {
     return N == Class::NumDimensions;
   }

 /**
  * Returns true if template Class has N dimensions
  *
  * Intended to be used as a template metaprogramming aid.
  *
  * @tparam Class Type to inspect, restricted to Eigen::SparseMatrix by SFINAE
  * @tparam N Number of dimensions to test for
  */
 template<typename Class, int N>
 constexpr typename std::enable_if<IsSparseMatrix<Class>::value, bool>::type
   HasNumDimensionsN() {
     return N == 2;  // matrices are inherently 2-dimensional
   }

/**
  * Returns true if template Class has N dimensions
  *
  * Intended to be used as a template metaprogramming aid.
  *
  * @tparam Class Type to inspect, restricted to Eigen::SparseVector by SFINAE
  * @tparam N Number of dimensions to test for
  */
 template<typename Class, int N>
 constexpr typename std::enable_if<IsSparseVector<Class>::value, bool>::type
   HasNumDimensionsN() {
     return N == 1;  // vectors are inherently 1-dimensional
   }

/**
 * Implicitly convert a Tensor expression input to an Eigen::Tensor object
 *
 * The compiler uses implicit conversion to decide how to convert the TensorXpr
 * input x to an Eigen::Tensor<Scalar, NumDimensions> object. Naturally, the
 * conversion process allocates memory for the Tensor, then populates the
 * Tensor's entries.
 *
 * Uses SFINAE to restrict the template function (nominally) to only work with
 * Tensor expression inputs vs. with evaluated, concrete Eigen::Tensor objects
 * and other concrete types.
 *
 * If the input is already an Eigen::Tensor object, for example, the desired
 * behavior is to return a reference to the object (implemented in an overloaded
 * function) because the input is already an Eigen::Tensor object, which is the
 * desired output class.  If we were to call this function on an Eigen::Tensor
 * object (i.e., if SFINAE restriction is removed), it would only consume memory
 * and time by creating a copy of an otherwise reasonable input.
 *
 * @tparam TensorXpr Nominally, an unevaluated tensor expression
 */
template<
    typename TensorXpr,
    typename std::enable_if<
        IsTensorExpression<TensorXpr>::value,
        TensorXpr
    >::type* = nullptr
>
Eigen::Tensor<typename TensorXpr::Scalar, TensorXpr::NumDimensions> eval(
    TensorXpr & x
) {
    return x;
}

/**
 * Return a reference to an Eigen::Tensor object, or concrete Eigen type,
 * avoiding copying the input
 *
 * Uses SFINAE to restrict the template function to only work with Eigen::Tensor
 * inputs and other types with concrete data storage, vs. with Tensor expression
 * objects.
 *
 * If the input is a Tensor expression object, the desired behavior is to return
 * an Eigen::Tensor object with the results of the evaluated Tensor expression
 * (implemented in an overloaded function).  Calling this function on a Tensor
 * expression object (i.e., if SFINAE restriction is removed) will cause type
 * errors when eval() is nested within other functions that use Tensors, but
 * require evaluated inputs, such as wrappers for matrix multiplication and
 * linear solvers.
 *
 * @tparam Xpr An Eigen::Tensor or other concrete object type
 */
template<
    typename Xpr,
    typename std::enable_if<
    IsEvaluatedType<Xpr>::value,
        Xpr
    >::type* = nullptr
>
Xpr& eval(Xpr & x) {
    return x;
}

/**
 * Constant version of eval() with Eigen::Tensor and other concrete inputs.
 *
 * See documentation for eval() with Eigen::Tensor inputs for more details.
 *
 * @tparam Xpr An Eigen::Tensor or other concrete object type
 */
template<
    typename Xpr,
    typename std::enable_if<
        IsEvaluatedType<Xpr>::value,
        Xpr
    >::type* = nullptr
>
const Xpr& eval(const Xpr & x) {
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
 * "Passthrough" for a const Eigen::SparseMatrix object
 *
 * @tparam Scalar (primitive) type for matrix entries
 */
 template<typename Scalar>
Eigen::SparseMatrix<Scalar>& matmap(Eigen::SparseMatrix<Scalar> & x) {
    return x;
}

 /**
 * Constant "passthrough" for a const Eigen::SparseMatrix object
 *
 * @tparam Scalar (primitive) type for matrix entries
 */
 template<typename Scalar>
 const Eigen::SparseMatrix<Scalar>& matmap(
     const Eigen::SparseMatrix<Scalar> & x
 ) {
     return x;
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
    const auto xEval = eval(x);
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
        IsTensorExpression<TensorExpr>::value || IsTensor<TensorExpr>::value,
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
 * Generate a rank-2 Eigen::Tensor object (i.e., a matrix) with constant
 * diagonal.  Uses SFINAE to restrict usage to template arguments Scalar, which
 * are a basic numeric type.
 *
 * @tparam Scalar (primitive) type for Tensor entries
 * @tparam Index (primitive) type for Tensor dimension values
 *
 * @param x Constant value for diagonal entries
 * @param nrow Number of rows for Tensor output
 * @param ncol Number of columns for Tensor output
 */
 template<
   typename Scalar,
   typename Index,
   typename std::enable_if<
     std::is_arithmetic<Scalar>::value
   >::type* = nullptr
 >
 Eigen::Tensor<Scalar, 2> nDiag(Scalar x, Index nrow, Index ncol) {
    // initialize output
    Eigen::Tensor<Scalar, 2> res(nrow, ncol);
    // zero-initialize tensor contents
    res.setZero();
    // figure out how large the main diagonal is
    Index nEntries = std::min(nrow, ncol);
    // populate diagonal and return
    for(Index i = 0; i < nEntries; ++i) {
        res(i,i) = x;
    }
    return res;
 }

 /**
 * Generate a rank-2 Eigen::Tensor object (i.e., a matrix) with non-constant
 * diagonal
 *
 * @tparam Xpr Eigen::Tensor or Tensor expression type with diagonal entries
 * @tparam Scalar (primitive) type for Tensor entries
 * @tparam Index (primitive) type for Tensor dimension values
 *
 * @param x Vector of values for diagonal entries
 * @param nrow Number of rows for Tensor output
 * @param ncol Number of columns for Tensor output
 */
 template<
    typename Xpr,
    typename Index,
    typename Scalar = typename Xpr::Scalar,
    typename std::enable_if<
        (IsTensor<Xpr>::value || IsTensorExpression<Xpr>::value) &&
        HasNumDimensionsN<Xpr, 1>(),
        Xpr
    >::type* = nullptr
>
Eigen::Tensor<Scalar, 2> nDiag(Xpr x, Index nrow, Index ncol) {
    // evaluate input if needed
    auto xEval = eval(x);
    // initialize output
    Eigen::Tensor<Scalar, 2> res(nrow, ncol);
    // zero-initialize tensor contents
    res.setZero();
    // figure out how large the main diagonal is
    Index nEntries = std::min(nrow, ncol);
    if(x.size() != nEntries) {
        throw std::range_error(
            "nCompiler::nDiag - Diagonal entry vector length does not match matrix size"
        );
    }
    // populate diagonal and return
    for(Index i = 0; i < nEntries; ++i) {
        res(i,i) = xEval(i);
    }
    return res;
 }

 /**
 * Generate a Sparse matrix with constant diagonal.  Uses SFINAE to restrict
 * usage to template arguments Scalar, which are a basic numeric type.
 *
 * @tparam Scalar (primitive) type for matrix entries
 * @tparam Index (primitive) type for matrix dimension values
 *
 * @param x Constant value for diagonal entries
 * @param nrow Number of rows for matrix output
 * @param ncol Number of columns for matrix output
 */
 template<
   typename Scalar,
   typename Index,
   typename std::enable_if<
     std::is_arithmetic<Scalar>::value
   >::type* = nullptr
 >
 Eigen::SparseMatrix<Scalar> nDiagonal(Scalar x, Index nrow, Index ncol) {
    // initialize output
    Eigen::SparseMatrix<Scalar> res(nrow, ncol);
    // figure out how large the main diagonal is
    Index nEntries = std::min(nrow, ncol);
    // populate diagonal and return
    for(Index i = 0; i < nEntries; ++i) {
        res.coeffRef(i,i) = x;
    }
    return res;
 }

 /**
 * Generate a Sparse matrix with non-constant diagonal
 *
 * @tparam Xpr Eigen::Tensor or Tensor expression type with diagonal entries
 * @tparam Scalar (primitive) type for matrix entries
 * @tparam Index (primitive) type for matrix dimension values
 *
 * @param x Vector of values for diagonal entries
 * @param nrow Number of rows for matrix output
 * @param ncol Number of columns for matrix output
 */
 template<
    typename Xpr,
    typename Index,
    typename Scalar = typename Xpr::Scalar,
    typename std::enable_if<
        (IsTensor<Xpr>::value || IsTensorExpression<Xpr>::value) &&
        HasNumDimensionsN<Xpr, 1>(),
        Xpr
    >::type* = nullptr
>
Eigen::SparseMatrix<Scalar> nDiagonal(Xpr x, Index nrow, Index ncol) {
    // evaluate input if needed
    auto xEval = eval(x);
    // initialize output
    Eigen::SparseMatrix<Scalar> res(nrow, ncol);
    // figure out how large the main diagonal is
    Index nEntries = std::min(nrow, ncol);
    if(x.size() != nEntries) {
        throw std::range_error(
            "nCompiler::nDiag - Diagonal entry vector length does not match matrix size"
        );
    }
    // populate diagonal and return
    for(Index i = 0; i < nEntries; ++i) {
        res.coeffRef(i,i) = xEval(i);
    }
    return res;
 }

 /**
  * DiagIO is a templated class that uses partial specialization and
  * SFINAE to implement a family of specialized template classes facilitating
  * assignment and extraction of diagonal entries for object types that
  * nCompiler uses to store matrix-like data.  For example, I/O is supported for
  * Eigen::Tensor<Scalar, 2> types, Eigen Tensor expressions of such objects,
  * Eigen::SparseMatrix types, and others.
  *
  * The design goal is that the target object will be wrapped by a specialized
  * DiagIO class that uses conversion operators and overloaded
  * assignment operators to provide additional functionality (i.e., assignment
  * and extraction of diagonal entries) in a way that is compatible with
  * nCompiler/R-like syntax.  Without the added flexibility, nCompiler would
  * need to perform additional, possibly complex, modification of an nFunction's
  * AST in order to change R-like syntax into Eigen-like syntax.
  *
  * @tparam xType The type of the target object to be manipulated
  * @tparam Enable template parameter used by SFINAE to allow the compiler to
  *   select a partially specialized class that will have appropriate
  *   syntax/method implementations for manipulating objects of type xType
  */
 template<typename xType, typename Enable = void>
 class DiagIO { };

 /**
  * Partial specialization of DiagIO designed to work with
  * Eigen::Tensor<Scalar, 2> objects or Eigen Tensor expressions of those same
  * types of objects.
  *
  * @tparam xType an Eigen::Tensor<Scalar, 2> type, or Expression of Tensor
  *   objects with 2 dimensions.
  */
 template<typename xType>
 class DiagIO<
    xType,
    typename std::enable_if<
        (IsTensorExpression<xType>::value || IsTensor<xType>::value) &&
        HasNumDimensionsN<xType, 2>(),
        void
    >::type
 > {

    private:

        // wrapped object
        xType &x;

        // data storage type
        typedef typename xType::Scalar Scalar;
        // tensor type associated with wrapped object
        typedef Eigen::Tensor<Scalar, xType::NumDimensions> TensorType;
        // type associate with tensor indices and coordinate objects
        typedef typename Eigen::TensorRef<TensorType>::Index Index;
        typedef typename Eigen::TensorRef<TensorType>::Dimensions Dimensions;

        // access elements of x without fully evaluating x if a tensor object
        Eigen::TensorRef<TensorType> xref;
        // coordinate object to access elements of x
        Dimensions xcoord;
        // number of elements in main diagonal
        Index nDiag;

        /**
         * Point coordinate object to the i^th diagonal element of wrapped obj.
         */
        Dimensions& diagElem(Index i) {
            auto indexEnd = xcoord.end();
            for(auto val = xcoord.begin(); val != indexEnd; ++val)
                *val = i;
            return xcoord;
        }

    public:

        explicit DiagIO(xType & tgt) : x(tgt), xref(x),
            xcoord(xref.dimensions()),
            nDiag(*(std::min_element(xcoord.begin(), xcoord.end()))) { };

        /**
         * replace diagonal of wrapped object with contents of a tensor
         */
         template<
             typename Xpr,
             typename std::enable_if<
                 (IsTensorExpression<Xpr>::value || IsTensor<Xpr>::value) &&
                 HasNumDimensionsN<Xpr, 1>(),
                 Xpr
             >::type* = nullptr
         >
         DiagIO& operator=(const Xpr& v) {
             auto veval = eval(v);
             if(veval.size() != nDiag) {
                 throw std::range_error(
                     "nCompiler::DiagIO - Replacement diagonal entry vector length does not match matrix size"
                 );
             }
             Scalar *vScalar = veval.data();
             for(auto i = 0; i < nDiag; ++i)
                 xref.coeffRef(diagElem(i)) = *(vScalar++);
            return *this;
         }

        /**
         * replace diagonal of wrapped object with a constant value
         */
        DiagIO& operator=(const Scalar& cst) {
            for(auto i = 0; i < nDiag; ++i)
                xref.coeffRef(diagElem(i)) = cst;
            return *this;
        }

        /**
         * use explicit conversion to extract diagonal from wrapped object
         */
        explicit operator Eigen::Tensor<Scalar, 1> ()  {
            Eigen::Tensor<Scalar, 1> res(nDiag);
            Scalar *diagIt = res.data();
            for(auto i = 0; i < nDiag; ++i)
                *(diagIt++) = xref.coeff(diagElem(i));
            return res;
        }
};

template<typename xType>
 class DiagIO<
    xType,
    typename std::enable_if<
        IsSparseMatrix<xType>::value,
        void
    >::type
 > {

    private:

        // wrapped object
        xType &x;

        // data storage type
        typedef typename xType::Scalar Scalar;
        // type associate with tensor indices and coordinate objects
        typedef typename xType::StorageIndex StorageIndex;

        // number of elements in main diagonal
        StorageIndex nDiag;

    public:

        explicit DiagIO(xType & tgt) : x(tgt),
            nDiag(std::min(x.rows(), x.cols())) { };

        /**
         * replace diagonal of wrapped object with contents of a tensor
         */
         template<
             typename Xpr,
             typename std::enable_if<
                 (IsTensorExpression<Xpr>::value || IsTensor<Xpr>::value) &&
                 HasNumDimensionsN<Xpr, 1>(),
                 Xpr
             >::type* = nullptr
         >
         DiagIO& operator=(const Xpr& v) {
             auto veval = eval(v);
             if(veval.size() != nDiag) {
                 throw std::range_error(
                     "nCompiler::DiagIO - Replacement diagonal entry vector length does not match matrix size"
                 );
             }
             Scalar *vScalar = veval.data();
             for(auto i = 0; i < nDiag; ++i)
                 x.coeffRef(i,i) = *(vScalar++);
            return *this;
         }

        /**
         * replace diagonal of wrapped object with a constant value
         */
         DiagIO& operator=(const Scalar& cst) {
             for(auto i = 0; i < nDiag; ++i)
                 x.coeffRef(i,i) = cst;
             return *this;
         }

        /**
        * explicit conversion to extract diagonal from an Eigen::SparseMatrix
        */
        explicit operator Eigen::Tensor<Scalar, 1> ()  {
         Eigen::Tensor<Scalar, 1> res(x.rows());
         auto diagmap = matmap(res);
         diagmap = x.diagonal();
         return(res);
     }
};

/**
 * Pass assignment and extraction of diagonal entries to instantiations of a
 * partial specialization of the template class DiagIO.  SFINAE will determine
 * which partial specialization to use during compilation.
 */
template<typename xType>
auto nDiag(xType & x) -> decltype(
    DiagIO<xType>(x)
) {
    return DiagIO<xType>(x);
}

/**
 * Required to support composition of diagonal operator, i.e., nDiag(nDiag(...))
 * because the output of the nested nDiag(...) will be an rvalue argument into
 * the outer nDiag( ) call.  As an rvalue argument, the output of
 * nDiag(...) will not be able to be used with the previous implementation of
 * the outer function, with signature nDiag(xType & x) for lvalue args.
 */
template<
    typename xType,
    class = typename std::enable_if<
        !std::is_lvalue_reference<xType>::value
    >::type
>
auto nDiag(xType && x) -> decltype(
    DiagIO<xType>(x)
) {
    return DiagIO<xType>(x);
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
 * Solve the linear system A %*% x = b when RHS represents a matrix or a vector,
 * and A is a square matrix
 *
 * @tparam LHS An Eigen::Tensor or tensor expression object type
 * @tparam RHS An Eigen::Tensor or tensor expression object type
 */
template<typename LHS, typename RHS>
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> nSolve(
    const LHS & A, const RHS & b
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
    resMap = Amap.partialPivLu().solve(bmap);
    return res;
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
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> nForwardsolve(
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
Eigen::Tensor<typename RHS::Scalar, RHS::NumDimensions> nBacksolve(
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
        HasNumDimensionsN<Xpr, 2>() && HasNumDimensionsN<Ypr, 2>(),
        Xpr
    >::type* = nullptr,
    typename ResultType = typename std::conditional<
        IsSparseType<Xpr>::value && IsSparseType<Ypr>::value,
        Eigen::SparseMatrix<typename Xpr::Scalar>,
        Eigen::Tensor<typename Xpr::Scalar, 2>
    >::type
>
ResultType nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // map inputs and initialize output
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    ResultType res(xmap.rows(), ymap.cols());
    // map and multiply!
    matmap(res) = xmap * ymap;
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
        HasNumDimensionsN<Xpr, 1>() && HasNumDimensionsN<Ypr, 1>(),
        Xpr
    >::type* = nullptr,
    typename ResultType = typename std::conditional<
        IsSparseType<Xpr>::value && IsSparseType<Ypr>::value,
        Eigen::SparseMatrix<typename Xpr::Scalar>,
        Eigen::Tensor<typename Xpr::Scalar, 2>
    >::type
>
ResultType nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // map inputs and initialize output
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    ResultType res(1, 1);
    // map and multiply!
    matmap(res) = xmap.transpose() * ymap;
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
        HasNumDimensionsN<Xpr, 2>() && HasNumDimensionsN<Ypr,1>(),
        Xpr
    >::type* = nullptr,
    typename ResultType = typename std::conditional<
        IsSparseType<Xpr>::value && IsSparseType<Ypr>::value,
        Eigen::SparseMatrix<typename Xpr::Scalar>,
        Eigen::Tensor<typename Xpr::Scalar, 2>
    >::type
>
ResultType nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // map inputs
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    // initialize output
    bool as_col_vec = xmap.cols() > 1;
    ResultType res(xmap.rows(), as_col_vec ? 1 : ymap.rows());
    // map and multiply!
    if(as_col_vec) {
        matmap(res) = xmap * ymap;
    } else {
        matmap(res) = xmap * ymap.transpose();
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
        HasNumDimensionsN<Xpr, 1>() && HasNumDimensionsN<Ypr, 2>(),
        Xpr
    >::type* = nullptr,
    typename ResultType = typename std::conditional<
        IsSparseType<Xpr>::value && IsSparseType<Ypr>::value,
        Eigen::SparseMatrix<typename Xpr::Scalar>,
        Eigen::Tensor<typename Xpr::Scalar, 2>
    >::type
>
ResultType nMul(const Xpr & x, const Ypr & y) {
    // evaluate arguments, if necessary
    const auto & xeval = eval(x);
    const auto & yeval = eval(y);
    // map inputs
    auto xmap = matmap(xeval);
    auto ymap = matmap(yeval);
    // initialize and map output
    bool as_col_vec = ymap.rows() == 1;
    ResultType res(as_col_vec ? xmap.rows() : 1, ymap.cols());
    // map and multiply!
    if(as_col_vec) {
        matmap(res) = xmap * ymap;
    } else {
        matmap(res) = xmap.transpose() * ymap;
    }
    return res;
}

// This is drafted but not yet used.
template<typename Scalar >
bool nIsSymmetric(const Eigen::Tensor<Scalar, 2> &x) {
  double tol = 100. * DBL_EPSILON; // copied from default of R's isSymmetric 
  // could add "tol1"-like behavior. See help(isSymmetric)
  // Could possibly use stl to search more efficiently
  int nrows(x.dimensions()[0]);
  int ncols(x.dimensions()[1]);
  for(int i = 0; i < nrows; i++){
    for(int j = i + 1; j < ncols; j++){
      if(x(i,j) != x(j,i)){
	return(false);
      }
    }
  }
  return(true);
}

#ifdef PREDEFINED_SVDDecomp
std::shared_ptr<SVDDecomp> nSvd(
    const Eigen::Tensor<double, 2> &x, int vectors
) {
    auto xm = matmap(x);
    std::shared_ptr<SVDDecomp> ans(new SVDDecomp);

    int n = xm.rows();
    int p = xm.cols();
 	int nu = std::min(n, p);

 	Eigen::JacobiSVD<Eigen::MatrixXd> svd;

 	/* note: if nu > 16, bidiagonialization algo. is recommended on eigen
 	   website.  not currently available w/ nimble's version of eigen, but may
 	   be in future. */
 	if(vectors == 0) {
 	    svd.compute(xm);
 	}
 	else {
 	    int leftSVs = nu;
 	    int rightSVs = nu;

 	    if(vectors == 1) {
 	        svd.compute(xm, Eigen::ComputeThinU | Eigen::ComputeThinV);
 	    }
 	    if(vectors == 2) {
 	        leftSVs = xm.rows();
 	        rightSVs = xm.cols();
 	        svd.compute(xm, Eigen::ComputeFullU | Eigen::ComputeFullV);
 	    }

 	    ans->u.resize({{n, leftSVs}});
 	    auto u = matmap(ans->u);
 	    u = svd.matrixU();

 	    ans->v.resize({{p, rightSVs}});
 	    auto v = matmap(ans->v);
        v = svd.matrixV();
 	}

    ans->d.resize(nu);
 	auto d = matmap(ans->d);
 	d = svd.singularValues();

 	return ans;
}
#endif

#ifdef PREDEFINED_EigenDecomp
  std::shared_ptr<EigenDecomp> nEigen(const Eigen::Tensor<double, 2> &x, bool symmetric = true, bool valuesOnly = false) {
    auto x_map = matmap(x);
    int nrows(x_map.rows());
    int ncols(x_map.cols());
    // potentially error-trap of nrows == ncols.
    std::shared_ptr<EigenDecomp> ans(new EigenDecomp);
    ans->values.resize({{nrows}});
    auto values_map = matmap(ans->values);
    if(!valuesOnly){
      ans->vectors.resize({{nrows, ncols}});
    }
    Eigen::DecompositionOptions eigOpts = valuesOnly ? Eigen::EigenvaluesOnly : Eigen::ComputeEigenvectors;
    if(symmetric) {
      Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> solver1(x_map, eigOpts);
      values_map = solver1.eigenvalues().reverse();
      if(!valuesOnly){
	Eigen::Map<Eigen::MatrixXd> vecs_map = matmap(ans->vectors);
	vecs_map = solver1.eigenvectors().rowwise().reverse();	
      }
    } else {
      Eigen::EigenSolver<Eigen::MatrixXd> solver2(x_map, eigOpts);
      std::vector<std::pair<double,int> > sortIndices;
      for(int i = 0; i < nrows; i++){
	sortIndices.push_back(std::make_pair(abs(solver2.eigenvalues().real()(i)),i));
      }
      std::sort(sortIndices.begin(), sortIndices.end());
      std::reverse(sortIndices.begin(), sortIndices.end());

      for(int i = 0; i < nrows ; ++i){
	if(solver2.eigenvalues().imag()(sortIndices[i].second) != 0){
	  // emit error message like thsi from nimble
	  // _nimble_global_output <<"Run-time warning: matrix used in call to nimEigen() has a complex valued eigenvector."<<"\n"; nimble_print_to_R(_nimble_global_output);
	  values_map(i) = NAN;
	} else {
	  values_map(i) = solver2.eigenvalues().real()(sortIndices[i].second);
	}
      }
      if(!valuesOnly){
	Eigen::Map<Eigen::MatrixXd> vecs_map = matmap(ans->vectors);
	for(int i = 0; i < ncols; i++){
	  vecs_map.col(i) = solver2.eigenvectors().real().col(sortIndices[i].second);
	  for(int j = 0; j < nrows; j++){
	    if(solver2.eigenvectors().imag()(j, sortIndices[i].second) != 0){
	      // emit warning something like this from nimble:
	      // _nimble_global_output <<"Run-time warning: matrix used in call to nimEigen() has a complex valued eigenvector."<<"\n"; nimble_print_to_R(_nimble_global_output);
	      vecs_map(j, i) = NAN;
	    }
	  }
	}
      }
    }
    return ans;
  }
#endif
  
#endif
