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
  
#endif
