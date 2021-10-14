#include "preamble.h"

using Eigen::Tensor;
using Eigen::DefaultDevice;
using std::cout;
using std::endl;

// The input types, A_ and B_, could themselves be (Eigen) ops of ops of ops.
// Eigen's design is that results of (componentwise binary) operations on a 
// Tensor are returned as a subclass of the main tensor class, 
// TensorBase<Derived, ReadOnlyAccessors>.  If A_ or B_ is an op of an op, then 
// the subclass template argument Derived will be 
// TensorBase<X, ReadOnlyAccessors>, and X will grow as the number of nested ops 
// grows.

// The code below lets arbitrary operators be processed.

// TODO: How to handle arbitrary tensors of unequal dimensions?  What is the 
// right code design?  For example, a tensor A with dimension sizes (1,3,2) 
// will have 6 entries, while a tensor B with dimension sizes (6,3,8,9,3) will 
// have 3,888 entries.  How/should these be combined?  See array_munging.R
// for a review of what R supports.

// TODO: Is the code pattern below suitable for nCompiler?

/**
 * Perform an arbitrary binary componentwise operator on Eigen tensor objects.
 * 
 * The template function relies on automatic template deduction to allow the 
 * function to be called recursively without explicitly needing to declare 
 * object types.
 * 
 * @param a In practice, an object that subclasses 
 *   Eigen::TensorBase<Derived, ReadOnlyAccessors>.  With the way Eigen is 
 *   built, the subclass may be an explicit tensor (i.e., structure of numbers)
 *   or a set of operations on explicit tensors.
 * @param b Same as a
 * @param o In practice, an object that subclasses 
 *   Eigen::internal::binary_op_base<LhsScalar,RhsScalar>.
 */


template<typename OP_, typename A_, typename B_>
auto R_op_chainable(const A_ &a, 
                    const B_&b, 
                    const OP_&o) -> decltype(
    a.binaryExpr(
      b.reshape(
        Eigen::TensorRef<Eigen::Tensor<typename A_::Scalar, A_::NumDimensions>>(a).dimensions()
      ), 
    o)
  ) {
                      
  Eigen::TensorRef<Eigen::Tensor<typename A_::Scalar, A_::NumDimensions>> aEval(a);
  auto aDim = aEval.dimensions();
  auto y = b.reshape(aDim);
  
  return a.binaryExpr(y, o);
}