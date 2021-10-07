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
                    const OP_&o) -> decltype(a.binaryExpr(b, o)) {
  // Eigen uses the public member function binaryExpr to implement componentwise 
  // binary operations, callable via member functions or overloaded operators.
  return a.binaryExpr(b, o);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> R_op_test(Eigen::Tensor<double, 2> a,
                                   Eigen::Tensor<double, 2> b)  {
  
  // // a binary operator can be passed as a function argument
  // auto ans = R_op_chainable(
  //   a, b, Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  // );
  
  // // Eigen defines many operators in BinaryFunctors.h, and the class def's. 
  // // look simple enough for us to define our own operators
  // auto ans = R_op_chainable(
  //   a, b, Eigen::internal::scalar_product_op<Eigen::Tensor<double, 2>::Scalar>()
  // );
  
  // TODO: demonstrate how to define custom binary operators, such as a 
  //   scalar_power_op class that allows componentwise a^b to be evaluated here.
  
  // chaining operators works!
  auto step1 = R_op_chainable(
    a, b, Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
  auto step2 = R_op_chainable(
    step1, a, Eigen::internal::scalar_sum_op<Eigen::Tensor<double, 2>::Scalar>()
  );
  auto ans = R_op_chainable(
    step1, step2, Eigen::internal::scalar_product_op<Eigen::Tensor<double, 2>::Scalar>()
  );
  
  // TODO: the template parameter Eigen::Tensor<double, 2>::Scalar can be 
  // replaced by "double", but will we need a more general way to pass types?
  // I had expected this information will always be available to us from the 
  // nClass and nFunction declarations in R.
  
  return ans;
}