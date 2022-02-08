
using Eigen::Tensor;
using Eigen::DefaultDevice;
using std::cout;
using std::endl;

// This is used from Rplus_test1.cpp.

// Rplus_ gives a bare bones illustration of accessing dimensions in Eigen.
// We are using Eigen's Tensor support, which is under the "unsupported" directory
// but is in fact highly developed.  I understand it is used for CPU evaluation in TensorFlow,
// but I don't know any details of that.

// The input types, A_ and B_, could themselves be (Eigen) ops of ops of ops.
// Eigen's design is that a Derived type inherits from TensorBase<Derived> using CRTP.
// class Tensor is itself a TensorBase<Tensor> and provides actual numbers.
// Evaluation of ops is triggered by operator=.
// This is done by a TensorEvaluator, which in turn uses a TensorExecutor.
// From studying Eigen source code, the TensorEvaluator is the "correct"
// way to get to dimension information.
// The number of dimensions is known at compile time, while the dimensions()
// are at run-time.

// In this example, nothing is done with the dimensions except to print them.

// One general coding question will be what is a clean/elegant way to do the same
// dimension/size handling for multiple operators (e.g. component-wise +, -, /, *, &&, >, etc.

template<typename A_, typename B_>
auto Rplus_(const A_ &a,
	    const B_ &b) -> decltype( a + b ) {
  int aNumDimensions = A_::NumDimensions; // Number of dimensions is known at compile time.
  int bNumDimensions = B_::NumDimensions;

  auto aEval = Eigen::TensorEvaluator<A_, DefaultDevice>(typename A_::Nested(a), Eigen::DefaultDevice()); // Evaluator object
  auto bEval = Eigen::TensorEvaluator<B_, DefaultDevice>(typename B_::Nested(b), Eigen::DefaultDevice());

  cout<<"Showing sizes of a for "<<aNumDimensions<<" dimensions"<<endl;
  auto const &aDim = aEval.dimensions(); // Evaluator gives access to dimensions (sizes) vector (not necessarily std::vector)
  for(int i = 0; i < aNumDimensions; i++) {
    cout << aDim[i];
  }
  cout<<endl;

  cout<<"Showing sizes of b for "<<bNumDimensions<<" dimensions"<<endl;
  auto const &bDim = bEval.dimensions();
  for(int i = 0; i < bNumDimensions; i++) {
    cout << bDim[i];
  }
  cout<<endl;
  
  auto ans = a + b; // Here is the actual op.
  return ans;
}

// First sketch of what "smart" matrix + vector could look like.
// If the matrix has one row or one column, treat the vector as the same-shape matrix.
// That imitates R to a point.
// It does not imitate recycling-rule behavior.  Right now let's not support that.
//
// It would be nice to get more of the type determination automatic or at least organized into structs or some other more compact/elegant/readable code

// This is the full, explicit return type:
// const Eigen::TensorCwiseBinaryOp<Eigen::internal::scalar_sum_op<typename A_::Scalar>, const A_, const Eigen::TensorReshapingOp<const typename Eigen::TensorEvaluator<A_, DefaultDevice>::Dimensions, const B_>>

template<typename A_, typename B_>
  auto Rplus_2_1_(const A_ &a,
		  const B_ &b) -> decltype(a + b.reshape( Eigen::TensorEvaluator<A_, DefaultDevice>(typename A_::Nested(a), Eigen::DefaultDevice()).dimensions())) {  
  //  static int aNumDimensions = A_::NumDimensions; // Number of dimensions is known at compile time.
  //  static int bNumDimensions = B_::NumDimensions;
  // We could do compile-time check of number of dimensions here
  
  auto const aDim = Eigen::TensorEvaluator<A_, DefaultDevice>(typename A_::Nested(a), Eigen::DefaultDevice()).dimensions(); // Evaluator object
  auto const bDim = Eigen::TensorEvaluator<B_, DefaultDevice>(typename B_::Nested(b), Eigen::DefaultDevice()).dimensions();

  // We could do run-time check of dimensions (sizes) here.
  
  auto bMat = b.reshape(aDim); // Here is the reshaping op, assuming dimensions are ok (number of rows or columns in a is 1)
  auto ans = a + bMat;         // Here is the actual op.

  // One thing not to do:
  // Eigen::Tensor<double, 2> answer;
  // answer = a + bMat;
  // This defeats Eigen's efficiency by forcing evaluation and creation of the
  // temporary variable answer.

  // Another thing not to do:
  //  if( b_has_one_column ) {
  //   ans = one_eigen_operation(a, b);
  //} else {
  //  ans = another_eigen_operation(a, b.reshape(new_dim));
  // }
  // because every Eigen operation returns a new templated type!
  // The two ans's likely would end up as different templated types.
  
  return ans;
}
