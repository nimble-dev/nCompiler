#include <RcppEigen.h>
#include <nCompiler/nCompiler_Eigen.h>
#include <nCompiler/tensorOperations.h>

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(nCompiler)]]


/***************************************/

using std::cout;
using std::endl;

// warm-up function
// [[Rcpp::export]]
void hw(
        Eigen::Tensor<double, 1> x
) {
  std::cout<<"hello world.  first element is "<<x(0)<<std::endl;
}

// "Single" below means there is only one operation, not nested operations.

///////////////////////
// SINGLE CHIPS
// Read from single chip
// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex1(Eigen::Tensor<double, 2> x) {
  return x.chip(2, 0);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex1b(Eigen::Tensor<double, 2> x) {
//  return nCompiler::IndexByScalar<0>().op(2, x); // same as x.chip<0>(2)
  // Try macro version
  return ISINGLE_(0, 2, x);
}

// Assign to single chip
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex2(Eigen::Tensor<double, 2> x, Eigen::Tensor<double, 1> v) {
  x.chip(2, 0) = v;
  return x;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex2b(Eigen::Tensor<double, 2> x, Eigen::Tensor<double, 1> v) {
//  nCompiler::IndexByScalar<0>(2).op(x) = v;
  nCompiler::IndexByScalar<0>().op(2, x) = v;
  return x;
}


////////////////////////
// CHIP OF CHIP
// Access
// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex1p1(Eigen::Tensor<double, 3> x) {
  // operations are left to right.
  return x.chip<2>(2).chip<1>(3);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex1p1b(Eigen::Tensor<double, 3> x) {
  // operations are inside (right) to outside (left)
  return nCompiler::IndexByScalar<1>().op(3, nCompiler::IndexByScalar<2>().op(2, x));
}

// Assign
// [[Rcpp::export]]
Eigen::Tensor<double, 3> ex2p1(Eigen::Tensor<double, 3> x, Eigen::Tensor<double, 1> v) {
  // operations are left to right.
  x.chip<2>(2).chip<1>(3) = v;
  return x;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3> ex2p1b(Eigen::Tensor<double, 3> x, Eigen::Tensor<double, 1> v) {
  // operations are left to right.
  // Try Macro version of this one
  //  nCompiler::IndexByScalar<1>().op(3, nCompiler::IndexByScalar<2>().op(2, x)) = v;
  ISINGLE_(1, 3, ISINGLE_(2, 2, x))=v;
  return x;
}

// using nCompiler::BaseType;

// // MakeTensorRef returns a TensorRef around an operator that allows
// // singleton access and assignment.  For access, follow by () of .coeff().
// // For assignment, follow by .coeffRef() = value.
// template<class T >
// auto MakeTensorRef(T &&x) ->   Eigen::TensorRef<
//     Eigen::Tensor<typename BaseType<T>::type::Scalar,
//                   BaseType<T>::type::NumDimensions>
//     >
//  {
//   return Eigen::TensorRef<
//     Eigen::Tensor<typename BaseType<T>::type::Scalar,
//                   BaseType<T>::type::NumDimensions>
//     >(std::forward<T>(x));
// }

////////////////////////
// SCALAR ELEMENT OF CHIP OF CHIP
// [[Rcpp::export]]
double ex1p2(Eigen::Tensor<double, 3> x) {
  // operations are left to right.
  // ANY OF THESE WORK.
  //  return MakeTensorRef(x.chip<2>(2).chip<1>(3))(1);
  return nCompiler::MakeTensorRef(x.chip<2>(2).chip<1>(3)).coeff(1);
  //return MakeTensorRef(x.chip<2>(2).chip<1>(3)).coeffRef(1);
}

// [[Rcpp::export]]
double ex1p2b(Eigen::Tensor<double, 3> x) {
  // operations are left to right.
  // ANY OF THESE WORK.
  //  return MakeTensorRef(x.chip<2>(2).chip<1>(3))(1);
  //return MakeTensorRef(nCompiler::IndexByScalar<1>(3).op(nCompiler::IndexByScalar<2>(2).op(x))).coeff(1);
  //return MakeTensorRef(x.chip<2>(2).chip<1>(3)).coeffRef(1);
  return nCompiler::MakeTensorRef(nCompiler::IndexByScalar<1>().op(3, nCompiler::IndexByScalar<2>().op(2, x))).coeff(1);
}


// [[Rcpp::export]]
Eigen::Tensor<double, 3> ex2p2(Eigen::Tensor<double, 3> x, double v) {
  // operations are left to right.
  // For some reason operator() does not seem to be adequately overloaded for this
  nCompiler::MakeTensorRef(x.chip<2>(2).chip<1>(3)).coeffRef(1) = v;
  return x;
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3> ex2p2b(Eigen::Tensor<double, 3> x, double v) {
  // operations are left to right.
  // For some reason operator() does not seem to be adequately overloaded for this
//  MakeTensorRef(nCompiler::IndexByScalar<1>(3).op(nCompiler::IndexByScalar<2>(2).op(x))).coeffRef(1) = v;
  nCompiler::MakeTensorRef(nCompiler::IndexByScalar<1>().op(3, nCompiler::IndexByScalar<2>().op(2, x))).coeffRef(1) = v;
  return x;
}

////////////////////////
// Single indexing vectors
// Read from single indexing vector
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex3(Eigen::Tensor<double, 2> x, Eigen::Tensor<int, 1> iv) {
  return nCompiler::IndexByVec<0>().op(iv,x);
}

// Assign via single indexing vector
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex4(Eigen::Tensor<double, 2> x, Eigen::Tensor<int, 1> iv, Eigen::Tensor<double, 2> v) {
  nCompiler::IndexByVec<0>().op(iv,x) = v;
  return x;
}

///////////////////////////
// INDEX VEC OF INDEX VEC
// Read from index vec of index vec
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex3p1(Eigen::Tensor<double, 2> x,
                               Eigen::Tensor<int, 1> iv,
                               Eigen::Tensor<int, 1> iv2) {
  return nCompiler::IndexByVec<1>().op(iv2, nCompiler::IndexByVec<0>().op(iv,x));
}

// Assign to index vec of index vec
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex4p1(Eigen::Tensor<double, 2> x,
                               Eigen::Tensor<int, 1> iv,
                               Eigen::Tensor<int, 1> iv2,
                               Eigen::Tensor<double, 2> v) {
  // TRY MACRO VERSION
  // nCompiler::IndexByVec<1>().op(iv2, nCompiler::IndexByVec<0>().op(iv,x)) = v;
  IVEC_(1, iv2, IVEC_(0, iv, x)) = v;
  return x;
}

/////////////////////////////////////
// INDEX VEC OF INDEX SCALAR AND VICE VERSA
// Read
// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex3p2(Eigen::Tensor<double, 2> x,
                               Eigen::Tensor<int, 1> iv) {
  // x[2, iv] // not outer DimId 0 is after removing first dim
  return nCompiler::IndexByVec<0>().op(iv, nCompiler::IndexByScalar<0>().op(1,x));
}
// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex4p2(Eigen::Tensor<double, 2> x,
                               Eigen::Tensor<int, 1> iv) {
  // x[2, iv]
  return nCompiler::IndexByScalar<0>().op(1, nCompiler::IndexByVec<1>().op(iv, x));
}

// Assign
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex3p3(Eigen::Tensor<double, 2> x,
                               Eigen::Tensor<int, 1> iv,
                               Eigen::Tensor<double, 1> v) {
  // x[2, iv] // not outer DimId 0 is after removing first dim
  nCompiler::IndexByVec<0>().op(iv, nCompiler::IndexByScalar<0>().op(1,x))=v;
  return x;
}
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex4p3(Eigen::Tensor<double, 2> x,
                               Eigen::Tensor<int, 1> iv,
                               Eigen::Tensor<double, 1> v) {
  // x[2, iv]
  nCompiler::IndexByScalar<0>().op(1, nCompiler::IndexByVec<1>().op(iv, x))=v;
  return x;
}

/////////////////////////
// Single indexing by sequence
// Read from single sequence
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex5(Eigen::Tensor<double, 2> x) {
  return nCompiler::IndexBySeqs<1>::go({{{0, 1, 2}}},x); // x[2:3,]
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex5p1(Eigen::Tensor<double, 2> x) {
  // Index multiple dimensions and use macros
//  return nCompiler::IndexBySeqs<2>::go({{{0, 1, 2}, {1, 1, 3}}},x); // x[2:3,]
  return ISEQS_(2, SEQS_(SEQ_(0,1,2), SEQ_(1,1,3)), x);
}


// Use compact notation from macros as will be needed in generated code
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex5b(Eigen::Tensor<double, 2> x) {
  return nCompiler::IndexBySeqs<1>::go(SEQS_(SEQ_(0,1,2)),x); // x[2:3,]
}

// Assign via a single sequence
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex6(Eigen::Tensor<double, 2> x, Eigen::Tensor<double, 2> v) {
  // Try macro format
  double i = 1;
  ISEQS_(1, SEQS_(SEQ_(0,i,2)), x)=v;
//  nCompiler::IndexBySeqs<1>::go({{{0, 1, 2}}},x) = v; // x[2:3,]
  return x;
}

///////////////////////
// CHIP OF INDEX SEQ
// Access
// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex7(Eigen::Tensor<double, 2> x) {
  return nCompiler::IndexByScalar<1>().op(2, nCompiler::IndexBySeqs<1>::go({{{0, 1, 2}}}, x));
}

// Assign
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex8(Eigen::Tensor<double, 2> x,
                             Eigen::Tensor<double, 1> v) {
  nCompiler::IndexByScalar<1>().op(2, nCompiler::IndexBySeqs<1>::go({{{0, 1, 2}}}, x))=v;
  return x;
}

///////////////////////
// INDEX SEQ OF CHIP
// Access
// [[Rcpp::export]]
Eigen::Tensor<double, 1> ex7p1(Eigen::Tensor<double, 2> x) {
  return nCompiler::IndexBySeqs<1>::go({{{0, 1, 2}}}, nCompiler::IndexByScalar<1>().op(2, x));
}

// Assign
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex8p1(Eigen::Tensor<double, 2> x,
                             Eigen::Tensor<double, 1> v) {
   nCompiler::IndexBySeqs<1>::go({{{0, 1, 2}}}, nCompiler::IndexByScalar<1>().op(2, x))=v;
  return x;
}


///////////////////////
// INDEX VEC OF INDEX SEQ
// Access
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex9(Eigen::Tensor<double, 2> x,
                             Eigen::Tensor<int, 1> iv) {
  // x[, 2:4][,iv]
  return nCompiler::IndexByVec<1>().op(iv, nCompiler::IndexBySeqs<1>::go({{{1, 1, 3}}}, x));
}

// Assign
// [[Rcpp::export]]
Eigen::Tensor<double, 2> ex10(Eigen::Tensor<double, 2> x,
                             Eigen::Tensor<int, 1> iv,
                             Eigen::Tensor<double, 2> v) {
  double i = 1;
  nCompiler::IndexByVec<1>().op(iv, nCompiler::IndexBySeqs<1>::go({{{I_(i), 1, 3}}}, x))=v;
  return x;
}
