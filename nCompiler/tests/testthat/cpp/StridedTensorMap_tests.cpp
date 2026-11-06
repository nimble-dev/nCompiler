#include "preamble.h"


// [[Rcpp::export]]
double STM1 ( Eigen::Tensor<double, 3> x )  {
  // This is the most basic test of making a map and accessing an element
  // Make a 3D tensor within the 3D origin
   typedef double ScalarType;
//   // nDim is dimension of the map, not the original
   #define nDim 3
   typedef Eigen::Tensor<ScalarType, nDim> TensorType;
   typedef Eigen::StridedTensorMap< TensorType> StridedTensorMapType;
// x[, 2:3, ], input is 6 x 5 x 4
   StridedTensorMapType xMap(x, Eigen::array<b__, 3>({b__(0, 5), b__(1, 2), b__(0,3)}));
   return xMap(1, 1, 2); //xMap[2, 2, 3] in R
}


// [[Rcpp::export]]
Eigen::Tensor<double, 3>  STM2 ( Eigen::Tensor<double, 3> x )  {
  // This test uses assignment with the map on the RHS.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 3> ans;
   typedef double ScalarType;
//   // nDim is dimension of the map, not the original
   #define nDim 3
   typedef Eigen::Tensor<ScalarType, nDim> TensorType;
   typedef Eigen::StridedTensorMap< TensorType> StridedTensorMapType;
// x[, 2:3, ], input is 6 x 5 x 4
   StridedTensorMapType xMap(x, Eigen::array<b__, 3>({b__(0, 5), b__(1, 2), b__(0,3)}));
   ans = xMap;          // arithmetic
   return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3>  STM3 ( Eigen::Tensor<double, 3> x )  {
  // This test uses more compact notation with fewer typedefs.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 3> ans;
  ans = Eigen::MakeStridedTensorMap<3>::make(x, Eigen::MakeIndexBlocks(b__(0, 5), b__(1, 2), b__(0, 3))).log();          // arithmetic
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3>  STM4 ( Eigen::Tensor<double, 3> x )  {
  // this test uses the more compact notation to create the map as a separate object.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 3> ans;
  Eigen::StridedTensorMap< Eigen::Tensor<double, 3> > xMap = Eigen::MakeStridedTensorMap<3>::make(x, Eigen::MakeIndexBlocks(b__(0, 5), b__(1, 2), b__(0, 3)));
  ans = xMap.log();          // arithmetic
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 3>  STM5 ( Eigen::Tensor<double, 3> x )  {
  // This test checks use of a map on the LHS of an assignment.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 3> ans;
  ans = x; // just for dimensions
  ans.setConstant(0);
  Eigen::MakeStridedTensorMap<3>::make(ans, Eigen::MakeIndexBlocks(b__(0, 5), b__(1, 2), b__(0, 3))) =
    Eigen::MakeStridedTensorMap<3>::make(x, Eigen::MakeIndexBlocks(b__(0, 5), b__(1, 2), b__(0, 3)));
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2>  STM6 ( Eigen::Tensor<double, 3> x )  {
  // This test checks making a 2D tensor from a 3D map with singleton dimension.
  Eigen::Tensor<double, 2> ans;
  // x[2:5, 3, 2:3]
  ans = Eigen::MakeStridedTensorMap<2>::make(x, Eigen::MakeIndexBlocks(b__(1, 4), b__(2), b__(1, 2)));
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2>  STM7 ( Eigen::Tensor<double, 3> x )  {
  // This test checks making a 2D tensor from a 3D map with singleton dimension in first slot.
  Eigen::Tensor<double, 2> ans;
  // x[5, 1:3, 2:3]
  ans = Eigen::MakeStridedTensorMap<2>::make(x, Eigen::MakeIndexBlocks(b__(4), b__(0, 2), b__(1, 2)));
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1>  STM8 ( Eigen::Tensor<double, 3> x )  {
  // This test checks making a 1D tensor from a 3D map.
  Eigen::Tensor<double, 1> ans;
  // x[5, 1:3, 2]
  ans = Eigen::MakeStridedTensorMap<1>::make(x, Eigen::MakeIndexBlocks(b__(4), b__(0, 2), b__(1)));
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2>  STM9 ( Eigen::Tensor<double, 3> x )  {
  // This test checks making a 1D tensor from a 3D map with a blank.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 2> ans;
  // x[5, , 2:3]
  ans = Eigen::MakeStridedTensorMap<2>::make(x, Eigen::MakeIndexBlocks(b__(4), b__(), b__(1, 2)));
  return(ans);
}

// [[Rcpp::export]]
double STM10 ( Eigen::Tensor<double, 3> x )  {
  // Tests the default constructor and rebind(): a default-constructed
  // StridedTensorMap starts empty (isEmpty()), and rebind() gives it the
  // same data/shape/selection that make_fieldSTM/rebind_fieldSTM pass to it
  // (a raw Scalar* plus generic input_sizes/ss containers), as opposed to
  // STM1's InputType&-based constructor. Same slice/index as STM1.
  Eigen::StridedTensorMap<Eigen::Tensor<double, 3> > xMap;
  if (!xMap.isEmpty())
    Rcpp::stop("STM10: expected isEmpty() to be true before rebind()");
  std::vector<int> dims = {(int)x.dimension(0), (int)x.dimension(1), (int)x.dimension(2)};
  std::vector<b__> ss = {b__(0, 5), b__(1, 2), b__(0, 3)};
  xMap.rebind(x.data(), dims, ss);
  if (xMap.isEmpty())
    Rcpp::stop("STM10: expected isEmpty() to be false after rebind()");
  return xMap(1, 1, 2); // x[, 2:3, ][2, 2, 3] in R
}

// [[Rcpp::export]]
Rcpp::List STM11 ( Eigen::Tensor<double, 3> x, Eigen::Tensor<double, 3> y )  {
  // Tests that rebind() can be called again on an already-bound
  // StridedTensorMap to re-seat it to a different tensor's storage -- the
  // "declare once, rebind at setup" persistent-member pattern rebind_fieldSTM
  // is meant to support for repeated access in a hot loop.
  Eigen::StridedTensorMap<Eigen::Tensor<double, 3> > map;
  std::vector<b__> ss = {b__(0, 5), b__(1, 1), b__(0, 3)};

  std::vector<int> dimsX = {(int)x.dimension(0), (int)x.dimension(1), (int)x.dimension(2)};
  map.rebind(x.data(), dimsX, ss);
  Eigen::Tensor<double, 3> ansX = map;

  std::vector<int> dimsY = {(int)y.dimension(0), (int)y.dimension(1), (int)y.dimension(2)};
  map.rebind(y.data(), dimsY, ss);
  Eigen::Tensor<double, 3> ansY = map;

  return Rcpp::List::create(Rcpp::Named("ansX") = ansX, Rcpp::Named("ansY") = ansY);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2> STM12 ( Eigen::Tensor<double, 3> x )  {
  // Tests rebind() with a singleton (rank-reducing) selection -- same
  // selection as STM6, but built via rebind()'s raw-pointer/std::vector<b__>
  // path instead of STM6's Eigen::array<b__,N>+InputType constructor path.
  Eigen::StridedTensorMap<Eigen::Tensor<double, 2> > map;
  std::vector<int> dims = {(int)x.dimension(0), (int)x.dimension(1), (int)x.dimension(2)};
  std::vector<b__> ss = {b__(1, 4), b__(2), b__(1, 2)}; // x[2:5, 3, 2:3]
  map.rebind(x.data(), dims, ss);
  Eigen::Tensor<double, 2> ans = map;
  return ans;
}
