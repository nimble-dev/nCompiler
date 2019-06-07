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
  // This test checkes use of a map on the LHS of an assignment.
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
  // This test checkes use of a map on the LHS of an assignment.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 2> ans;
  // x[2:5, 3, 2:3]
  ans = Eigen::MakeStridedTensorMap<2>::make(x, Eigen::MakeIndexBlocks(b__(1, 4), b__(2), b__(1, 2)));
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2>  STM7 ( Eigen::Tensor<double, 3> x )  {
  // This test checkes use of a map on the LHS of an assignment.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 2> ans;
  // x[5, 1:3, 2:3]
  ans = Eigen::MakeStridedTensorMap<2>::make(x, Eigen::MakeIndexBlocks(b__(4), b__(0, 2), b__(1, 2)));
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 1>  STM8 ( Eigen::Tensor<double, 3> x )  {
  // This test checkes use of a map on the LHS of an assignment.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 1> ans;
  // x[5, 1:3, 2]
  ans = Eigen::MakeStridedTensorMap<1>::make(x, Eigen::MakeIndexBlocks(b__(4), b__(0, 2), b__(1)));
  return(ans);
}

// [[Rcpp::export]]
Eigen::Tensor<double, 2>  STM9 ( Eigen::Tensor<double, 3> x )  {
  // This test checkes use of a map on the LHS of an assignment.
  // Make a 3D tensor within the 3D origin
  Eigen::Tensor<double, 2> ans;
  // x[5, , 2:3]
  ans = Eigen::MakeStridedTensorMap<2>::make(x, Eigen::MakeIndexBlocks(b__(4), b__(), b__(1, 2)));
  return(ans);
}
