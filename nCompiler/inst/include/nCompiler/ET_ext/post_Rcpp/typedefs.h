#ifndef _NCOMPILER_TYPEDEFS
#define _NCOMPILER_TYPEDEFS

typedef Eigen::Tensor<double, 1> Eigen1d;
typedef Eigen::Tensor<double, 2> Eigen2d;
typedef Eigen::Tensor<double, 3> Eigen3d;
typedef Eigen::Tensor<int, 1> Eigen1i;
typedef Eigen::Tensor<int, 2> Eigen2i;
typedef Eigen::Tensor<int, 3> Eigen3i;
typedef Eigen::Tensor<bool, 1> Eigen1b;
typedef Eigen::Tensor<bool, 2> Eigen2b;
typedef Eigen::Tensor<bool, 3> Eigen3b;
typedef Eigen::TensorMap<Eigen1d> EigenMap1d;
typedef Eigen::array<Eigen1d::Index, 1> IndexArray;
typedef double(*distn3_1scalar_op)(double, double, double, int);

#endif
