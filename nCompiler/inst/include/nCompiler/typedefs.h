#ifndef _NCOMPILER_TYPEDEFS
#define _NCOMPILER_TYPEDEFS

typedef Eigen::Tensor<double, 1> Eigen1d;
typedef Eigen::TensorMap<Eigen1d> EigenMap1d;
typedef Eigen::array<Eigen1d::Index, 1> IndexArray;
typedef double(*distn3_1scalar_op)(double, double, double, int);

#endif
