#ifndef TENSORFLEX_CPPAD_H_
#define TENSORFLEX_CPPAD_H_

#include "tensorFlex.h"

template<>
struct type_category<CppAD::AD<double> > {
  typedef trueScalar type;
};

template<>
struct nDimTraits<CppAD::AD<double> > {
  static const int NumDimensions = 0;
};

template<>
struct is_zeroDim<CppAD::AD<double> > {
  typedef zeroDim type; // This will be over-ridden by true_scalar in the next tag argument
  // but nevertheless we need this template to evaluate correctly.
};

template<>
class flex__<CppAD::AD<double> > {
public:
  typedef CppAD::AD<double> ScalarType;
  ScalarType &x;
  explicit flex__(ScalarType &x_) : x(x_) {
    //  std::cout<<"building flex__ for flexible-size Lhs"<<std::endl;
  }
  template<typename oType>
  ScalarType& operator=(const oType &other) {
    // std::cout<<"RHS NumDims = "<< nDimTraits<oType>::NumDimensions <<std::endl;
    // For now, assume nDim(oType) > nDim(xType). Figure out dispatching later.
    return smartAssignToScalar(x, other,
                               typename is_zeroDim<oType>::type(),
                               typename type_category<oType>::type());
  }
};
#endif // TENSORFLEX_CPPAD_H_
