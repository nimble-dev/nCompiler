#ifndef TENSORFLEX_CPPAD_H_
#define TENSORFLEX_CPPAD_H_

#include "tensorFlex.h"

// With this, flex_ treats CppAD::AD<double> as a true scalar.
template<>
struct type_category<CppAD::AD<double> > {
  typedef trueScalar type;
};

namespace Eigen {
  template<>
  struct nDimTraits2<CppAD::AD<double> > {
    static const std::size_t NumDimensions = 0;
    typedef CppAD::AD<double> Scalar;
  };
}

// DEPRECATED along with nDimTraits (see tensorFlex.h).
template<>
struct nDimTraits<CppAD::AD<double> > {
  static const int NumDimensions = 0;
};

#endif // TENSORFLEX_CPPAD_H_
