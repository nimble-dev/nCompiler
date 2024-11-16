#ifndef _NCOMPILER_RECYCLYING_RULE
#define _NCOMPILER_RECYCLYING_RULE

#include <unsupported/Eigen/CXX11/Tensor>
#include "typedefs.h"

// recycling rule
// see typedefs.h for the types used below
template<typename ScalarType, distn3_1scalar_op distn>
class distn_nullaryClass {
  const Eigen1d &Arg1, &Arg2, &Arg3;
  const ScalarType &Arg4;
  int size;
public:
  typedef ScalarType ScalarArgType;
  typedef typename Eigen1d::Index Index;
  // the following doesn't work:
  // static const distn3_1scalar_op distn = distn_;

 distn_nullaryClass(const Eigen1d &A1, const Eigen1d &A2, const Eigen1d &A3, const ScalarType &A4) :
   Arg1(A1), Arg2(A2), Arg3(A3), Arg4(A4), size(R::imax2(A1.size(), R::imax2(A2.size(), A3.size())))
  {}

  // Whatever size determinations are needed can happen here.
  // And we can return a fixed-size array to allow more than one dimension
  int getSize() const {
    // when we move to multiple dimensions, this method should return an IndexArray
    return size;
  }

  double operator()(Index i) const {
    // use the Rf naming convention in generated code
    return distn(Arg1.coeff(i % Arg1.size()), Arg2.coeff(i % Arg2.size()), Arg3.coeff(i % Arg3.size()), Arg4);
  }
  double operator()(Index i, Index j) const {
    // not implemented
    return Arg1.coeff(i);
  }
};

// The reason for using a class here for a second layer is to
// ensure that everything stays in scope while it's being used.
template<class nullaryClass>
class customNullary {
  nullaryClass nullaryObj;
  typedef EigenMap1d sizeProxyType;
  sizeProxyType sizeProxy;
  typedef typename nullaryClass::ScalarArgType ScalarType;
  typedef typename Eigen::TensorCwiseNullaryOp< nullaryClass, sizeProxyType > nullaryOpType;
public:
  customNullary(const Eigen1d &Arg1, const Eigen1d &Arg2, const Eigen1d &Arg3, const ScalarType &Arg4) :
    nullaryObj(Arg1, Arg2, Arg3, Arg4),
    // we'll eventually want to use the more general constructor
    // TensorMap(PointerArgType dataPtr, const array<Index, NumIndices>& dimensions)
    sizeProxy(static_cast<double*>(0), nullaryObj.getSize()) {}

  nullaryOpType op() { return nullaryOpType(sizeProxy, nullaryObj); }
};

// 3 arguments and 1 scalar
// since args constant references, need to have a function that calls RR3_1scalar
template<typename ScalarType, distn3_1scalar_op distn>
Eigen1d RR3_1scalar(const Eigen1d &arg1, const Eigen1d &arg2, const Eigen1d &arg3, const ScalarType &arg4) {
  return customNullary< distn_nullaryClass< ScalarType, distn > >(arg1, arg2, arg3, arg4).op();
}

// define a function-like macro that returns a distribution function
#define RR31fun(NAME, DISTN) \
template<typename ScalarType> \
Eigen1d RR_ ## NAME(const Eigen1d &x1, const Eigen1d &x2, const Eigen1d &x3, const ScalarType &x4) { \
  return RR3_1scalar<ScalarType, DISTN>(x1, x2, x3, x4); \
};

RR31fun(dbeta, Rf_dbeta); // RR_dbeta
RR31fun(dbinom, Rf_dbinom); // RR_dbinom
// RR31fun(ddexp, ???); // RR_ddexp
RR31fun(dgamma, Rf_dgamma); // RR_dgamma
// RR31fun(dinvgamma, ???); // RR_dinvgamma
RR31fun(dlnorm, Rf_dlnorm); // RR_dlnorm
RR31fun(dnbinom, Rf_dnbinom); // RR_dnbinom
RR31fun(dnorm, Rf_dnorm4); // RR_dnorm
RR31fun(dt, Rf_dt); // RR_dt
// RR31fun(dt_nonstandard, ???); // RR_dt_nonstandard
RR31fun(dunif, Rf_dunif); // RR_dunif
RR31fun(dweibull, Rf_dweibull); // RR_dweibull

#endif
