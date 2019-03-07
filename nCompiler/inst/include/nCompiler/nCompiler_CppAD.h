#ifndef _NCOMPILER_CPPAD
#define _NCOMPILER_CPPAD
/* Definitions only to be included when a nimbleFunction needs CppAD */
#include <cppad/cppad.hpp>
//#include <cppad/utility/nan.hpp>
//#include <nimble/predefinedNimbleLists.h>
#include <cstdio>
#include <vector>
#include <algorithm>
#include "predefined.cpp"

/* nCompilerCppADinfoClass is the class to convey information from a nimbleFunction
object
to generic CppAD driver wrappers like calcjacobian.
Each nimbleFunction enabled for CppAD will have an object of this class. */

class nCompilerCppADinfoClass {
public:
  std::vector<double> independentVars;
  CppAD::ADFun<double> *ADtape;
};

// MACROS to get Cfoo_derivs_:
// Definition:
// Example DERIVS_METHOD_DEF(nimClass_1,Cfoo,Cfoo_derivs_ (ARG1_x_, ARG1_y_), double ARG1_x_, double ARG1_Y_)
// will give
//  std::shared_ptr<nC_derivClass> nimClass_1::Cfoo_derivs_(double ARG1_x_, double ARG1_Y_,
//                                                           const Eigen::Tensor<int, 1> &derivOrders) {
//   getDerivs_(Cfoo_ADargumentTransfer_(ARG1_x_, ARG1_y_,
//                                       derivOrders);
//              );
// }
// Note that the third macro argument has all fn arguments inside (), while the
// following macro arguments have the typed fn arguments one by one.
#define DERIVS_METHOD_DEF(CLASS, NAME, NAME_DERIVS_, ARGS,...)		\
  std::shared_ptr<nC_derivClass> CLASS::NAME_DERIVS_ (__VA_ARGS__, const Eigen::Tensor<int, 1> derivOrders) { \
  return( \
	 getDerivs_(NAME ## _ADargumentTransfer_ ARGS, \
		   derivOrders) \
	 ); \
} \

// Declaration:
// Example DERIVS_METHOD_DECL(Cfoo,Cfoo_derivs_, double ARG1_x_, double ARG1_Y_)
// will give
//  std::shared_ptr<nC_derivClass> nimClass_1::Cfoo_derivs_(double ARG1_x_, double ARG1_Y_,
//                                                           const Eigen::Tensor<int, 1> &derivOrders);
#define DERIVS_METHOD_DECL(NAME_DERIVS_, ...)				\
  std::shared_ptr<nC_derivClass> NAME_DERIVS_ (__VA_ARGS__, const Eigen::Tensor<int, 1> derivOrders) ; \


std::shared_ptr<nC_derivClass> getDerivs_(nCompilerCppADinfoClass &ADinfo,
					 const Eigen::Tensor<int, 1> &derivOrders) {
  std::shared_ptr<nC_derivClass> ans(new nC_derivClass);
  typedef typename Eigen::Tensor<double, 1>::Index Index;
  std::vector<double> value_ans = ADinfo.ADtape->Forward(0, ADinfo.independentVars);
  Index q = value_ans.size();
  Index n = ADinfo.independentVars.size();
  Eigen::array< Index, 1> sizeValue = {q};
  ans->value.resize(sizeValue);
  Eigen::array< Index, 2> sizeGrad = {n, q};
  ans->gradient.resize(sizeGrad);
  std::copy(value_ans.begin(), value_ans.end(), ans->value.data());
  std::vector<double> w(q, 0);
  std::vector<double> cppad_derivOut;
  std::size_t maxOrder = 1; // TO-DO generalize from derivOrders
  for(Index dy_ind = 0; dy_ind < q; dy_ind++){
    w[dy_ind] = 1;
    cppad_derivOut = ADinfo.ADtape->Reverse(1, w);
    for(Index i = 0; i < n; i++){
      ans->gradient(i, dy_ind) = cppad_derivOut[i*maxOrder + 0];
    }
    w[dy_ind] = 0;
  }
  return(ans);
}

/* nimbleFunctionCppADbase is a base class to be inherited by all
CppAD-enabled nimbleFunctions. Some of these functions might
make more sense as stand-alone functions.  Let's see. */
class nimbleFunctionCppADbase {
public:
  void getDerivs(nCompilerCppADinfoClass &ADinfo) {}; // shell to be filled
};

class nComp_AD_class {
 public:
  nComp_AD_class();
};


#endif
