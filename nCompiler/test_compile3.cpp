#ifndef __nClass_1_CPP
#define __nClass_1_CPP
#define NCOMPILER_HANDLE_EIGEN_ERRORS
#define NCOMPILER_USES_EIGEN
#define NCOMPILER_USES_NCLASS_INTERFACE
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <iostream>
#include <nCompiler/nCompiler_omnibus_first_cpp.h>
#include "test_compile3.h"
using namespace Rcpp;
// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]
// [[Rcpp::depends(RcppEigenAD)]]
// [[Rcpp::depends(RcppParallel)]]
// [[Rcpp::depends(nCompiler)]]
// [[Rcpp::depends(Rcereal)]]

  nClass_1::nClass_1 (  )  {
RESET_EIGEN_ERRORS
}

// [[Rcpp::export]]
SEXP  new_nClass_1 (  )  {
RESET_EIGEN_ERRORS
return CREATE_NEW_NCOMP_OBJECT(nClass_1);
}

// [[Rcpp::export]]
void  set_CnClass_env_nClass_1 ( SEXP env )  {
RESET_EIGEN_ERRORS
SET_CNCLASS_ENV(nClass_1, env);
}

NCOMPILER_INTERFACE(
nClass_1,
NCOMPILER_FIELDS(
field("x", &nClass_1::x)
),
NCOMPILER_METHODS()
)
#endif
#ifndef __R_interfaces_CPP
#define __R_interfaces_CPP
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include "test_compile3_interfaces.h"

inline genericInterfaceBaseC *get_genericInterfaceBaseC(SEXP Xptr) {
  return reinterpret_cast<genericInterfaceBaseC*>
    (reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Xptr))->get_ptr());
}

// This is completely generic, good for all derived classes
// [[Rcpp::export]]
SEXP get_value(SEXP Xptr, const std::string &name) {
  genericInterfaceBaseC *obj =
    get_genericInterfaceBaseC(Xptr);
  //  std::cout << name << std::endl;
  return(obj->get_value( name ));
}

// This is completely generic, good for all derived classes
// [[Rcpp::export]]
SEXP set_value(SEXP Xptr, const std::string &name, SEXP Svalue) {
  genericInterfaceBaseC *obj =
    get_genericInterfaceBaseC(Xptr);
  //std::cout << name << std::endl;
  obj->set_value( name, Svalue );
  return(R_NilValue);
}

// This is completely generic, good for all derived classes
// [[Rcpp::export]]
SEXP call_method(SEXP Xptr, const std::string &name, SEXP Sargs) {
  genericInterfaceBaseC *obj =
    get_genericInterfaceBaseC(Xptr);
  //  std::cout << name << std::endl;
  return(obj->call_method( name, Sargs ));
}

#endif
