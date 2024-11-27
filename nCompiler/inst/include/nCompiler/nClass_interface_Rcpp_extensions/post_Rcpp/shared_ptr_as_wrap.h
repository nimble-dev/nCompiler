#ifndef SHARED_PTR_AS_WRAP_H_
#define SHARED_PTR_AS_WRAP_H_

#include <memory>
#include <nCompiler/nClass_interface_Rcpp_extensions/shared_ptr_as_wrap_forward_declarations.h>
#include <nCompiler/nClass_interface/nClass_factory.h>
// #include "loadedObjectEnv.h"

// For an input of type T (e.g. shared_ptr< some_nClass_ >),
// Rcpp creates code like this:
// void test_input_(std::shared_ptr<nc1_> obj);
// RcppExport SEXP sourceCpp_1_test_input_(SEXP objSEXP) {
//  BEGIN_RCPP
//  Rcpp::RNGScope rcpp_rngScope_gen;
//  Rcpp::traits::input_parameter< std::shared_ptr<nc1_> >::type obj(objSEXP);
//  test_input_(obj);
//  return R_NilValue;
//  END_RCPP
// }
//
// The Rcpp::traits::input_parameter< std::shared_ptr<nc1_> >::type
// will be Rcpp::InputParameter< std::shared_ptr<nc1_> >, which simply
// holds the input SEXP (objSEXP) and has an explicit conversion to std::shared_ptr<nc1_>
// which will be used for obj in test_input_(obj).
// That explicit conversion is defined by as<std::shared_ptr<nc1_>(SEXP)
// The generic template case of as<> creates an Rcpp::traits::Exporter<nc1>(SEXP)
// and calls its get() function.
// Hence giving template specialization for std::shared_ptr< T > below means
// that this object is first created and then the get() function is called
// to provide the argument of the desired type (to test_input_ above).
namespace Rcpp {
  namespace traits {
    template <typename T>
    class Exporter< std::shared_ptr< T > > {
    public:
      std::shared_ptr<T> sp_;
      Exporter(SEXP Sx) {
        Rcpp::Environment Sx_env(Sx); // Sx is an environment, so initialize an Rcpp:Environment from it.
        SEXP Xptr = PROTECT(Sx_env["extptr"]); // Get the extptr element of it.
        bool ok(false);
        if(Xptr != R_NilValue) {
          ok = true;
        } else {
          UNPROTECT(1);
          Nullable<Rcpp::Environment> private_env = Sx_env["private"];
          if(private_env.isNotNull()) {
            Nullable<Rcpp::Environment> CppObj = Rcpp::Environment(private_env)["CppObj"];
            if(CppObj.isNotNull()) {
              Xptr = PROTECT(Rcpp::Environment(CppObj)["extptr"]);
              if(Xptr != R_NilValue) {
                ok=true;}}}
        }
        if(!ok) {stop("An argument that should be an nClass object is not valid.");}
        sp_ = reinterpret_cast<shared_ptr_holder<T>* >(R_ExternalPtrAddr(Xptr))->sp();
        UNPROTECT(1);
      }
      inline std::shared_ptr< T > get(){
        return sp_;
      }
    };
  }
}

// This is called by code generated by Rcpp
// to return an object of a type such as std::shared_ptr< some_nClass_type >
// Rcpp's code looks like:
// rcpp_result_gen = Rcpp::wrap(test_output_());
// based on user-written code:
// std::shared_ptr<nc1_> test_output_ (  )  {
// std::shared_ptr<nc1_> obj ( new nc1_ );
// return(obj);
// }
namespace Rcpp {
  template<typename T>
  SEXP wrap( std::shared_ptr< T > obj ) {
    SEXP Sans;
    Sans = PROTECT(T::setup_R_return_object_full( PROTECT(return_nCompiler_object< T >(obj) ) ) );
    //   Sans = PROTECT(loadedObjectEnv(PROTECT(return_nCompiler_object< T >(obj))));
    UNPROTECT(2);
    return Sans;
  }
}

#endif // SHARED_PTR_AS_WRAP_H_