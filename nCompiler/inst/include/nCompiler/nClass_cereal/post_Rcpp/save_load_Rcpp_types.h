#ifndef NCOMPILER_CEREAL_SAVE_LOAD_RCPP_TYPES_H_
#define NCOMPILER_CEREAL_SAVE_LOAD_RCPP_TYPES_H_

#include <nCompiler/nClass_cereal/archives.h>
#include "Rcpp_types_cereal.h"

template<class Archive, class RcppType>
  void save_Rcpp_cereal(Archive & archive, RcppType const & m) {
  Rcpp::Function f("serialize");
  Rcpp::RawVector rv = f(m, R_NilValue);
  std::vector<unsigned char> rv_cpp(rv.size());
  std::copy(rv.begin(), rv.end(), rv_cpp.begin());
  archive( rv_cpp );
}

template<class Archive, class RcppType>
  void load_Rcpp_cereal(Archive & archive, RcppType const & m) {
  Rcpp::Function f("unserialize");
  std::vector<unsigned char> rv_cpp;
  archive( rv_cpp );
  Rcpp::RawVector rv(rv_cpp.size() );
  std::copy(rv_cpp.begin(), rv_cpp.end(), rv.begin());
  m = f(rv);
}

#endif // SAVE_LOAD_RCPP_TYPES_H_
