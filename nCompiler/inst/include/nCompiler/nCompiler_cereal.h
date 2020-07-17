#ifndef _NCOMPILER_CEREAL
#define _NCOMPILER_CEREAL

#define CEREAL_SERIALIZE_FUNCTION_NAME _SERIALIZE_
#include <cereal/archives/binary.hpp>
#include <cereal/archives/xml.hpp>
#include <cereal/types/polymorphic.hpp>
#include <cereal/types/vector.hpp>
#include <type_traits>

template <class possibleRcppType>
struct is_RcppType {
  static const bool value = false;
};

/* To add support for additional Rcpp types, copy and paste the following */
/* and change Rcpp::NumericVector to the Rcpp type to be supported. */

template<>
struct is_RcppType<Rcpp::NumericVector> {static const bool value = true;};

namespace cereal {
  template<class Archive,
    class possibleRcppType,
    typename std::enable_if< is_RcppType<possibleRcppType>::value, int >::type = 0 >
    void save(Archive & archive, 
	      possibleRcppType const & m)
    {
      Rcpp::Function f("serialize");
      Rcpp::RawVector rv = f(m, R_NilValue);
      std::vector<unsigned char> rv_cpp(rv.size());
      std::copy(rv.begin(), rv.end(), rv_cpp.begin());
      archive( rv_cpp );
    }

  template<class Archive,
    class possibleRcppType,
    typename std::enable_if< is_RcppType<possibleRcppType>::value, int >::type = 0 >
    void load(Archive & archive,
	      possibleRcppType & m)
    {
      Rcpp::Function f("unserialize");
      std::vector<unsigned char> rv_cpp;
      archive( rv_cpp );
      Rcpp::RawVector rv(rv_cpp.size() );
      std::copy(rv_cpp.begin(), rv_cpp.end(), rv.begin());
      m = f(rv);
    }
}

#endif
