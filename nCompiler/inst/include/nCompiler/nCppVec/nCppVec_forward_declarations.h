#ifndef NCPPVEC_FORWARD_DECLARATIONS_H_
#define NCPPVEC_FORWARD_DECLARATIONS_H_

template<typename T>
class nCppVec;

namespace Rcpp {
  namespace traits {
    template <typename T>
    class Exporter< nCppVec< T > >;
  }
}

namespace Rcpp {
  template<typename T>
    SEXP wrap(const nCppVec< T > & obj );
}

#endif // NCPPVEC_FORWARD_DECLARATIONS_H_
