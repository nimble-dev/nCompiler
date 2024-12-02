#ifndef NLIST_FORWARD_DECLARATIONS_H_
#define NLIST_FORWARD_DECLARATIONS_H_

template<typename T>
class nList;

namespace Rcpp {
  namespace traits {
    template <typename T>
    class Exporter< nList< T > >;
  }
}

namespace Rcpp {
  template<typename T>
    SEXP wrap(const nList< T > & obj );
}

#endif // NLIST_FORWARD_DECLARATIONS_H_
