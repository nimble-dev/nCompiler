#ifndef SHARED_PTR_AS_WRAP_FORWARD_DECLARATIONS_H_
#define SHARED_PTR_AS_WRAP_FORWARD_DECLARATIONS_H_

#include <memory>

namespace Rcpp {
namespace traits {
  template <typename T>
    class Exporter< std::shared_ptr< T > >;
  }
}

namespace Rcpp {
  template<typename T>
    SEXP wrap( std::shared_ptr< T > obj );
}

#endif // SHARED_PTR_AS_WRAP_FORWARD_DECLARATIONS_H_
