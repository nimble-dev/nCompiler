#ifndef _AS_WRAP_SHARED_PTR
#define _AS_WRAP_SHARED_PTR

#include "shared_ptr_holder.h"
#include "nCompiler_class_factory.h"

namespace Rcpp {
namespace traits {
template <typename T>
class Exporter< std::shared_ptr< T > > {
public:
  std::shared_ptr<T> sp_;
  Exporter(SEXP Sx) {
    // std::cout<<"Got it!"<<std::endl;
    Rcpp::Environment loadedObjEnv(Sx);
    SEXP Xptr = PROTECT(loadedObjEnv["extptr"]);
    sp_ = reinterpret_cast<shared_ptr_holder<T>* >(R_ExternalPtrAddr(Xptr))->sp();
    UNPROTECT(1);
  }
  inline std::shared_ptr< T > get(){
    //std::cout<<"Getting it!"<<std::endl;
    return sp_;
  }
};
}
}

namespace Rcpp {
 template<typename T>
 SEXP wrap( std::shared_ptr< T > obj ) {
   // std::cout<<"Got it!"<<std::endl;
   SEXP Sans;
   Sans = PROTECT(loadedObjectEnv(PROTECT(return_nCompiler_object< T >(obj))));
   UNPROTECT(2);
   return Sans;
 }
}


#endif
