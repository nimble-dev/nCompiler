#ifndef __NCOMPILER_CLASS_FACTORY
#define __NCOMPILER_CLASS_FACTORY

#include<R.h>
#include<Rinternals.h>
#include "shared_ptr_holder.h"

inline void finalize_shared_ptr_holder(SEXP Xptr) {
  std::cout<<"Entering finalize_shared_ptr_holder"<<std::endl;
  shared_ptr_holder_base *sph = reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Xptr));
  if(sph) delete sph;
}

template<typename T>
SEXP new_nCompiler_object (  )  {
  shared_ptr_holder<T> *sph = new shared_ptr_holder<T>(new T);
  SEXP Sans;
  Sans = PROTECT(R_MakeExternalPtr(sph, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(Sans, finalize_shared_ptr_holder, static_cast<Rboolean>(1));
  UNPROTECT(1);
  return(Sans);
}

template<typename T>
SEXP return_nCompiler_object(std::shared_ptr<T> &sp_other ) {
  shared_ptr_holder<T> *sph = new shared_ptr_holder<T>(sp_other);
  SEXP Sans;
  Sans = PROTECT(R_MakeExternalPtr(sph, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(Sans, finalize_shared_ptr_holder, static_cast<Rboolean>(1));
  UNPROTECT(1);
  return(Sans);  
}

#endif
