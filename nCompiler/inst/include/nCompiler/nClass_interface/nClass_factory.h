#ifndef __NCOMPILER_CLASS_FACTORY
#define __NCOMPILER_CLASS_FACTORY

#include<R.h>
#include<Rinternals.h>
#ifdef NCOMPILER_USES_CEREAL
#include<nCompiler/nClass_cereal/archives.h>
#endif
#define SHOW_SHARED_PTR_DESTRUCTORS

class shared_ptr_holder_base {
 public:
  virtual void *get_ptr() const {
   std::cout<<"Error: you should be in a derived shared_ptr_holder class get_ptr().  This is the base class."<<std::endl;
    return(0);
  };
  virtual std::shared_ptr<genericInterfaceBaseC> get_interfaceBase_shared_ptr() const {
   std::cout<<"Error: you should be in a derived shared_ptr_holder class get_interfaceBase_shared_ptr().  This is the base class."<<std::endl;
    return(std::shared_ptr<genericInterfaceBaseC>());
  };
  virtual shared_ptr_holder_base* make_shared_ptr_holder()=0;
  virtual ~shared_ptr_holder_base() {
   // std::cout<<"destructing shared_ptr_holder_base"<<std::endl;
  };
  virtual SEXP return_this_nCompiler_object()=0;
#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
  void _SERIALIZE_(Archive &archive) {}
#endif
};

template<typename T>
class shared_ptr_holder: public shared_ptr_holder_base {
 public:
  std::shared_ptr<T> sp_;
  void *get_ptr() const {
    return static_cast<void*>(dynamic_cast<genericInterfaceBaseC*>(sp_.get()));
  }
  std::shared_ptr<genericInterfaceBaseC> get_interfaceBase_shared_ptr() const {
    return std::static_pointer_cast<genericInterfaceBaseC>(sp_);
  }
  shared_ptr_holder_base* make_shared_ptr_holder() {
    std::cout<<"making new shared_ptr_holder_base"<<std::endl;
    return
      dynamic_cast<shared_ptr_holder_base*>
      (new shared_ptr_holder<T>(sp_));
  }
  SEXP return_this_nCompiler_object();
  std::shared_ptr<T> &sp() {return sp_;}
  shared_ptr_holder() {} // needed for cereal
  shared_ptr_holder(T *obj) : sp_(obj) {}
  shared_ptr_holder(std::shared_ptr<T> &sp_other) {sp_=  sp_other;}
  ~shared_ptr_holder() {
#ifdef SHOW_SHARED_PTR_DESTRUCTORS
//    std::cout<<"Destroying shared_ptr_holder.";
    if(sp_.unique()) {
//      std::cout<<" This should destroy the underlying nCompiler object."<<std::endl;
    } else {
//      std::cout<<" This won't yet destroy the underlying nCompiler object. It is not unique."<<std::endl;
    }
#endif
  }
#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
  void _SERIALIZE_(Archive &archive) {
    archive(cereal::base_class<shared_ptr_holder_base>(this), sp_);
  }
#endif
};

inline void finalize_shared_ptr_holder(SEXP Xptr) {
//  std::cout<<"Entering finalize_shared_ptr_holder"<<std::endl;
  shared_ptr_holder_base *sph = static_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Xptr));
  if(sph) delete sph;
}

/* This takes care of creating an R external pointer with a registered finalizer
   for a new object. */
template<typename T>
SEXP new_nCompiler_object (  )  {
  shared_ptr_holder<T> *sph = new shared_ptr_holder<T>(new T);
  SEXP Sans;
  // The dynamic_cast to shared_ptr_holder_base is because that is what we will cast back to
  // when a void* (externalptr) is passed back in from R elsewhere.
  // The static_cast to void* should happen implicitly anyway but we write it for clarity.
  Sans = PROTECT(R_MakeExternalPtr(static_cast<void*>(dynamic_cast<shared_ptr_holder_base*>(sph)),
                                   R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(Sans, finalize_shared_ptr_holder, static_cast<Rboolean>(1));
  UNPROTECT(1);
  return(Sans);
}

/* This takes care of creating an R external pointer with a registered finalizer
   for an existing object. */
template<typename T>
SEXP return_nCompiler_object(std::shared_ptr<T> &sp_other ) {
  shared_ptr_holder<T> *sph = new shared_ptr_holder<T>(sp_other);
  SEXP Sans;
  Sans = PROTECT(R_MakeExternalPtr(static_cast<void*>(dynamic_cast<shared_ptr_holder_base*>(sph)),
                                   R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(Sans, finalize_shared_ptr_holder, static_cast<Rboolean>(1));
  UNPROTECT(1);
  return(Sans);  
}

template<typename T>
SEXP shared_ptr_holder<T>::return_this_nCompiler_object() {
    return return_nCompiler_object<T>(sp_);
  }

// support syntax like this: nClass_builder<nc1>()(x, y)
template<class NCLASS>
class nClass_builder {
 public:
  template<typename... ARGS>
    std::shared_ptr<NCLASS> operator()(ARGS... x) {
    return std::shared_ptr<NCLASS>(new NCLASS(x...));
  }
};


#endif
