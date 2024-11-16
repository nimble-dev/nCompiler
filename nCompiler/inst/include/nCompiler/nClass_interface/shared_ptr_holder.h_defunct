#ifndef __SHARED_PTR_HOLDER
#define __SHARED_PTR_HOLDER

#define SHOW_SHARED_PTR_DESTRUCTORS

#include<nCompiler/nCompiler_cereal.h>

class shared_ptr_holder_base {
 public:
  virtual void *get_ptr() const {
   std::cout<<"Error: you should be in a derived shared_ptr_holder class get_ptr().  This is the base class."<<std::endl;
    return(0);
  };
  virtual shared_ptr_holder_base* make_shared_ptr_holder()=0;
  virtual ~shared_ptr_holder_base() {std::cout<<"destructing shared_ptr_holder_base"<<std::endl;};
  virtual SEXP return_this_nCompiler_object()=0;
  template<class Archive>
  void _SERIALIZE_(Archive &archive) {}
};

template<typename T>
class shared_ptr_holder: public shared_ptr_holder_base {
 public:
  std::shared_ptr<T> sp_;
  void *get_ptr() const {
    return static_cast<void*>(sp_.get());
  }
  shared_ptr_holder_base* make_shared_ptr_holder() {
    std::cout<<"making new shared_ptr_holder_base"<<std::endl;
    return
      dynamic_cast<shared_ptr_holder_base*>
      (new shared_ptr_holder<T>(sp_));
  }
  SEXP return_this_nCompiler_object() {
    return return_nCompiler_object<T>(sp_); // This gives a compiler warning because return_nCompiler_object is not yet defined. to-do: check on ordering of #includes etc.
  }
  std::shared_ptr<T> &sp() {return sp_;}
  shared_ptr_holder() {} // needed for cereal
  shared_ptr_holder(T *obj) : sp_(obj) {}
  shared_ptr_holder(std::shared_ptr<T> &sp_other) {sp_=  sp_other;}
  ~shared_ptr_holder() {
#ifdef SHOW_SHARED_PTR_DESTRUCTORS
    std::cout<<"Destroying shared_ptr_holder.";
    if(sp_.unique()) {
      std::cout<<" This should destroy the underlying nCompiler object."<<std::endl;
    } else {
      std::cout<<" This won't yet destroy the underlying nCompiler object. It is not unique."<<std::endl;
    }
#endif
  }
  template<class Archive>
  void _SERIALIZE_(Archive &archive) {
    archive(cereal::base_class<shared_ptr_holder_base>(this), sp_);
  }
};

#endif
