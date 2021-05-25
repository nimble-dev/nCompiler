#ifndef __SHARED_PTR_HOLDER
#define __SHARED_PTR_HOLDER

#define SHOW_SHARED_PTR_DESTRUCTORS

class shared_ptr_holder_base {
 public:
  virtual void *get_ptr() const {
   std::cout<<"Error: you should be in a derived shared_ptr_holder class get_ptr().  This is the base class."<<std::endl;
    return(0);
  };
  virtual ~shared_ptr_holder_base() {};
};

template<typename T>
class shared_ptr_holder: public shared_ptr_holder_base {
 public:
  std::shared_ptr<T> sp_;
  void *get_ptr() const {
    return static_cast<void*>(sp_.get());
  }
  std::shared_ptr<T> &sp() {return sp_;}
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
};

#endif
