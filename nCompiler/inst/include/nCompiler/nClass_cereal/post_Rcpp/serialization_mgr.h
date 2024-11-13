#ifndef _NCOMP_SERIALIZATION_MGR
#define _NCOMP_SERIALIZATION_MGR

#include <nCompiler/nClass_cereal/archives.h>
#include <nCompiler/nClass_cereal/Rcpp_types_cereal.h>
#include <nCompiler/nClass_interface/generic_class_interface.h>
#include <nCompiler/nClass_interface/loadedObjectsHook.h>
#include <nCompiler/nClass_interface/nClass_factory.h>
// Rough draft of C++ serialization manager class
// I want to make this as similar as possible to the design of an nClass
// for simplicity and code re-use.
class serialization_mgr : public genericInterfaceC<serialization_mgr>, loadedObjectHookC<serialization_mgr> {
public:
  typedef std::shared_ptr<shared_ptr_holder_base> ptr_holder_ptr; // use shared_ptr so we can error trap on non-uniqueness. each one *should* be unique.
  std::vector< ptr_holder_ptr > objects;
  // register an extptr for its object to be included in serialization
  int add_extptr(SEXP Sextptr) {
    shared_ptr_holder_base *base_obj = reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Sextptr));
    // make a shared_ptr_holder that doesn't destroy the underlying object
    // because it uses the same shared_ptr lineage but has no R finalizer registered.
    shared_ptr_holder_base *shared_base_obj = base_obj->make_shared_ptr_holder();
    std::cout<<"***adding extptr "<<shared_base_obj->get_ptr() <<" to serialization_mgr\n";
    objects.push_back( ptr_holder_ptr(shared_base_obj) );
    return objects.size() - 1;
  }
  // find the ID of a registered extptr.
  // return -1 if NULL if not available
  class extptr_match {
    public:
    const void *target_ptr;
    extptr_match(void *target_ptr_) : target_ptr(target_ptr_) {}
    bool operator()(const ptr_holder_ptr &compare_obj) {
      std::cout<<"comparing "<<compare_obj->get_ptr()<<" to "<<target_ptr<<" "<< (compare_obj->get_ptr()==target_ptr) <<std::endl;
      return compare_obj->get_ptr()==target_ptr;
    }
  };
  SEXP find_extptr(SEXP Sextptr) {
    shared_ptr_holder_base *base_obj = reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Sextptr));
  //  std::cout<<"*** finding extptr "<<base_obj->get_ptr()<<std::endl;
    auto found = std::find_if(objects.crbegin(), objects.crend(), extptr_match(base_obj->get_ptr()));
    if(found == objects.crend()) return(R_NilValue);
    int ID = objects.size() - (found - objects.crbegin()) - 1;
    std::cout<<"found at ID = "<<ID<<std::endl;
    return(Rcpp::wrap(ID));
  }
  // get the extptr to an object whose ID is known
  SEXP get_extptr(int i) {
    Rprintf("calling get_extptr with i = %i\n", i);
    shared_ptr_holder_base *shared_base_obj = objects[i].get();
    SEXP Sans = shared_base_obj->return_this_nCompiler_object();
    return(Sans);
  }
  void clear() {
    std::cout<<"clearing serialization_mgr objects"<<std::endl;
    for(size_t i = 0; i < objects.size(); ++i) {
      objects[i].reset();
    }
  }
  ~serialization_mgr() {
    clear();
  }
  template<class Archive>
    void _SERIALIZE_ ( Archive & archive );
  SEXP make_deserialized_return_SEXP ( );
};

template void serialization_mgr::_SERIALIZE_(cereal::BinaryOutputArchive &archive);
template void serialization_mgr::_SERIALIZE_(cereal::BinaryInputArchive &archive);

CEREAL_FORCE_DYNAMIC_INIT(serialization_mgr)

template<class Archive>
void serialization_mgr::_SERIALIZE_ ( Archive & archive ) {
  archive(cereal::base_class<genericInterfaceC<serialization_mgr> >(this),
          CEREAL_NVP(objects));
};

SEXP  serialization_mgr::make_deserialized_return_SEXP (  )  {
  std::cout<<"returning new serialization_mgr"<<std::endl;
//  std::shared_ptr<serialization_mgr> shared(this);
  RETURN_THIS_NCOMP_OBJECT(serialization_mgr);//, shared);
//SEXP Sans = PROTECT(return_nCompiler_object<serialization_mgr>(shared));
//UNPROTECT(1);
//return Sans;
}

// This needs to be in code-generated C++ for the Rcpp::export annotation to be picked up by Rcpp 
/* // [[Rcpp::export]] */
//SEXP new_serialization_mgr ( ) {
//return(loadedObjectEnv(new_nCompiler_object<serialization_mgr>()));
//}

CEREAL_REGISTER_TYPE(serialization_mgr)

CEREAL_REGISTER_DYNAMIC_INIT(serialization_mgr);

NCOMPILER_INTERFACE(
                    serialization_mgr,
                    NCOMPILER_FIELDS(
                                     ),
                    NCOMPILER_METHODS(
                                      method("add_extptr",
                                             &serialization_mgr::add_extptr,
                                             args({{arg("extptr",copy)}})),
                                      method("find_extptr",
                                             &serialization_mgr::find_extptr,
                                             args({{arg("extptr",copy)}})),
                                      method("get_extptr",
                                             &serialization_mgr::get_extptr,
                                             args({{arg("id",copy)}})),
                                      method("clear",
                                             &serialization_mgr::clear,
                                             args({{}}))
                                      )
                    )


#endif
