#ifndef _NCOMP_SERIALIZATION_MGR
#define _NCOMP_SERIALIZATION_MGR

#include "nCompiler_class_interface.h"


/**
   @brief C++ serialization prototype, mimicking design of 'nClass' for simplicity and re-use.
 */
class serialization_mgr : public genericInterfaceC<serialization_mgr> { ///< CRTP
public:
  typedef std::unique_ptr<genericInterfaceBaseC> unique_base_ptr;
  std::vector< unique_base_ptr > objects; ///< Core-side cached objects.

  /**
     @brief Caches an object for serialization.

     @param Sextptr is a raw expression pointer to the object being cached.

     @return index into vector of the serialized objects.
   */
  int add_extptr(SEXP Sextptr) {
    std::cout<< "Greetings from add_extptr" << endl;
    return 0;

    objects.emplace_back(reinterpret_cast<genericInterfaceBaseC*>(reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Sextptr))->get_ptr()));

    return objects.size() - 1;
  }

  /**
     @brief Looks up serialized object by index.

     @return R-style expression pointer to bserialized object.
   */
  SEXP get_extptr(int i) {
    SEXP Sans = PROTECT((objects[i].release())->make_deserialized_return_SEXP());
    UNPROTECT(1);
    return(Sans);
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
archive(
cereal::base_class<genericInterfaceC<serialization_mgr> >(this),
CEREAL_NVP(objects)
	);};

SEXP  serialization_mgr::make_deserialized_return_SEXP (  )  {
std::shared_ptr<serialization_mgr> shared(this);
SEXP Sans = PROTECT(return_nCompiler_object<serialization_mgr>(shared));
UNPROTECT(1);
return Sans;
}

// This needs to be in code-generated C++ for the Rcpp::export annotation to be picked up by Rcpp 
/* // [[Rcpp::export]] */
//SEXP new_serialization_mgr ( ) {
//return(loadedObjectEnv(new_nCompiler_object<serialization_mgr>()));
//}

CEREAL_REGISTER_TYPE(serialization_mgr)

CEREAL_REGISTER_DYNAMIC_INIT(serialization_mgr)

NCOMPILER_INTERFACE(
		    serialization_mgr,
		    NCOMPILER_FIELDS(
				     ),
		    NCOMPILER_METHODS(
				      method("add_extptr",
					     &serialization_mgr::add_extptr),
				      method("get_extptr",
					     &serialization_mgr::get_extptr)
				      )
		    )


#endif
