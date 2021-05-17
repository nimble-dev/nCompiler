#ifndef _NCOMP_SERIALIZATION_MGR
#define _NCOMP_SERIALIZATION_MGR

#include "nCompiler_class_interface.h"

using namespace std;


typedef genericInterfaceBaseC BaseType;

/**
   @brief Wraps pointer to C++ object in smart pointer, as required by Cereal.
 */
struct CerealWrapper {
  unique_ptr<BaseType> uPtr; ///< smart pointer shares temporary ownership.

  CerealWrapper(BaseType* cPointer_) :
    uPtr(unique_ptr<BaseType>(cPointer_)) {
  }

  CerealWrapper() = default; ///< Needed by Cereal for vector construction.

  // Forces move constructor to be employed for vector reallocation.
  CerealWrapper(const CerealWrapper&) = delete; ///< No copy constructor.

  CerealWrapper(CerealWrapper&&) = default;

  /**
     @brief Relinquishes temporary ownership.
   */
  ~CerealWrapper() {
    (void) uPtr.release();
  }


  BaseType* getPtr() const {
    uPtr.get();
  }


  /**
     @brief Releases underlying pointer.
   */
  BaseType* release() {
    return uPtr.release();
  }

  
  template<class Archive> void _SERIALIZE_ ( Archive & archive );
};


template void CerealWrapper::_SERIALIZE_(cereal::BinaryOutputArchive &archive);
template void CerealWrapper::_SERIALIZE_(cereal::BinaryInputArchive &archive);

CEREAL_FORCE_DYNAMIC_INIT(CerealWrapper)

template<class Archive>
void CerealWrapper::_SERIALIZE_ ( Archive & archive ) {
  archive(
	  CEREAL_NVP(uPtr)
	);
}


/**
   @brief Maintains unique map of pointer targets.
 */
class CerealUnique {
  unordered_map<const BaseType*, size_t> indexMap;

public:

  /**
     @brief Appends an external pointer to the map, if new.
   */
  size_t addPtr(BaseType* extPtr,
		class serialization_mgr* mgr);
};


/**
   @brief C++ serialization prototype, mimicking design of 'nClass' for simplicity and re-use.
 */
class serialization_mgr : public genericInterfaceC<serialization_mgr> { ///< CRTP
  CerealUnique cerealUnique;
  vector<CerealWrapper> cSerialand;

public:

  /**
     @brief Caches an object for serialization.

     @param Sextptr is a raw expression pointer to the object being cached.

     @return index into vector of the serializable references.
   */
  int add_extptr(SEXP Sextptr) {
    return cerealUnique.addPtr(reinterpret_cast<genericInterfaceBaseC*>(reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Sextptr))->get_ptr()), this);
  }


  void addSerialand(genericInterfaceBaseC* extPtr) {
    cSerialand.emplace_back(extPtr);
  }
  

  /**
     @brief Looks up serialized object by index.

     @return R-style expression pointer to serialized object.
   */
  SEXP get_extptr(int i) {
    SEXP Sans = PROTECT((cSerialand[i].release())->make_deserialized_return_SEXP());
    UNPROTECT(1);
    return(Sans);
  }

  template<class Archive>
    void _SERIALIZE_ ( Archive & archive );
  
  SEXP make_deserialized_return_SEXP ( ) {
    shared_ptr<serialization_mgr> shared(this);
    SEXP Sans = PROTECT(return_nCompiler_object<serialization_mgr>(shared));
    UNPROTECT(1);

    return Sans;
  }
};


size_t CerealUnique::addPtr(BaseType* extPtr,
			    serialization_mgr* mgr) {
  typename unordered_map<const BaseType*, size_t>::iterator itr = indexMap.find(extPtr);
  if (itr == indexMap.end()) {
    indexMap.insert(make_pair(extPtr, indexMap.size()));
    mgr->addSerialand(extPtr);
    return indexMap.size() - 1;
  }
  else {
    return itr->second;
  }
}


template void serialization_mgr::_SERIALIZE_(cereal::BinaryOutputArchive &archive);
template void serialization_mgr::_SERIALIZE_(cereal::BinaryInputArchive &archive);

CEREAL_FORCE_DYNAMIC_INIT(serialization_mgr)

template<class Archive>
void serialization_mgr::_SERIALIZE_ ( Archive & archive ) {
  archive(
	  cereal::base_class<genericInterfaceC<serialization_mgr> >(this),
	  CEREAL_NVP(cSerialand)
	);
};

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
				      method("add_extptr", &serialization_mgr::add_extptr),
				      method("get_extptr", &serialization_mgr::get_extptr)
				      )
		    )


#endif
