#ifndef _NCOMP_SERIALIZATION_MGR
#define _NCOMP_SERIALIZATION_MGR

#include "nCompiler_class_interface.h"

using namespace std;


/**
   @brief Wraps pointer to C++ object.
 */

typedef genericInterfaceBaseC* PtrType;
struct CerealWrapper {
  const PtrType cPointer; ///<
  CerealWrapper(PtrType cPointer_) :
    cPointer(cPointer_) {
  }
};


class CerealUnique {
  unordered_map<PtrType, size_t> indexMap;
  vector<unique_ptr<CerealWrapper>> uniqueRef;

public:
  /**
     @brief Appends an external pointer to the map, if new.
   */
  size_t addPtr(PtrType extPtr) {
    unordered_map<PtrType, size_t>::iterator itr = indexMap.find(extPtr);
    if (itr == indexMap.end()) {
      size_t vecTop = uniqueRef.size();
      indexMap.insert(make_pair(extPtr, vecTop));
      uniqueRef.emplace_back(make_unique<CerealWrapper>(extPtr));
      return vecTop;
    }
    else {
      return itr->second;
    }
  }
};


/**
   @brief C++ serialization prototype, mimicking design of 'nClass' for simplicity and re-use.
 */
class serialization_mgr : public genericInterfaceC<serialization_mgr> { ///< CRTP
  CerealUnique cerealUnique;

public:
  typedef unique_ptr<genericInterfaceBaseC> unique_base_ptr;
  vector< unique_base_ptr > cSerialands; ///< Core-side cached objects.

  /**
     @brief Caches an object for serialization.

     @param Sextptr is a raw expression pointer to the object being cached.

     @return index into vector of the serialized cSerialands.
   */
  int add_extptr(SEXP Sextptr) {
    return cerealUnique.addPtr(reinterpret_cast<genericInterfaceBaseC*>(reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Sextptr))->get_ptr()));
  }

  /**
     @brief Looks up serialized object by index.

     @return R-style expression pointer to serialized object.
   */
  SEXP get_extptr(int i) {
    SEXP Sans = PROTECT((cSerialands[i].release())->make_deserialized_return_SEXP());
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
	  CEREAL_NVP(cSerialands)
	);
};


SEXP  serialization_mgr::make_deserialized_return_SEXP (  )  {
  shared_ptr<serialization_mgr> shared(this);
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
				      method("add_extptr", &serialization_mgr::add_extptr),
				      method("get_extptr", &serialization_mgr::get_extptr)
				      )
		    )


#endif
