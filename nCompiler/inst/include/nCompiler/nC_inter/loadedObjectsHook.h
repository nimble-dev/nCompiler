#ifndef _NCOMPILER_LOADED_OBJECTS_HOOK
#define _NCOMPILER_LOADED_OBJECTS_HOOK

#include<Rinternals.h>
#ifdef NCOMPILER_USES_CEREAL
#include<nCompiler/nC_cereal/archives.h>
#endif

class loadedObjectHookBaseC {
 public:
  virtual void hw()=0;
  virtual ~loadedObjectHookBaseC() {};
#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {}
#endif
};

template <class T>
class loadedObjectHookC;


#define CREATE_NEW_NCOMP_OBJECT(NCLASS_) \
loadedObjectHookC<NCLASS_>::setup_R_return_object(new_nCompiler_object<NCLASS_>())

#define RETURN_THIS_NCOMP_OBJECT(NCLASS_) \
  std::shared_ptr<NCLASS_> SHARED_(this); \
return loadedObjectHookC<NCLASS_>::setup_R_return_object(return_nCompiler_object<NCLASS_>(SHARED_))

#define SET_CNCLASS_ENV(NCLASS_, ENV_) \
loadedObjectHookC<NCLASS_>::set_CnClass_env(ENV_)

#define GET_CNCLASS_ENV(NCLASS_) \
loadedObjectHookC<NCLASS_>::get_CnClass_env()


#endif
