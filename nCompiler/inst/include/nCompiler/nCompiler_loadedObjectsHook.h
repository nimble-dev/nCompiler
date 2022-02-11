#ifndef _NCOMPILER_LOADED_OBJECTS_HOOK
#define _NCOMPILER_LOADED_OBJECTS_HOOK

#include<Rinternals.h>

class loadedObjectHookBaseC {
public:
  
  virtual SEXP base_setup_R_return_object(SEXP Xptr)=0;
};

template <class T>
class loadedObjectHookC : public loadedObjectHookBaseC {
public:
  static SEXP CnClass_env;
  static void set_CnClass_env(SEXP env) {CnClass_env = env;}
  static SEXP setup_R_return_object(SEXP Xptr) {
    Rcpp::Environment nc("package:nCompiler");
    Rcpp::Function newLOE = nc["new.loadedObjectEnv"];
    return newLOE(Xptr, CnClass_env);
  };
  
  SEXP base_setup_R_return_object(SEXP Xptr) {
    Rcpp::Environment nc("package:nCompiler");
    Rcpp::Function newLOE = nc["new.loadedObjectEnv"];
    return newLOE(Xptr, CnClass_env);
  };
};

template<class T>
SEXP loadedObjectHookC<T>::CnClass_env;

#define CREATE_NEW_NCOMP_OBJECT(NCLASS_) \
loadedObjectHookC<NCLASS_>::setup_R_return_object(new_nCompiler_object<NCLASS_>())

#define SET_CNCLASS_ENV(NCLASS_, ENV_) \
loadedObjectHookC<NCLASS_>::set_CnClass_env(ENV_)

#endif