#ifndef _NCOMPILER_LOADED_OBJECTS_HOOK
#define _NCOMPILER_LOADED_OBJECTS_HOOK

#include<Rinternals.h>
#include<nCompiler/nCompiler_cereal.h>

class loadedObjectHookBaseC {
 public:
  //  virtual SEXP base_setup_R_return_object(SEXP Xptr)=0;
  virtual void hw()=0;
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {}
};

template <class T>
class loadedObjectHookC : public loadedObjectHookBaseC {
public:
  void hw() {};
  static Rcpp::Environment CnClass_env;
  static void set_CnClass_env(Rcpp::Environment env) {CnClass_env = env;}
  static SEXP setup_R_return_object_full(SEXP Xptr) {
    Rcpp::Environment nc("package:nCompiler");
    Rcpp::Function newLOE(nc["new.loadedObjectEnv_full"]);
    return newLOE(Xptr, CnClass_env);
  };
  static SEXP setup_R_return_object(SEXP Xptr) {
    Rcpp::Environment nc("package:nCompiler");
    Rcpp::Function newLOE(nc["new.loadedObjectEnv"]);
    return newLOE(Xptr, CnClass_env);
  };  
  /* SEXP base_setup_R_return_object(SEXP Xptr) { */
  /*   Rcpp::Environment nc("package:nCompiler"); */
  /*   Rcpp::Function newLOE = nc["new.loadedObjectEnv"]; */
  /*   return newLOE(Xptr, CnClass_env); */
  /* }; */
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {
    archive(cereal::base_class<loadedObjectHookBaseC>(this));
  }
};

template<class T>
Rcpp::Environment loadedObjectHookC<T>::CnClass_env;

#define CREATE_NEW_NCOMP_OBJECT(NCLASS_) \
loadedObjectHookC<NCLASS_>::setup_R_return_object(new_nCompiler_object<NCLASS_>())

#define RETURN_THIS_NCOMP_OBJECT(NCLASS_) \
  std::shared_ptr<NCLASS_> SHARED_(this); \
return loadedObjectHookC<NCLASS_>::setup_R_return_object(return_nCompiler_object<NCLASS_>(SHARED_))

// #define RETURN_THIS_NCOMP_OBJECT(NCLASS_, OBJ) \
// return return_nCompiler_object<NCLASS_>(OBJ_)

#define SET_CNCLASS_ENV(NCLASS_, ENV_) \
loadedObjectHookC<NCLASS_>::set_CnClass_env(ENV_)

#endif
