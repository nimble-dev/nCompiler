#ifndef _NCOMPILER_LOADED_OBJECTS_HOOK
#define _NCOMPILER_LOADED_OBJECTS_HOOK

#include<Rinternals.h>
#ifdef NCOMPILER_USES_CEREAL
#include<nCompiler/nClass_cereal/archives.h>
#endif

class loadedObjectHookBaseC {
 public:
  //  virtual SEXP base_setup_R_return_object(SEXP Xptr)=0;
  virtual void hw()=0;
  virtual ~loadedObjectHookBaseC() {};
#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {}
#endif
};

// class CnC_env_holderC;

// class CnC_env_holderC {
//   public:
//   static Rcpp::Environment CnClass_env;

// };

template <class T>
class loadedObjectHookC;

// template <class T>
// class loadedObjectHookC : public loadedObjectHookBaseC {
// public:
//   void hw() {};
//   ~loadedObjectHookC() {};
//   static Rcpp::Environment CnClass_env;
//   static void set_CnClass_env(Rcpp::Environment env) {CnClass_env = env;}
//   static SEXP setup_R_return_object_full(SEXP Xptr) {
//     Rcpp::Environment nc("package:nCompiler");
//     Rcpp::Function newLOE(nc["new.loadedObjectEnv_full"]);
//     return newLOE(Xptr, CnClass_env);
//   };
//   static SEXP setup_R_return_object(SEXP Xptr) {
//     Rcpp::Environment nc("package:nCompiler");
//     Rcpp::Function newLOE(nc["new.loadedObjectEnv"]);
//     return newLOE(Xptr, CnClass_env);
//   };
//   /* SEXP base_setup_R_return_object(SEXP Xptr) { */
//   /*   Rcpp::Environment nc("package:nCompiler"); */
//   /*   Rcpp::Function newLOE = nc["new.loadedObjectEnv"]; */
//   /*   return newLOE(Xptr, CnClass_env); */
//   /* }; */
// #ifdef NCOMPILER_USES_CEREAL
//   template<class Archive>
//     void _SERIALIZE_(Archive &archive) {
//     archive(cereal::base_class<loadedObjectHookBaseC>(this));
//   }
// #endif
// };

// template<class T>
// Rcpp::Environment loadedObjectHookC<T>::CnClass_env;

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
