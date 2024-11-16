#ifndef LOADEDOBJECTHOOKC_H_
#define LOADEDOBJECTHOOKC_H_

template <class T>
class loadedObjectHookC : public loadedObjectHookBaseC {
public:
  void hw() {};
  ~loadedObjectHookC() {};
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
#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {
    archive(cereal::base_class<loadedObjectHookBaseC>(this));
  }
#endif
};

template<class T>
Rcpp::Environment loadedObjectHookC<T>::CnClass_env;

#endif // LOADEDOBJECTHOOKC_H_
