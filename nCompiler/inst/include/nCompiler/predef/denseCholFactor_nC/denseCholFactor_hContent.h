/* OPENER (Do not edit this comment) */
#ifndef __denseCholFactor_H
#define __denseCholFactor_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class denseCholFactor : public interface_resolver< genericInterfaceC<denseCholFactor> >, public loadedObjectHookC<denseCholFactor> {
public:
      denseCholFactor (  ) ;
  Eigen::LLT<Eigen::MatrixXd> llt;
};

    SEXP  new_denseCholFactor (  ) ;

    void  set_CnClass_env_denseCholFactor ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_denseCholFactor (  ) ;

#include <nCompiler/ET_ext/post_Rcpp/tensorOperations_denseChol.h>

#endif
