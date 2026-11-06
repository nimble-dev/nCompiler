/* OPENER (Do not edit this comment) */
#ifndef __EigenDecomp_H
#define __EigenDecomp_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class EigenDecomp : public interface_resolver< genericInterfaceC<EigenDecomp> >, public loadedObjectHookC<EigenDecomp> {
public:
      EigenDecomp (  ) ;
  Eigen::Tensor<double, 1> values;
  Eigen::Tensor<double, 2> vectors;
};

    SEXP  new_EigenDecomp (  ) ;

    void  set_CnClass_env_EigenDecomp ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_EigenDecomp (  ) ;

#include <nCompiler/ET_ext/post_Rcpp/tensorOperations_Eigen.h>

#endif
