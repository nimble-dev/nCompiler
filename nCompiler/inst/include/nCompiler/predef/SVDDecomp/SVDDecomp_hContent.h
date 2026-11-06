/* OPENER (Do not edit this comment) */
#ifndef __SVDDecomp_H
#define __SVDDecomp_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class SVDDecomp : public interface_resolver< genericInterfaceC<SVDDecomp> >, public loadedObjectHookC<SVDDecomp> {
public:
      SVDDecomp (  ) ;
  Eigen::Tensor<double, 1> d;
  Eigen::Tensor<double, 2> v;
  Eigen::Tensor<double, 2> u;
};

    SEXP  new_SVDDecomp (  ) ;

    void  set_CnClass_env_SVDDecomp ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_SVDDecomp (  ) ;

#include <nCompiler/ET_ext/post_Rcpp/tensorOperations_SVD.h>

#endif
