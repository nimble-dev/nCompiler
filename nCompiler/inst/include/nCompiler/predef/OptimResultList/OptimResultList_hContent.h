/* OPENER (Do not edit this comment) */
#ifndef __OptimResultList_H
#define __OptimResultList_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class OptimResultList : public interface_resolver< genericInterfaceC<OptimResultList> >, public loadedObjectHookC<OptimResultList> {
public:
      OptimResultList (  ) ;
  Eigen::Tensor<double, 1> par;
  double value;
  Eigen::Tensor<double, 2> hessian;
  Eigen::Tensor<int, 1> counts;
  int convergence;
  Rcpp::CharacterVector message;
};

    SEXP  new_OptimResultList (  ) ;

    void  set_CnClass_env_OptimResultList ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_OptimResultList (  ) ;


#endif
