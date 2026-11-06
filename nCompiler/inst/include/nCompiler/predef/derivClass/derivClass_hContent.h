/* OPENER (Do not edit this comment) */
#ifndef __derivClass_H
#define __derivClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class derivClass : public interface_resolver< genericInterfaceC<derivClass> >, public loadedObjectHookC<derivClass> {
public:
      derivClass (  ) ;
  Eigen::Tensor<double, 1> value;
  Eigen::Tensor<double, 2> gradient;
  Eigen::Tensor<double, 3> hessian;
};

    SEXP  new_derivClass (  ) ;

    void  set_CnClass_env_derivClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_derivClass (  ) ;


#endif
