/* OPENER (Do not edit this comment) */
#ifndef __OptimControlList_H
#define __OptimControlList_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class OptimControlList : public interface_resolver< genericInterfaceC<OptimControlList> >, public loadedObjectHookC<OptimControlList> {
public:
    void  nFun_2 (  ) ;
      OptimControlList (  ) ;
  int trace;
  double fnscale;
  Eigen::Tensor<double, 1> parscale;
  Eigen::Tensor<double, 1> ndeps;
  int maxit;
  double abstol;
  double reltol;
  double alpha;
  double beta;
  double gamma;
  int REPORT;
  int type;
  int lmm;
  double factr;
  double pgtol;
  int tmax;
  double temp;
};

    SEXP  new_OptimControlList (  ) ;

    void  set_CnClass_env_OptimControlList ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_OptimControlList (  ) ;


#endif
