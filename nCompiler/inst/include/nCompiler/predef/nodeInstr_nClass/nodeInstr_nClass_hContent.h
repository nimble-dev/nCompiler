/* OPENER (Do not edit this comment) */
#ifndef __nodeInstr_nClass_H
#define __nodeInstr_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class nodeInstr_nClass : public interface_resolver< genericInterfaceC<nodeInstr_nClass> >, public loadedObjectHookC<nodeInstr_nClass> {
public:
      nodeInstr_nClass (  ) ;
  Eigen::Tensor<int, 1> methodInstr;
  nList<Eigen::Tensor<int, 1> > indsInstrVec;
};

    SEXP  new_nodeInstr_nClass (  ) ;

    void  set_CnClass_env_nodeInstr_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_nodeInstr_nClass (  ) ;


#endif
