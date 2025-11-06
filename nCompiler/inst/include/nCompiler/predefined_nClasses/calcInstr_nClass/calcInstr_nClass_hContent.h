/* OPENER (Do not edit this comment) */
#ifndef __calcInstr_nClass_H
#define __calcInstr_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class calcInstr_nClass : public interface_resolver< genericInterfaceC<calcInstr_nClass> >, public loadedObjectHookC<calcInstr_nClass> {
public:
      calcInstr_nClass (  ) ;
  int nodeIndex;
  nList<std::shared_ptr<nodeInstr_nClass> > nodeInstrVec;
};

    SEXP  new_calcInstr_nClass (  ) ;

    void  set_CnClass_env_calcInstr_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_calcInstr_nClass (  ) ;


#endif
