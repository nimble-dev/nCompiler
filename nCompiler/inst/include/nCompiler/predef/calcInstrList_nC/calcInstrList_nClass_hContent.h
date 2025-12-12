/* OPENER (Do not edit this comment) */
#ifndef __calcInstrList_nClass_H
#define __calcInstrList_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "calcInstr_nClass_c_.h"

class calcInstrList_nClass : public interface_resolver< genericInterfaceC<calcInstrList_nClass> >, public loadedObjectHookC<calcInstrList_nClass> {
public:
      calcInstrList_nClass (  ) ;
  nList<std::shared_ptr<calcInstr_nClass> > calcInstrList;
};

    SEXP  new_calcInstrList_nClass (  ) ;

    void  set_CnClass_env_calcInstrList_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_calcInstrList_nClass (  ) ;


#endif
