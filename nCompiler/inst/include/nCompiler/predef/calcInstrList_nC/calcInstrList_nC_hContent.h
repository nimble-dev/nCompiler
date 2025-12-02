/* OPENER (Do not edit this comment) */
#ifndef __calcInstrList_nC_H
#define __calcInstrList_nC_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include <calcInstr_nClass_c_.h>

class calcInstrList_nC : public interface_resolver< genericInterfaceC<calcInstrList_nC> >, public loadedObjectHookC<calcInstrList_nC> {
public:
      calcInstrList_nC (  ) ;
  nList<std::shared_ptr<calcInstr_nClass> > calcInstrList;
};

    SEXP  new_calcInstrList_nC (  ) ;

    void  set_CnClass_env_calcInstrList_nC ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_calcInstrList_nC (  ) ;


#endif
