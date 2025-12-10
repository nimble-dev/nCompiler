/* OPENER (Do not edit this comment) */
#ifndef __modelBase_nClass_H
#define __modelBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include <nodeFxnBase_nClass_c_.h>
#include <calcInstrList_nClass_c_.h>
#include "calcInstrList_nClass_c_.h"

class modelBase_nClass : public interface_resolver< genericInterfaceC<modelBase_nClass> >, public loadedObjectHookC<modelBase_nClass> {
public:
   virtual  bool  ping (  ) ;
   virtual  double  calculate ( std::shared_ptr<calcInstrList_nClass> calcInstrList ) ;
      modelBase_nClass (  ) ;
};

    void  set_CnClass_env_modelBase_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_modelBase_nClass (  ) ;

#include <nCompiler/predef/modelClass_/modelClass_.h>

#endif
