/* OPENER (Do not edit this comment) */
#ifndef __nodeFxnBase_nClass_H
#define __nodeFxnBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nodeInstr_nClass_c_.h"

class nodeFxnBase_nClass : public interface_resolver< genericInterfaceC<nodeFxnBase_nClass> >, public loadedObjectHookC<nodeFxnBase_nClass> {
public:
   virtual  bool  ping (  ) ;
   virtual  double  calculate ( std::shared_ptr<nodeInstr_nClass> nodeInstr ) ;
      nodeFxnBase_nClass (  ) ;
};

    void  set_CnClass_env_nodeFxnBase_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_nodeFxnBase_nClass (  ) ;

#include <nCompiler/predef/nodeFxnClass_/nodeFxnClass_.h>

#endif
