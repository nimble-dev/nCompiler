/* OPENER (Do not edit this comment) */
#ifndef __nListBase_nClass_H
#define __nListBase_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include "nListBase_nClass_c_.h"

class nListBase_nClass : public interface_resolver< genericInterfaceC<nListBase_nClass> >, public loadedObjectHookC<nListBase_nClass> {
public:
   virtual  bool  ping (  ) ;
   virtual  int  setLength ( int length ) ;
   virtual  int  getLength (  ) ;
   virtual  std::shared_ptr<genericInterfaceBaseC>  get_interface_ptr_at ( int i ) ;
   virtual  std::unique_ptr<ETaccessorBase>  access_at ( int i ) ;
      nListBase_nClass (  ) ;

};

    void  set_CnClass_env_nListBase_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_nListBase_nClass (  ) ;

#include <nCompiler/predef/nList_/nList_.h>

#endif
