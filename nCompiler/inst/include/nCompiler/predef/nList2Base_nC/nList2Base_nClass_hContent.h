/* OPENER (Do not edit this comment) */
#ifndef __nList2Base_nClass_H
#define __nList2Base_nClass_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class nList2Base_nClass : public interface_resolver< genericInterfaceC<nList2Base_nClass> >, public loadedObjectHookC<nList2Base_nClass> {
public:
   virtual  bool  ping (  ) ;
      nList2Base_nClass (  ) ;
};

    void  set_CnClass_env_nList2Base_nClass ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_nList2Base_nClass (  ) ;

#include <nCompiler/predef/nList2_/nList2_.h>


#endif
