#ifndef __nClass_1_H
#define __nClass_1_H
#define NCOMPILER_HANDLE_EIGEN_ERRORS
#define NCOMPILER_USES_EIGEN
#define NCOMPILER_USES_NCLASS_INTERFACE
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
//#include <nCompiler/nCompiler_Eigen.h>
//#include <nCompiler/nCompiler_TBB.h>
//#include <nCompiler/nCompiler_core.h>
//#include <nCompiler/nCompiler_loadedObjectsHook.h>
//#include <nCompiler/nCompiler_class_interface.h>

class nClass_1 : public genericInterfaceC<nClass_1>, public loadedObjectHookC<nClass_1> {
public:
  nClass_1 (  );
  double x;
};

SEXP  new_nClass_1 (  );

void  set_CnClass_env_nClass_1 ( SEXP env );

#endif
