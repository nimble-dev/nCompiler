#ifndef __test_predefined_H2
#define __test_predefined_H2
#define NCOMPILER_HANDLE_EIGEN_ERRORS
#define NCOMPILER_USES_EIGEN
#define NCOMPILER_USES_NCLASS_INTERFACE
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>
#include <nCompiler/nCompiler_omnibus_first_h.h>

class test_predefined : public genericInterfaceC<test_predefined>, public loadedObjectHookC<test_predefined> {
public:
  test_predefined (  );
  double a;
};

SEXP  new_test_predefined (  );

void  set_CnClass_env_test_predefined ( SEXP env );


#endif
