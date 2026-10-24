/* OPENER (Do not edit this comment) */
#ifndef __sparseCholFactor_H
#define __sparseCholFactor_H
/* BODY (Do not edit this comment) */
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#include <Rinternals.h>

class sparseCholFactor : public interface_resolver< genericInterfaceC<sparseCholFactor> >, public loadedObjectHookC<sparseCholFactor> {
public:
      sparseCholFactor (  ) ;
  Eigen::SimplicialLLT<Eigen::SparseMatrix<double>> llt;
};

    SEXP  new_sparseCholFactor (  ) ;

    void  set_CnClass_env_sparseCholFactor ( SEXP env ) ;

    Rcpp::Environment  get_CnClass_env_sparseCholFactor (  ) ;

#include <nCompiler/ET_ext/post_Rcpp/tensorOperations_sparseChol.h>

#endif
