// This file is intended to be copied to the inst/staticLib directory of the nCompLocal package.
// nCompLocal is a small package that nComp will install.
#include "nCompiler/loadedObjectEnv.h"

// [[Rcpp::depends(nCompiler)]]

SEXP loadedObjectEnv(SEXP Xptr) {
  Rcpp::Environment nc("package:nCompiler");
  Rcpp::Function newLOE = nc["new.loadedObjectEnv"];
  return newLOE(Xptr);
}
