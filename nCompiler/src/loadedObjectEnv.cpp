// This file is intended to be copied to the inst/staticLib directory of the nCompLocal packaage.
// nimCompLocal is a small package that nComp will install.
#include "nCompiler/loadedObjectEnv.h"

SEXP loadedObjectEnv(SEXP Xptr) {
  Rcpp::Environment nc("package:nCompiler");
  Rcpp::Function newLOE = nc["new.loadedObjectEnv"];
  return newLOE(Xptr);
}
