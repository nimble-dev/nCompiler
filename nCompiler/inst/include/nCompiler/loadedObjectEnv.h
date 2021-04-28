#ifndef _LOADED_OBJECT_ENV
#define _LOADED_OBJECT_ENV
#include <Rcpp.h>

// This may be somewhat confusing.
// We have two ways to link to the function loadedObjectEnv.
//
// The first is to use the nCompLocal package.
// In R files, this is created and found by requireLocalDLLpackage() and
// other functions in local_DLL.R
// In that case, the definition of loadedObjectEnv is compiled in nCompLocal.
// On-the-fly compilations then link to that.
// This system is a placeholder for the general need to have one-time locally compiled
// functions that on-the-fly compilations can be linked to.  That was a need learned
// from experience with nimble.
//
// The second is to not use the nCompLocal package.
// This arose because we enountered fussiness getting nCompLocal to work
// on different OSes.
// In this case, the definition of loadedObjectEnv is generated into
// the on-the-fly code, and hence the defintion for loadedObjectEnv is right
// there.
//
// Choice of which system to use is controlled by
// set_nOption("use_nCompLocal", < TRUE for first option | FALSE for second option> )
//
// The reason not to have the definition hard-coded here (but rather generated)
// is that the linker seems to not find the loadedObjectEnv.  That may have to do
// with Rcpp's compilation scheme.

SEXP loadedObjectEnv(SEXP Xptr);

#endif
