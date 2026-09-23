// Umbrella header for nCompiler's distribution kernels, for use from
// nCompile-generated code. Pulled into nCompiler_omnibus.h under
// #ifdef NCOMPILER_USES_MVDISTS.
//
// Individual dist headers are self-contained (each brings its own Eigen
// includes, etc.), so this file's only job is to gather them in one place.
// As more dist kernels are added, they just get an #include line here.

#ifndef _NCOMPILER_DISTS_POST_RCPP
#define _NCOMPILER_DISTS_POST_RCPP

#include <nCompiler/dists/dmnorm_chol.h>
#include <nCompiler/dists/dmnorm_chol_tensor.h>

#endif // _NCOMPILER_DISTS_POST_RCPP
