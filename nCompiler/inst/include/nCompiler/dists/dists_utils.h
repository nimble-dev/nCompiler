// Small helpers used by nCompiler's distribution kernels (ported from
// nimble's dists.cpp). Kept header-only and inline, rather than living only
// in src/dists.cpp, so the same implementation can be compiled both into
// the package DLL and into code nCompile generates and compiles on a
// user's machine -- see dmnorm_chol.h for why that split is necessary.

#ifndef _NCOMPILER_DISTS_UTILS
#define _NCOMPILER_DISTS_UTILS

// R_IsNA, R_IsNaN, ISNAN, and R_FINITE are declared in R_ext/Arith.h, not
// Rmath.h; NA_REAL, R_NaN, and R_NegInf (used by dmnorm_chol.h) live there
// too.
#include <R_ext/Arith.h>
#include <Rmath.h>

// Detects NA
inline bool R_IsNA_ANY(double* P, int s) {
  for(int i = 0; i < s; ++i) if(R_IsNA(P[i])) return(true);
  return(false);
}

// Detects NaN
inline bool R_IsNaN_ANY(double* P, int s) {
  for(int i = 0; i < s; ++i) if(R_IsNaN(P[i])) return(true);
  return(false);
}

// Detects NA or NaN
inline bool ISNAN_ANY(double* P, int s) {
  for(int i = 0; i < s; ++i) if(ISNAN(P[i])) return(true);
  return(false);
}

inline bool R_FINITE_ANY(double* P, int s) {
  for(int i = 0; i < s; ++i) if(!R_FINITE(P[i])) return(false);
  return(true);
}

#endif // _NCOMPILER_DISTS_UTILS
