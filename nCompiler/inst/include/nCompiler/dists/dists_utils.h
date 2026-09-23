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
#include <R_ext/Error.h> // Rf_warning, used by MATHLIB_WARNING below
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

// Mirrors src/Utils.h's ML_ERROR/ML_ERR_return_NAN, guarded so that if
// Utils.h has already been included in this translation unit (as it is via
// dists.h when building the package DLL), its definitions win and these are
// skipped. This is currently a hollow shell -- the ME_DOMAIN case (the only
// one dist kernels use today) never reaches MATHLIB_WARNING -- but it's
// kept in this form, not collapsed to "return ML_NAN;", so that filling in
// real warnings for other error classes later only requires editing here
// (and in Utils.h), not re-auditing every dist kernel that calls it.
#ifndef MATHLIB_WARNING
#define MATHLIB_WARNING(fmt,x) Rf_warning(fmt,x)
#endif

// These are originally defined in Utils.h
// For on-the-fly compilation these will be read?

#ifndef ME_NONE
#define ME_NONE       0
#define ME_DOMAIN     1
#define ME_RANGE      2
#define ME_NOCONV     4
#define ME_PRECISION  8
#define ME_UNDERFLOW  16
#endif

#ifndef ML_ERROR
#define ML_ERROR(x, s) {                        \
   if(x > ME_DOMAIN) { \
       const char *msg = ""; \
       switch(x) { \
       case ME_DOMAIN: \
	   msg = "argument out of domain in '%s'\n"; \
	   break; \
       case ME_RANGE: \
	   msg = "value out of range in '%s'\n"; \
	   break; \
       case ME_NOCONV: \
	   msg = "convergence failed in '%s'\n"; \
	   break; \
       case ME_PRECISION: \
	   msg = "full precision may not have been achieved in '%s'\n"; \
	   break; \
       case ME_UNDERFLOW: \
	   msg = "underflow occurred in '%s'\n"; \
	   break; \
       } \
       MATHLIB_WARNING(msg, s); \
   } \
}
#endif

#ifndef ML_ERR_return_NAN
#define ML_ERR_return_NAN { ML_ERROR(ME_DOMAIN, ""); return R_NaN; }
#endif

// Mirrors src/Utils.h's R_D_forceint/R_D_nonint/R_D_negInonint (from the
// "dpq.h" section), as plain inline functions rather than macros since
// none of the three reference give_log/log_p. Guarded the same way as the
// ML_ERROR family above, so Utils.h's macro versions win when this header
// is included alongside it (they'd otherwise clash: Utils.h defines these
// as object-like function macros, which would mangle these very
// definitions via textual substitution if both were active).
#ifndef R_D_forceint
inline double R_D_forceint(double x) { return floor(x + 0.5); }
#endif

#ifndef R_D_nonint
inline bool R_D_nonint(double x) { return fabs(x - floor(x + 0.5)) > 1e-7; }
#endif

#ifndef R_D_negInonint
inline bool R_D_negInonint(double x) { return x < 0. || R_D_nonint(x); }
#endif

#endif // _NCOMPILER_DISTS_UTILS
