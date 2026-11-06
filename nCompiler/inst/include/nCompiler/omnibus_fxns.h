#ifndef _NCOMPILER_CORE
#define _NCOMPILER_CORE

#include <nCompiler/utils.h>
#include <nCompiler/cWiseUnary_external.h>

inline Rcpp::Nullable<Rcpp::Environment> get_env_from_env(const Rcpp::Environment& env, const std::string& name) {
  Rcpp::RObject maybe_env = env[name];
  if(Rcpp::is<Rcpp::Environment>(maybe_env)) {
    return static_cast<SEXP>(maybe_env);
  }
  return R_NilValue;
}

inline SEXP get_extptr_from_env(const Rcpp::Environment& env, const std::string& name) {
  Rcpp::RObject maybe_extptr = env[name];
  SEXP Smaybe_extptr = static_cast<SEXP>(maybe_extptr);
  if(TYPEOF(Smaybe_extptr) == EXTPTRSXP) {
    return Smaybe_extptr;
  }
  return R_NilValue;
}

// This is used from two places.
// First is the shared_ptr<> Exporter in shared_ptr_as_wrap.
// Second is the genericInterfaceC::accessor_class::set function
// In the second case, sometimes we will be calling the Exporter below.
// In that case, the underlying extptr will have already been extracted
// and when the Exporter call this function, it will return the extptr directly
// after the first clause.
inline SEXP get_extptr_from_SEXP(SEXP Svalue) {
  if(TYPEOF(Svalue) == EXTPTRSXP) {
    return Svalue;
  }
  SEXP Sres = R_NilValue; // constructed as NULL
  if(Rcpp::is<Rcpp::Environment>(Svalue)) {
    Rcpp::Environment Senv(Svalue);
    Sres = get_extptr_from_env(Senv, "extptr");
    if(Sres != R_NilValue) {
      return Sres;
    }
    Rcpp::Nullable<Rcpp::Environment> private_env = get_env_from_env(Senv, "private");
    if(private_env.isNotNull()) {
      Rcpp::Nullable<Rcpp::Environment> CpublicObj_env = get_env_from_env(Rcpp::Environment(private_env), "Cpublic_obj");
      if(CpublicObj_env.isNotNull()) {
        Rcpp::Nullable<Rcpp::Environment> private2_env = get_env_from_env(Rcpp::Environment(CpublicObj_env), "private");
        if(private2_env.isNotNull()) {
          Rcpp::Nullable<Rcpp::Environment> LOE_env = get_env_from_env(Rcpp::Environment(private2_env), "CppObj");
          if(LOE_env.isNotNull()) {
            Sres = get_extptr_from_env(Rcpp::Environment(LOE_env), "extptr");
            if(Sres != R_NilValue) {
              return Sres;
            }
          }
        }
      }
    }
  }
  return Sres; // will be NULL
}

#endif
