#ifndef NCOMPILER_COMPILENIMBLE_SUPPORT_H_
#define NCOMPILER_COMPILENIMBLE_SUPPORT_H_


// I think the following will be moved to a nimble 2.0 package but is drafted here for now

inline Rcpp::Environment get_NF_dataenv(SEXP NFobj) {
  SEXP S_string_xData;
  SEXP S_xData;
  PROTECT(S_string_xData = Rf_allocVector(STRSXP, 1));
  SET_STRING_ELT(S_string_xData, 0, PROTECT(Rf_mkChar(".xData")));
  PROTECT(S_xData = R_do_slot(NFobj, S_string_xData));
  UNPROTECT(3);
  return S_xData;
}

template<typename T>
struct nComp_SEXP_to_type {
    static void go(T &target, SEXP Sobj) {
        target = Rcpp::as<T>(Sobj);
    }
};

template<typename T>
void SEXP_to_type(T &target, SEXP Sobj) {
    nComp_SEXP_to_type<T>::go(target, Sobj);
}

#endif // NCOMPILER_COMPILENIMBLE_SUPPORT_H_
