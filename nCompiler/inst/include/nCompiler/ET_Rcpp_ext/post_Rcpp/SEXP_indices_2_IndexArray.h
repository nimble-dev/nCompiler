#ifndef SEXP_INDICES_2_INDEXARRAY_H_
#define SEXP_INDICES_2_INDEXARRAY_H_

#include <Rinternals.h>

template<bool do_resize=true>
struct resize_if_needed {
  template<typename T>
  static void resize(T &t, size_t n) {t.resize(n);}
};
template<>
struct resize_if_needed<false> {
  template<typename T>
  static void resize(T &t, size_t n) {}
};

/*
 * Notes on cases for SEXP_indices_2_IndexArray_general
 *
 * When do_resize=false, the return_type nDims is known at compile time because it must match the declared type.
 * The copying mechanism already uses a TensorMap into the R-allocated memory
 *
 * nimble behavior: for declared 1D, flatten input. for declared >1D, error out for mismatched input.
 * nCompiler behavior (if not mimicing nimble): possibly allow declared 2D to accept 1D input
 *          possibly allow (optional?) erroring instead of flattening for wrong 1D input
 *
 */
template<class Index, typename return_type, bool do_resize=true >
return_type SEXP_indices_2_IndexArray_general(SEXP x) {
  // from RcppEigen RcppEigenWrap.h
  /* Shield<SEXP> dims( ::Rf_getAttrib( object, R_DimSymbol ) ); */
  /* if( Rf_isNull(dims) || ::Rf_length(dims) != 2 ){ */
  /*   throw ::Rcpp::not_a_matrix(); */
  /* } */
  /* int* dims_ = INTEGER(dims); */
  return_type ans;
  SEXP Sinput_dims;
  PROTECT(Sinput_dims = ::Rf_getAttrib( x, R_DimSymbol ));
  size_t input_nDim;
  if(Sinput_dims == R_NilValue) input_nDim = 1; else input_nDim = LENGTH(Sinput_dims);
  resize_if_needed<do_resize>::template resize<return_type>(ans, input_nDim);
  size_t output_nDim = ans.size(); // will match input_nDim if do_resize==true
  bool mismatch(false);
  if(output_nDim == 1) {
    ans[0] = LENGTH(x);
  } else {
    if(output_nDim != input_nDim) {
      mismatch=true; // track error for later to avoid multiple UNPROTECTs
    } else {
      int *xIndices = INTEGER(Sinput_dims);
      for(size_t i = 0; i < input_nDim; i++) {
        ans[i] = xIndices[i];
      }
    }
  }
  UNPROTECT(1);
  if(mismatch) {
    Rcpp::stop("Dimension mismatch on input. Expected %i, but got %i.\n", output_nDim, input_nDim);
  }
  return ans;
}


// backward compatibility with original way this was written,
// assuming fixed know nDim in any calling code.
template<class Index, int nDim>
Eigen::array<Index, nDim> SEXP_indices_2_IndexArray(SEXP x) {
  return SEXP_indices_2_IndexArray_general<Index, Eigen::array<Index, nDim>, false>(x);
}


#endif // SEXP_INDICES_2_INDEXARRAY_H_
