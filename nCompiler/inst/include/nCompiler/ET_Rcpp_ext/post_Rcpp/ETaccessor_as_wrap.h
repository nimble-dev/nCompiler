#ifndef ETACCESSOR_RCPP_AS_WRAP_H_
#define ETACCESSOR_RCPP_AS_WRAP_H_

#include <nCompiler/ET_Rcpp_ext/post_Rcpp/ETaccessor_post_Rcpp.h>
#include <nCompiler/ET_Rcpp_ext/post_Rcpp/ET_Rcpp_as_wrap.h>

// Conversion from a SEXP to a type-erased ETaccessorBase.
// The concrete ETaccessor type must be chosen at run time from the SEXP:
// the scalar type from TYPEOF and nDim from the dim attribute (no dim
// attribute means nDim = 1, since R has no true scalars).
// The result owns its data (copy = true), because there is no C++ object
// for it to refer to. Hence set() on the result does not write back to R.

template<typename Scalar, int nDim>
std::unique_ptr<ETaccessorBase> SEXP_to_ETaccessor_typed(SEXP Sx) {
  using ET = Eigen::Tensor<Scalar, nDim>;
  return std::make_unique<ETaccessor<ET, true> >(Rcpp::as<ET>(Sx));
}

template<typename Scalar>
std::unique_ptr<ETaccessorBase> SEXP_to_ETaccessor_nDim(SEXP Sx, int nDim) {
  switch(nDim) {
  case 1: return SEXP_to_ETaccessor_typed<Scalar, 1>(Sx);
  case 2: return SEXP_to_ETaccessor_typed<Scalar, 2>(Sx);
  case 3: return SEXP_to_ETaccessor_typed<Scalar, 3>(Sx);
  case 4: return SEXP_to_ETaccessor_typed<Scalar, 4>(Sx);
  case 5: return SEXP_to_ETaccessor_typed<Scalar, 5>(Sx);
  case 6: return SEXP_to_ETaccessor_typed<Scalar, 6>(Sx);
  default:
    Rcpp::stop("Converting an R object to an ETaccessor supports up to 6 dimensions, but the object has %i.", nDim);
  }
  return nullptr;
}

inline std::unique_ptr<ETaccessorBase> SEXP_to_ETaccessorBase(SEXP Sx) {
  // NULL maps to an empty pointer, mirroring wrap().
  if(Sx == R_NilValue) return nullptr;
  SEXP Sdims = Rf_getAttrib(Sx, R_DimSymbol);
  int nDim = (Sdims == R_NilValue) ? 1 : Rf_length(Sdims);
  switch(TYPEOF(Sx)) {
  case REALSXP: return SEXP_to_ETaccessor_nDim<double>(Sx, nDim);
  case INTSXP:  return SEXP_to_ETaccessor_nDim<int>(Sx, nDim);
  case LGLSXP:  return SEXP_to_ETaccessor_nDim<bool>(Sx, nDim);
  case STRSXP:  return SEXP_to_ETaccessor_nDim<std::string>(Sx, nDim);
  default:
    Rcpp::stop("Converting an R object to an ETaccessor requires a numeric, integer, logical or character object.");
  }
  return nullptr;
}

namespace Rcpp {
  inline SEXP wrap(const std::unique_ptr<ETaccessorBase>& p) {
    return p ? p->get() : R_NilValue;
  }

  namespace traits {
    template <>
    class Exporter< std::unique_ptr<ETaccessorBase> > {
    public:
      Exporter(SEXP x) : x_(x) {}
      std::unique_ptr<ETaccessorBase> get() {
        return SEXP_to_ETaccessorBase(x_);
      }
    private:
      SEXP x_;
    };
  }
}

#endif // ETACCESSOR_RCPP_AS_WRAP_H_
