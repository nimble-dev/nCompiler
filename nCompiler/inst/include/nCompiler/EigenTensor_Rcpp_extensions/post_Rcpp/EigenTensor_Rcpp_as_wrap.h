#ifndef TENSOR_RCPP_AS_WRAP_H_
#define TENSOR_RCPP_AS_WRAP_H_

#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS

// should be in RcppEigen installation
#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>
#include <memory>
//#include "tensorFlex.h"
//#include "tensorUtils.h"
//#include "StridedTensorMap.h"
// Exporters go from SEXP to internal type
//#include "nCompiler_Rcpp.h"

#include "SEXP_2_EigenTensor.h"
#include "SEXP_indices_2_IndexArray.h"

// The following Exporter implements Rcpp::as< Eigen::Tensor<T, nDim> >
namespace Rcpp {
  namespace traits {
    // Casting here will be important to support.
    template <typename T, int nDim>
    class Exporter< Eigen::Tensor<T, nDim> > {
    public:
      typedef typename Eigen::Tensor<T, nDim>::Index Index;
      typedef typename Eigen::array<Index, nDim> IndexArray;
      Exporter(SEXP Sx) : indexArray(SEXP_indices_2_IndexArray<Index, nDim>(Sx)) {
        std::cout<<"In Exporter for Eigen::Tensor.  Check where this happens because we thought it might be cruft."<<std::endl;
        // This whole system seems to impose an extra copy.
        // That is because we're being dispatched from ::Rcpp::traits::r_type_generic_tag
        // We may need to work around that but will try not to need to.
        // Meanwhile a copy here could be spared if it is only done
        // by get().
        Sinput = Sx;
      }
      inline Eigen::Tensor<T, nDim> get(){
        typedef Eigen::Tensor<T, nDim> EigenTensorType;
    //    std::cout<<"get1"<<std::endl;
        EigenTensorType t = SEXP_2_EigenTensor<T, nDim>::template copy<EigenTensorType, IndexArray>(Sinput, indexArray);
        return t;
      }
    private:
      IndexArray indexArray;
      SEXP Sinput;
    };

    template <typename T, int nDim>
    class Exporter< Eigen::Tensor<T, nDim>& > {
    public:
      typedef typename Eigen::Tensor<T, nDim>::Index Index;
      typedef typename Eigen::array<Index, nDim> IndexArray;
      Exporter(SEXP Sx) : indexArray(SEXP_indices_2_IndexArray<Index, nDim>(Sx)) {
        //std::cout<<"In Exporter"<<std::endl;
        // This whole system seems to impose an extra copy.
        // That is because we're being dispatched from ::Rcpp::traits::r_type_generic_tag
        // We may need to work around that but will try not to need to.
        // Meanwhile a copy here could be spared if it is only done
        // by get().
        Sinput = Sx;
      }
      inline Eigen::Tensor<T, nDim> get(){
        typedef Eigen::Tensor<T, nDim> EigenTensorType;
    //    std::cout<<"get2"<<std::endl;
        EigenTensorType t = SEXP_2_EigenTensor<T, nDim>::template copy<EigenTensorType, IndexArray>(Sinput, indexArray);
        return t;
      }
      ~Exporter() {
        std::cout<<"in destructor"<<std::endl;
      }
    private:
      IndexArray indexArray;
      SEXP Sinput;
    };

  }
}

namespace Rcpp {
  // Casting should not be necessary here but might be
  // safe to provide.
  // But note that the as<> system could invoke an unnecessary
  // eigen evaluation, prior to a copy.
  template <int nDim>
  SEXP wrap( const Eigen::Tensor<double, nDim> &x ) {
    const typename Eigen::Tensor<double, nDim>::Dimensions &xDims = x.dimensions();
    SEXP Sans = PROTECT(::Rf_allocVector(REALSXP, x.size()));
    Eigen::TensorMap< Eigen::Tensor<double, nDim> > ansMap(REAL(Sans), xDims);
    ansMap = x; // copy the data
    if(nDim > 1) {
      SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim));
      int *dims = INTEGER(Sdims);
      for(unsigned int i = 0; i < nDim; i++) {
        dims[i] = xDims[i];
      }
      ::Rf_setAttrib(Sans, R_DimSymbol, Sdims);
      UNPROTECT(1);
    }
    UNPROTECT(1);
    return(Sans);
  };

  template <int nDim>
  SEXP wrap( const Eigen::Tensor<int, nDim> &x ) {
    const typename Eigen::Tensor<int, nDim>::Dimensions &xDims = x.dimensions();
    SEXP Sans = PROTECT(::Rf_allocVector(INTSXP, x.size()));
    Eigen::TensorMap< Eigen::Tensor<int, nDim> > ansMap(INTEGER(Sans), xDims);
    ansMap = x; // copy the data
    if(nDim > 1) {
      SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim));
      int *dims = INTEGER(Sdims);
      for(unsigned int i = 0; i < nDim; i++) {
        dims[i] = xDims[i];
      }
      ::Rf_setAttrib(Sans, R_DimSymbol, Sdims);
      UNPROTECT(1);
    }
    UNPROTECT(1);
    return(Sans);
  };

  template <int nDim>
  SEXP wrap( const Eigen::Tensor<bool, nDim> &x ) {
    const typename Eigen::Tensor<bool, nDim>::Dimensions &xDims = x.dimensions();
    SEXP Sans = PROTECT(::Rf_allocVector(LGLSXP, x.size()));
    // R represents logicals as integers
    Eigen::TensorMap< Eigen::Tensor<int, nDim> > ansMap(INTEGER(Sans), xDims);
    ansMap = x.template cast<int>(); // copy the data
    if(nDim > 1) {
      SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim));
      int *dims = INTEGER(Sdims);
      for(unsigned int i = 0; i < nDim; i++) {
        dims[i] = xDims[i];
      }
      ::Rf_setAttrib(Sans, R_DimSymbol, Sdims);
      UNPROTECT(1);
    }
    UNPROTECT(1);
    return(Sans);
  };
} // end namespace Rcpp

#endif // TENSOR_RCPP_AS_WRAP_H_
