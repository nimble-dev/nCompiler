#ifndef __NCOMPILER_EIGEN
#define __NCOMPILER_EIGEN

#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS

// should be in RcppEigen installation
#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>
#include <memory>
#include "tensorFlex.h"
#include "StridedTensorMap.h"
// Exporters go from SEXP to internal type

// This could be organized in a better place, but it is here for now:
#include "shared_ptr_as_wrap.h"

#include "typedefs.h"
#include "recyclingRule.h"
#include "nCompiler_Rcpp.h"
#include "cWiseUnary_external.h"

/* // Copy from SEXP to Eigen Tensor when scalar types are the same */
/* template<class fromT, class toT, int nDim> */
/* Eigen::Tensor<toT, nDim> */
/* castedTensorCopy(const fromT * const from, */
/* 		 const Eigen::array<typename Eigen::Tensor<fromT, nDim>::Index, nDim> &indexArray, */
/* 		 std::true_type same_types) { */
/*   Eigen::TensorMap< Eigen::Tensor<const fromT, nDim> > xMap(from, indexArray); */
/*   Eigen::Tensor<toT, nDim> to = xMap; */
/*   return to; */
/* } */

/* template<class fromT, class toT, int nDim> */
/* Eigen::Tensor<toT, nDim> */
/* castedTensorCopy(const fromT * const from, */
/* 		 const Eigen::array<typename Eigen::Tensor<fromT, nDim>::Index, nDim> &indexArray, */
/* 		 std::false_type different_types) { */
/*   Eigen::TensorMap< Eigen::Tensor<const fromT, nDim> > xMap(from, indexArray); */
/*   Eigen::Tensor<toT, nDim> to = xMap.template cast<toT>(); */
/*   return to; */
/* } */

/* // can be specialized for nDim up to 8 for array's ctors */
/* template<class Index, int nDim> */
/* Eigen::array<Index, nDim> SEXP_indices_2_IndexArray(SEXP x) { */
/*   // from RcppEigen RcppEigenWrap.h */
/*   /\* Shield<SEXP> dims( ::Rf_getAttrib( object, R_DimSymbol ) ); *\/ */
/*   /\* if( Rf_isNull(dims) || ::Rf_length(dims) != 2 ){ *\/ */
/*   /\*   throw ::Rcpp::not_a_matrix(); *\/ */
/*   /\* } *\/ */
/*   /\* int* dims_ = INTEGER(dims); *\/ */
/*     Eigen::array<Index, nDim> ans; */
/*   SEXP SDims;  */
/*   PROTECT(SDims = ::Rf_getAttrib( x, R_DimSymbol )); */
/*   if(SDims == R_NilValue) { */
/*     ans[0] = LENGTH(x); */
/*   } else { */
/*     int *xIndices = INTEGER(::Rf_getAttrib( x, R_DimSymbol )); */
/*     for(unsigned int i = 0; i < nDim; i++) { */
/*       ans[i] = xIndices[i]; */
/*     } */
/*   } */
/*   UNPROTECT(1); */
/*   return ans; */
/* } */

/* namespace Rcpp { */
/*   namespace traits { */
/*     // Casting here will be important to support. */
/*     template <typename T, int nDim> */
/*       class Exporter< Eigen::Tensor<T, nDim> > { */
/*     public: */
/*       typedef typename Eigen::Tensor<T, nDim>::Index Index; */
/*       typedef typename Eigen::array<Index, nDim> IndexArray; */
/*     Exporter(SEXP Sx) : indexArray(SEXP_indices_2_IndexArray<Index, nDim>(Sx)) { */
/* 	//std::cout<<"In Exporter"<<std::endl; */
/* 	// This whole system seems to impose an extra copy. */
/* 	// That is because we're being dispatched from ::Rcpp::traits::r_type_generic_tag */
/* 	// We may need to work around that but will try not to need to. */
/* 	// Meanwhile a copy here could be spared if it is only done */
/* 	// by get(). */
/* 	Sinput = Sx; */
/*       } */
/*       inline Eigen::Tensor<T, nDim> get(){ */
/* 	Eigen::Tensor<T, nDim> t; */
/* 	typedef typename std::is_same<T, int>::type i_match_type; */
/* 	typedef typename std::is_same<T, double>::type d_match_type; */
/* 	switch( TYPEOF(Sinput) ) { */
/* 	case REALSXP: */
/* 	  //	  std::cout<<"copying from REAL\n"; */

/* 	  t = */
/* 	    castedTensorCopy<double, T, nDim>(REAL(Sinput), */
/* 					      indexArray, */
/* 					      d_match_type() */
/* 					      ); */
/* 	return t; */
/* 	  break; */
/* 	case INTSXP: */
/* 	  // std::cout<<"copying from INTEGER\n"; */
/* 	  t = */
/* 	    castedTensorCopy<int, T, nDim>(INTEGER(Sinput), */
/* 					   indexArray, */
/* 					   i_match_type() */
/* 					   ); */
/* 	  return t; */
/* 	  break; */
/* 	case LGLSXP: */
/* 	  // std::cout<<"copying from LOGICAL\n"; */
/* 	  // R represents logicals as int */
/* 	  t = */
/* 	    castedTensorCopy<int, T, nDim>(INTEGER(Sinput), */
/* 					   indexArray, */
/* 					   i_match_type() */
/* 					   ); */
/* 	  return t; */
/* 	  break; */
/* 	default: */
/* 	  std::cout<<"Bad type\n"<<std::endl; */
/* 	  return t; */
/* 	} */
	
/*       } */
/*     private: */
/*       IndexArray indexArray; */
/*       SEXP Sinput; */
/*       //      Eigen::Tensor<T, nDim> t; */
/*     }; */
/*   } */
/* } */

/* namespace Rcpp { */
/*   // Casting should not be necessary here but might be */
/*   // safe to provide. */
/*   // But note that the as<> system could invoke an unnecessary */
/*   // eigen evaluation, prior to a copy. */
/*   template <int nDim> */
/*     SEXP wrap( const Eigen::Tensor<double, nDim> &x ) { */
/*     //    std::cout<<"In wrap with nDim = "<<nDim<<std::endl; */
/*     // if(nDim == 1) std::cout<<"Warning: vector will be returned as 1D array.  Fix the wrap method for Eigen::Tensor to fix this."<<std::endl; */
/*     SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim)); */
/*     int *dims = INTEGER(Sdims); */
/*     const typename Eigen::Tensor<double, nDim>::Dimensions &xDims = x.dimensions(); */
/*     for(unsigned int i = 0; i < nDim; i++) { */
/*       dims[i] = xDims[i]; */
/*     } */
/*     SEXP Sans = PROTECT(::Rf_allocVector(REALSXP, x.size())); */
/*     Eigen::TensorMap< Eigen::Tensor<double, nDim> > ansMap(REAL(Sans), xDims); */
/*     ansMap = x; // copy the data */
/*     ::Rf_setAttrib(Sans, R_DimSymbol, Sdims); */
/*     UNPROTECT(2); */
/*     return(Sans); */
/*   }; */

/*   template <int nDim> */
/*     SEXP wrap( const Eigen::Tensor<int, nDim> &x ) { */
/*     // std::cout<<"In wrap with nDim = "<<nDim<<std::endl; */
/*     // if(nDim == 1) std::cout<<"Warning: vector will be returned as 1D array.  Fix the wrap method for Eigen::Tensor to fix this."<<std::endl; */
/*     SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim)); */
/*     int *dims = INTEGER(Sdims); */
/*     const typename Eigen::Tensor<int, nDim>::Dimensions &xDims = x.dimensions(); */
/*     for(unsigned int i = 0; i < nDim; i++) { */
/*       dims[i] = xDims[i]; */
/*     } */
/*     SEXP Sans = PROTECT(::Rf_allocVector(INTSXP, x.size())); */
/*     Eigen::TensorMap< Eigen::Tensor<int, nDim> > ansMap(INTEGER(Sans), xDims); */
/*     ansMap = x; // copy the data */
/*     ::Rf_setAttrib(Sans, R_DimSymbol, Sdims); */
/*     UNPROTECT(2); */
/*     return(Sans); */
/*   }; */

/*   template <int nDim> */
/*     SEXP wrap( const Eigen::Tensor<bool, nDim> &x ) { */
/*     // std::cout<<"In wrap with nDim = "<<nDim<<std::endl; */
/*     // if(nDim == 1) std::cout<<"Warning: vector will be returned as 1D array.  Fix the wrap method for Eigen::Tensor to fix this."<<std::endl; */
/*     SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim)); */
/*     int *dims = INTEGER(Sdims); */
/*     const typename Eigen::Tensor<bool, nDim>::Dimensions &xDims = x.dimensions(); */
/*     for(unsigned int i = 0; i < nDim; i++) { */
/*       dims[i] = xDims[i]; */
/*     } */
/*     SEXP Sans = PROTECT(::Rf_allocVector(LGLSXP, x.size())); */
/*     // R represents logicals as integers */
/*     Eigen::TensorMap< Eigen::Tensor<int, nDim> > ansMap(INTEGER(Sans), xDims); */
/*     ansMap = x.template cast<int>(); // copy the data */
/*     ::Rf_setAttrib(Sans, R_DimSymbol, Sdims); */
/*     UNPROTECT(2); */
/*     return(Sans); */
/*   }; */
  
/* } */

#endif
