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

// Copy from SEXP to Eigen Tensor when scalar types are the same
template<class fromT, class toT, int nDim>
Eigen::Tensor<toT, nDim>
castedTensorCopy(const fromT * const from,
		 const Eigen::array<typename Eigen::Tensor<fromT, nDim>::Index, nDim> &indexArray,
		 std::true_type same_types) {
  Eigen::TensorMap< Eigen::Tensor<const fromT, nDim> > xMap(from, indexArray);
  Eigen::Tensor<toT, nDim> to = xMap;
  return to;
}

template<class fromT, class toT, int nDim>
Eigen::Tensor<toT, nDim>
castedTensorCopy(const fromT * const from,
		 const Eigen::array<typename Eigen::Tensor<fromT, nDim>::Index, nDim> &indexArray,
		 std::false_type different_types) {
  Eigen::TensorMap< Eigen::Tensor<const fromT, nDim> > xMap(from, indexArray);
  Eigen::Tensor<toT, nDim> to = xMap.template cast<toT>();
  return to;
}

// can be specialized for nDim up to 8 for array's ctors
template<class Index, int nDim>
Eigen::array<Index, nDim> SEXP_indices_2_IndexArray(SEXP x) {
  // from RcppEigen RcppEigenWrap.h
  /* Shield<SEXP> dims( ::Rf_getAttrib( object, R_DimSymbol ) ); */
  /* if( Rf_isNull(dims) || ::Rf_length(dims) != 2 ){ */
  /*   throw ::Rcpp::not_a_matrix(); */
  /* } */
  /* int* dims_ = INTEGER(dims); */
    Eigen::array<Index, nDim> ans;
  SEXP SDims; 
  PROTECT(SDims = ::Rf_getAttrib( x, R_DimSymbol ));
  if(SDims == R_NilValue) {
    ans[0] = LENGTH(x);
  } else {
    int *xIndices = INTEGER(::Rf_getAttrib( x, R_DimSymbol ));
    for(unsigned int i = 0; i < nDim; i++) {
      ans[i] = xIndices[i];
    }
  }
  UNPROTECT(1);
  return ans;
}

namespace Rcpp {
  namespace traits {
    // Casting here will be important to support.
    template <typename T, int nDim>
      class Exporter< Eigen::Tensor<T, nDim> > {
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
	Eigen::Tensor<T, nDim> t;
	typedef typename std::is_same<T, int>::type i_match_type;
	typedef typename std::is_same<T, double>::type d_match_type;
	switch( TYPEOF(Sinput) ) {
	case REALSXP:
	  //	  std::cout<<"copying from REAL\n";

	  t =
	    castedTensorCopy<double, T, nDim>(REAL(Sinput),
					      indexArray,
					      d_match_type()
					      );
	return t;
	  break;
	case INTSXP:
	  // std::cout<<"copying from INTEGER\n";
	  t =
	    castedTensorCopy<int, T, nDim>(INTEGER(Sinput),
					   indexArray,
					   i_match_type()
					   );
	  return t;
	  break;
	case LGLSXP:
	  // std::cout<<"copying from LOGICAL\n";
	  // R represents logicals as int
	  t =
	    castedTensorCopy<int, T, nDim>(INTEGER(Sinput),
					   indexArray,
					   i_match_type()
					   );
	  return t;
	  break;
	default:
	  std::cout<<"Bad type\n"<<std::endl;
	  return t;
	}
	
      }
    private:
      IndexArray indexArray;
      SEXP Sinput;
      //      Eigen::Tensor<T, nDim> t;
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
    //    std::cout<<"In wrap with nDim = "<<nDim<<std::endl;
    // if(nDim == 1) std::cout<<"Warning: vector will be returned as 1D array.  Fix the wrap method for Eigen::Tensor to fix this."<<std::endl;
    SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim));
    int *dims = INTEGER(Sdims);
    const typename Eigen::Tensor<double, nDim>::Dimensions &xDims = x.dimensions();
    for(unsigned int i = 0; i < nDim; i++) {
      dims[i] = xDims[i];
    }
    SEXP Sans = PROTECT(::Rf_allocVector(REALSXP, x.size()));
    Eigen::TensorMap< Eigen::Tensor<double, nDim> > ansMap(REAL(Sans), xDims);
    ansMap = x; // copy the data
    ::Rf_setAttrib(Sans, R_DimSymbol, Sdims);
    UNPROTECT(2);
    return(Sans);
  };

  template <int nDim>
    SEXP wrap( const Eigen::Tensor<int, nDim> &x ) {
    // std::cout<<"In wrap with nDim = "<<nDim<<std::endl;
    // if(nDim == 1) std::cout<<"Warning: vector will be returned as 1D array.  Fix the wrap method for Eigen::Tensor to fix this."<<std::endl;
    SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim));
    int *dims = INTEGER(Sdims);
    const typename Eigen::Tensor<int, nDim>::Dimensions &xDims = x.dimensions();
    for(unsigned int i = 0; i < nDim; i++) {
      dims[i] = xDims[i];
    }
    SEXP Sans = PROTECT(::Rf_allocVector(INTSXP, x.size()));
    Eigen::TensorMap< Eigen::Tensor<int, nDim> > ansMap(INTEGER(Sans), xDims);
    ansMap = x; // copy the data
    ::Rf_setAttrib(Sans, R_DimSymbol, Sdims);
    UNPROTECT(2);
    return(Sans);
  };

  template <int nDim>
    SEXP wrap( const Eigen::Tensor<bool, nDim> &x ) {
    // std::cout<<"In wrap with nDim = "<<nDim<<std::endl;
    // if(nDim == 1) std::cout<<"Warning: vector will be returned as 1D array.  Fix the wrap method for Eigen::Tensor to fix this."<<std::endl;
    SEXP Sdims = PROTECT(::Rf_allocVector(INTSXP, nDim));
    int *dims = INTEGER(Sdims);
    const typename Eigen::Tensor<bool, nDim>::Dimensions &xDims = x.dimensions();
    for(unsigned int i = 0; i < nDim; i++) {
      dims[i] = xDims[i];
    }
    SEXP Sans = PROTECT(::Rf_allocVector(LGLSXP, x.size()));
    // R represents logicals as integers
    Eigen::TensorMap< Eigen::Tensor<int, nDim> > ansMap(INTEGER(Sans), xDims);
    ansMap = x.template cast<int>(); // copy the data
    ::Rf_setAttrib(Sans, R_DimSymbol, Sdims);
    UNPROTECT(2);
    return(Sans);
  };
  
}

// recycling rule
// see typedefs.h for the types used below
template<typename ScalarType, distn3_1scalar_op distn>
class distn_nullaryClass {
  const Eigen1d &Arg1, &Arg2, &Arg3;
  const ScalarType &Arg4;
  int size;
public:
  typedef ScalarType ScalarArgType;
  typedef typename Eigen1d::Index Index;
  // the following doesn't work:
  // static const distn3_1scalar_op distn = distn_;

 distn_nullaryClass(const Eigen1d &A1, const Eigen1d &A2, const Eigen1d &A3, const ScalarType &A4) :
   Arg1(A1), Arg2(A2), Arg3(A3), Arg4(A4), size(R::imax2(A1.size(), R::imax2(A2.size(), A3.size())))
  {}

  // Whatever size determinations are needed can happen here.
  // And we can return a fixed-size array to allow more than one dimension
  int getSize() const {
    // when we move to multiple dimensions, this method should return an IndexArray
    return size;
  }

  double operator()(Index i) const {
    // use the Rf naming convention in generated code
    return distn(Arg1.coeff(i % Arg1.size()), Arg2.coeff(i % Arg2.size()), Arg3.coeff(i % Arg3.size()), Arg4);
  }
  double operator()(Index i, Index j) const {
    // not implemented
    return Arg1.coeff(i);
  }
};

// The reason for using a class here for a second layer is to
// ensure that everything stays in scope while it's being used.
template<class nullaryClass>
class customNullary {
  nullaryClass nullaryObj;
  typedef EigenMap1d sizeProxyType;
  sizeProxyType sizeProxy;
  typedef typename nullaryClass::ScalarArgType ScalarType;
  typedef typename Eigen::TensorCwiseNullaryOp< nullaryClass, sizeProxyType > nullaryOpType;
public:
  customNullary(const Eigen1d &Arg1, const Eigen1d &Arg2, const Eigen1d &Arg3, const ScalarType &Arg4) :
    nullaryObj(Arg1, Arg2, Arg3, Arg4),
    // we'll eventually want to use the more general constructor
    // TensorMap(PointerArgType dataPtr, const array<Index, NumIndices>& dimensions)
    sizeProxy(static_cast<double*>(0), nullaryObj.getSize()) {}

  nullaryOpType op() { return nullaryOpType(sizeProxy, nullaryObj); }
};

// 3 arguments and 1 scalar
// since args constant references, need to have a function that calls RR3_1scalar
template<typename ScalarType, distn3_1scalar_op distn>
Eigen1d RR3_1scalar(const Eigen1d &arg1, const Eigen1d &arg2, const Eigen1d &arg3, const ScalarType &arg4) {
  return customNullary< distn_nullaryClass< ScalarType, distn > >(arg1, arg2, arg3, arg4).op();
}

// define a function-like macro that returns a distribution function
#define RR31fun(NAME, DISTN) \
template<typename ScalarType> \
Eigen1d RR_ ## NAME(const Eigen1d &x1, const Eigen1d &x2, const Eigen1d &x3, const ScalarType &x4) { \
  return RR3_1scalar<ScalarType, DISTN>(x1, x2, x3, x4); \
};

RR31fun(dbeta, Rf_dbeta); // RR_dbeta
RR31fun(dbinom, Rf_dbinom); // RR_dbinom
// RR31fun(ddexp, ???); // RR_ddexp
RR31fun(dgamma, Rf_dgamma); // RR_dgamma
// RR31fun(dinvgamma, ???); // RR_dinvgamma
RR31fun(dlnorm, Rf_dlnorm); // RR_dlnorm
RR31fun(dnbinom, Rf_dnbinom); // RR_dnbinom
RR31fun(dnorm, Rf_dnorm4); // RR_dnorm
RR31fun(dt, Rf_dt); // RR_dt
// RR31fun(dt_nonstandard, ???); // RR_dt_nonstandard
RR31fun(dunif, Rf_dunif); // RR_dunif
RR31fun(dweibull, Rf_dweibull); // RR_dweibull

#endif
