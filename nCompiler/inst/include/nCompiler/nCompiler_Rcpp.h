#ifndef _NCOMPILER_RCPP
#define _NCOMPILER_RCPP


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

template<typename Scalar, int nInd> /* Scalar is the target scalar.  The input scalar is determined by TYPEOF(SINPUT). */
struct SEXP_2_EigenTensor {
  template<typename EigenTensorType, typename IndexArray>
    static EigenTensorType copy(SEXP &Sinput,
				const IndexArray &indexArray) {
    EigenTensorType xCopy;
    typedef typename std::is_same<Scalar, int>::type i_match_type;
    typedef typename std::is_same<Scalar, double>::type d_match_type;
    switch( TYPEOF(Sinput) ) {
    case REALSXP:
      //      std::cout<<"copying from REAL\n";
      xCopy =
	castedTensorCopy<double, Scalar, nInd>(REAL(Sinput),
					       indexArray,
					       d_match_type()
					       );
      break;
    case INTSXP:
      //      std::cout<<"copying from INTEGER\n";
      xCopy =
	castedTensorCopy<int, Scalar, nInd>(INTEGER(Sinput),
					    indexArray,
					    i_match_type()
					    );
      break;
    case LGLSXP:
      //      std::cout<<"copying from LOGICAL\n";
      // R represents logicals as int
      xCopy =
	castedTensorCopy<int, Scalar, nInd>(INTEGER(Sinput),
					    indexArray,
					    i_match_type()
					    );
      break;
    default:
      std::cout<<"Bad type\n"<<std::endl;
    }
    return xCopy; // compiler should use copy elision
  }
};


template< typename Scalar, int nInd >
class nCompiler_Eigen_SEXP_converter {
 public:
  typedef Eigen::Tensor<Scalar, nInd> EigenTensorType;
  typedef Eigen::Tensor<Scalar, nInd>& EigenTensorRefType;

  typedef typename EigenTensorType::Index Index;
  typedef typename Eigen::array<Index, nInd> IndexArray;
 nCompiler_Eigen_SEXP_converter(SEXP Sx) :
  Sinput(Sx),
    indexArray(SEXP_indices_2_IndexArray<Index, nInd>(Sx)) {
  }
  operator EigenTensorType() {
    EigenTensorType xCopy;
    xCopy = SEXP_2_EigenTensor<Scalar, nInd>::template copy<EigenTensorType, IndexArray>(Sinput, indexArray);
    return xCopy; // compiler should use copy elision
  }
 private:
  SEXP Sinput;
  IndexArray indexArray;
};

template< typename Scalar, int nInd >
class nCompiler_EigenRef_SEXP_converter {
 public:
  typedef Eigen::Tensor<Scalar, nInd> EigenTensorType;
  typedef Eigen::Tensor<Scalar, nInd>& EigenTensorRefType;

  typedef typename EigenTensorType::Index Index;
  typedef typename Eigen::array<Index, nInd> IndexArray;
 nCompiler_EigenRef_SEXP_converter(SEXP Sx) :
  Sinput(Sx),
    indexArray(SEXP_indices_2_IndexArray<Index, nInd>(Sx)) {
  }
  operator EigenTensorRefType() {
    xCopy = SEXP_2_EigenTensor<Scalar, nInd>::template copy<EigenTensorType, IndexArray>(Sinput, indexArray);
    return xCopy; // compiler should use copy elision
  }
  ~nCompiler_EigenRef_SEXP_converter() {
    // One idea was to update the Sinput object upon destruction,
    // but that is not how SEXP objects work.  If we assign it to
    // newly allocated data, this is not seen by the calling function.
    // std::cout<<xCopy[0]<<std::endl;
    //    Sinput = PROTECT(Rcpp::wrap(xCopy)); // Does not modify original object
    // UNPROTECT(1);
    //    REAL(Sinput)[0] = xCopy[0];
  }
 private:
  SEXP &Sinput;
  IndexArray indexArray;
  EigenTensorType xCopy;
};


namespace Rcpp {
  namespace traits {
    template<typename Scalar, int nInd>
      struct input_parameter< Eigen::Tensor<Scalar, nInd> > {
      typedef nCompiler_Eigen_SEXP_converter<Scalar, nInd> type;
    };
    template<typename Scalar, int nInd>
      struct input_parameter< Eigen::Tensor<Scalar, nInd>& > {
      typedef nCompiler_EigenRef_SEXP_converter<Scalar, nInd> type;
    };

  } // end namespace traits
  
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
  
} // end namespace Rcpp

#endif
