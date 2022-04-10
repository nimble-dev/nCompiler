#ifndef _NCOMPILER_RCPP
#define _NCOMPILER_RCPP

#define PRINTF Rprintf

// From nimble
int SEXP_2_int(SEXP Sn, int i, int offset ) {
  if(!(Rf_isNumeric(Sn) || Rf_isLogical(Sn))) PRINTF("Error: SEXP_2_int called for SEXP that is not numeric or logical\n");
  if(LENGTH(Sn) <= i) PRINTF("Error: SEXP_2_int called for element %i% >= length of %i.\n", i, LENGTH(Sn));
  if(Rf_isInteger(Sn) || Rf_isLogical(Sn)) {
    if(Rf_isInteger(Sn))
      return(INTEGER(Sn)[i] + offset);
    else
      return(LOGICAL(Sn)[i]);
  } else {
    if(Rf_isReal(Sn)) {
      double ans = REAL(Sn)[i] + offset;
      if(ans != floor(ans)) PRINTF("Warning from SEXP_2_int: input element is a real with a non-integer value\n");
      return(static_cast<int>(ans));
    } else {
      PRINTF("Error: We could not handle input type to  SEXP_2_int\n");
    }
  }
  return(0);
}


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

// this code got klugey
template<bool do_resize=true>
  struct resize_if_needed {
    template<typename T>
    static void resize(T &t, int n) {t.resize(n);}
  };
  template<>
  struct resize_if_needed<false> {
    template<typename T>
    static void resize(T &t, int n) {}
  };

// can be specialized for nDim up to 8 for array's ctors
template<class Index, typename return_type, bool do_resize=true >
return_type SEXP_indices_2_IndexArray_general(SEXP x) {
  // from RcppEigen RcppEigenWrap.h
  /* Shield<SEXP> dims( ::Rf_getAttrib( object, R_DimSymbol ) ); */
  /* if( Rf_isNull(dims) || ::Rf_length(dims) != 2 ){ */
  /*   throw ::Rcpp::not_a_matrix(); */
  /* } */
  /* int* dims_ = INTEGER(dims); */
  return_type ans;
  SEXP SDims; 
  PROTECT(SDims = ::Rf_getAttrib( x, R_DimSymbol ));
  if(SDims == R_NilValue) {
    resize_if_needed<do_resize>::template resize<return_type>(ans, 1);
    // if(do_resize) ans.resize(1);
    ans[0] = LENGTH(x);
  } else {
    int nDim = LENGTH(SDims);
    resize_if_needed<do_resize>::template resize<return_type>(ans, nDim);
    //    if(do_resize) ans.resize(nDim);
    int *xIndices = INTEGER(SDims);
    for(unsigned int i = 0; i < nDim; i++) {
      ans[i] = xIndices[i];
    }
  }
  UNPROTECT(1);
  return ans;
}

// backward compatibility with original way this was written,
// assuming fixed know nDim in any calling code.
template<class Index, int nDim>
Eigen::array<Index, nDim> SEXP_indices_2_IndexArray(SEXP x) {
  return SEXP_indices_2_IndexArray_general<Index, Eigen::array<Index, nDim>, false>(x);
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
    std::cout<<"found it"<<std::endl;
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
    std::cout<<"found it2"<<std::endl;
    return xCopy; // compiler should use copy elision
  }
  ~nCompiler_EigenRef_SEXP_converter() {
    // One idea was to update the Sinput object upon destruction,
    // but that is not how SEXP objects work.  If we assign it to
    // newly allocated data, this is not seen by the calling function.
    // std::cout<<xCopy[0]<<std::endl;
    //    Sinput = PROTECT(Rcpp::wrap(xCopy)); // Does not modify original object
    //    UNPROTECT(1);
    //    REAL(Sinput)[0] = xCopy[0]; // This does modify the calling object
  }
 private:
  SEXP Sinput;
  IndexArray indexArray;
  EigenTensorType xCopy;
};

SEXP STMinput_2_expr(SEXP Sx) {
  // add checks
  return VECTOR_ELT(Sx, 0);
}

SEXP STMinput_2_env(SEXP Sx) {
  // add checks
  return VECTOR_ELT(Sx, 1);
}

SEXP Sexpr_2_data(SEXP Sexpr, SEXP Senv) {
  // Input should be a name (`A`) or a bracket expression (`[`(A, <indices>), parsed from A[<indices>].
  // add checks and flexibility for integer vs double
  //
  // If it is bracketed expression, Sexpr is pairlist:
  // ( `[`, (`A`, ( index1, (index 2, etc. ) ) ) )
  // where CAR pulls out the first and CDR the rest at each step and
  // CADR( ) is CAR(CAR( ) ), etc.
  
  SEXP Ssym;
  if(Rf_isSymbol(Sexpr)) { // argument is a name without indexing brackets
    Ssym = PROTECT(Sexpr);
  } else {
    if( Rf_isLanguage(Sexpr) ) {
      SEXP Sop = PROTECT(CAR(Sexpr)); // should be `[` of A[ <inds> ]
      // If these get turned into error throws, check on PROTECT/UNPROTECT balance,
      // or use Rcpp::Shield.
      if(Sop != R_BracketSymbol) {
	std::cout<<"Problem: Argument to refBlock should be a name or indexing expression."<<std::endl;
      }
      UNPROTECT(1); // Sop
      Ssym = PROTECT(CADR(Sexpr));
      if(!Rf_isSymbol(Ssym)) {
	std::cout<<"Problem: Argument to refBlock has first arg that is not a symbol."<<std::endl;
      }
    } else {
      std::cout<<"Problem: Argument to refBlock should be a name or language object."<<std::endl;
      PROTECT(Ssym = R_NilValue);
    }
  }
  SEXP Robj = PROTECT(Rf_findVarInFrame(Senv, Ssym)); // This does not search up environments
  // SEXP Robj = PROTECT(Rf_findVar(Ssym, Senv)); // This does.
  // For now the reference behavior only works in the immediate calling environment.
  // Passing by reference is already dangerous, so the user should be careful,
  // and objects of the same name in higher environments should not be modified.
  if(Rf_isNull(Robj)) {
    std::cout<<"Problem: Variable in refBlock argument not found."<<std::endl;
  }
  UNPROTECT(2);
  return Robj;
}

int SEXP_eval_to_single_int(SEXP Sx, SEXP Senv) {
  int ans;
  if(Rf_isSymbol(Sx)) {
    ans = SEXP_2_int(PROTECT(Rf_eval(Sx, Senv)), 0, -1);
    UNPROTECT(1);
  }
  else
    ans = SEXP_2_int(Sx, 0, -1);
  return ans;
}

template<typename InputArray>
std::vector<b__> SEXP_2_indexBlockArray(SEXP Sexpr,
					SEXP Senv,
					InputArray &sizeArray) {
  // This will be called after Sexpr_2_data, so much of the checking
  // of Sexpr as a valid input will already be done.
  //
  // sizeArray is used if there are no indexing brackets.
  // It is also used to check that entries in indexing brackets 
  //  are within bounds.
  SEXP R_ColonSymbol = Rf_install(":"); // Why isn't this with the others in Rinternals.h

  int nDim = sizeArray.size();
  std::vector<b__> indexBlockArray(nDim);
  SEXP SnextIndex, Sind, Sop, Sargs;
  bool useSizeArray = Rf_isSymbol(Sexpr);
  if(!useSizeArray) {
    SnextIndex = PROTECT(CDDR(Sexpr)); // See explanation above of CAR, CADR, etc.
  }
  for(int i = 0; i < nDim; ++i) {
    std::cout<<"i = "<<i<<std::endl;
    if(!useSizeArray) {
      Sind = PROTECT(CAR(SnextIndex));
      if(Rf_isLanguage(Sind)) { // should be `:`(start, end)
	Sop = PROTECT(CAR(Sind)); // should be `:`
	if(Sop != R_ColonSymbol) {
	  std::cout<<"Problem: Index in a refBlock argument has an operation that is not ':'"<<std::endl;
	}
	UNPROTECT(1); // done with Sop
	Sargs = PROTECT(CDR(Sind));
	int first = SEXP_eval_to_single_int(PROTECT(CAR(Sargs)), Senv);
	int last =  SEXP_eval_to_single_int(PROTECT(CADR(Sargs)), Senv);
	if(first < 0) {
	  PRINTF("Problem: First index in a range is <= 0. Using 0.\n");
	  first = 0;
	}
	if(last > sizeArray[i] - 1) {
	  PRINTF("Problem: Last index in a range is too large.  Using size of object instead.\n");
	  last = sizeArray[i] - 1;
	}
	PRINTF("first last\n");
	indexBlockArray[i] = b__(first, last);
	UNPROTECT(3);
      } else { // index entry is a number, a variable, or a blank.
	bool isBlank(false);
	if(Rf_isSymbol(Sind)) {
	  isBlank = PRINTNAME(Sind) == R_BlankString;
	}
	if(isBlank) {
	  PRINTF("blank\n");
	  indexBlockArray[i] = b__(0, sizeArray[i]-1);
	} else {
	  indexBlockArray[i] = b__( SEXP_eval_to_single_int(Sind, Senv) );
	  std::cout<<"Got singleton "<< SEXP_eval_to_single_int(Sind, Senv)<<std::endl;
	}
      }
      if(i < nDim -1)
	SnextIndex = PROTECT(CDR(SnextIndex));
    } else {
      indexBlockArray[i] = b__(0, sizeArray[i]-1);
    }
  }
  if(!useSizeArray) {
    UNPROTECT(2*nDim); // From the SnextIndex and Sind uses.
  }
  return(indexBlockArray);
}

template< typename Scalar, int nInd >
class nCompiler_StridedTensorMap_SEXP_converter {
 public:
  typedef Eigen::Tensor<Scalar, nInd> EigenTensorType;
  typedef Eigen::StridedTensorMap<EigenTensorType> StridedTensorMapType;
  typedef Eigen::Tensor<Scalar, nInd>& EigenTensorRefType;

  typedef typename EigenTensorType::Index Index;
  typedef typename Eigen::array<Index, nInd> IndexArray;
 nCompiler_StridedTensorMap_SEXP_converter(SEXP Sx) :
  Sinput(Sx),
    Senv(STMinput_2_env(Sx)),
    Sexpr(STMinput_2_expr(Sx)),
    Sdata(Sexpr_2_data(Sexpr, Senv)),
    indexArray(SEXP_indices_2_IndexArray_general<Index, std::vector<Index> >(Sdata)),
    indexBlockArray(SEXP_2_indexBlockArray(Sexpr, Senv, indexArray)),
    xMap(REAL(Sdata), indexArray, indexBlockArray )
      {
  }
  operator StridedTensorMapType() {
    //xMap = Eigen::MakeStridedTensorMap<2>::make(ans, indexBlockArray);
    std::cout<<"found it3"<<std::endl;
    return xMap; // compiler should use copy elision
  }
  ~nCompiler_StridedTensorMap_SEXP_converter() {
  }
 private:
  SEXP Sinput;
  SEXP Senv;
  SEXP Sexpr;
  SEXP Sdata;
  std::vector<Index> indexArray;
  std::vector<b__> indexBlockArray;
  StridedTensorMapType xMap;
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
    template<typename Scalar, int nInd>
      struct input_parameter< Eigen::StridedTensorMap< Eigen::Tensor<Scalar, nInd> > > {
      typedef nCompiler_StridedTensorMap_SEXP_converter<Scalar, nInd> type;
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
