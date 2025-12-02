#ifndef TENSOR_SEXP_CONVERTER_H_
#define TENSOR_SEXP_CONVERTER_H_

/* Non-template functions here are static to avoid multiple symbols error from C++ during nCompile via packaging */

#include <unsupported/Eigen/CXX11/Tensor>
#include "SEXP_2_EigenTensor.h"
#include "SEXP_indices_2_IndexArray.h"
#include <nCompiler/ET_ext/index_block.h>

#define PRINTF Rprintf

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
    RxList_(Sx) {
    // Rprintf("hello to a tensor ref\n");
    if(!(RxList_.isUsable()))
      Rcpp::stop("Problem: List was not provided for a ref arg.\n");
    Rcpp::List RxList(RxList_);
    // I could not get the following uses of Nullable to work.
    // Rcpp::Nullable<Rcpp::CharacterVector> SobjName_(SxList[0]); // compiler error: ambiguous
    //
    // Sx should contain a list with first element a symbol and second element an environment
    SEXP SobjName = Rcpp::as<SEXP>(RxList[0]);
    if(TYPEOF(SobjName) != SYMSXP) {
      if(TYPEOF(SobjName) == LANGSXP)
        Rcpp::stop("A reference argument should be a variable name, not an expression.");
      else
        Rcpp::stop("A reference argument should be a variable name.");
    }
    objStr = Rcpp::String(PRINTNAME(SobjName)).get_cstring();
    // Rcpp::Nullable<Rcpp::Environment> Senv_(SxList[1]);  // ditto
    // if(Senv_.isNull())
    //   Rcpp::stop("Problem: Environment as second list element is missing for a ref arg.\n");
    // Senv = Senv_;
    Renv = RxList[1];
    // Since Nullable did not work in this context, error-trapping might need
    // simpler use of SEXPs until everything is checked.
  }
  operator EigenTensorRefType() {
    //    Rcpp::CharacterVector SobjName = SxList[0];
    //    Rcpp::Environment Renv(SxList[1]);
//    Rprintf("Doing the implicit type conversion operator\n");
    SEXP Sobj = PROTECT(Renv.get(objStr)); // equiv to Renv[ <1st arg> ]
    if(Sobj == R_NilValue) {
      Rcpp::stop("Problem: Could not obtain object for a ref arg.\n");
    }
    IndexArray indexArray( SEXP_indices_2_IndexArray<Index, nInd>(Sobj) );
    xCopy = SEXP_2_EigenTensor<Scalar, nInd>::template copy<EigenTensorType, IndexArray>(Sobj, indexArray);
    UNPROTECT(1);
    return xCopy; // compiler should use copy elision
  }
  ~nCompiler_EigenRef_SEXP_converter() {
//    Rprintf("goodbye to a tensor ref\n");
    SEXP Sputback = PROTECT(Rcpp::wrap(xCopy));
    Renv.assign(objStr,  Sputback);
    UNPROTECT(1);
    // One idea was to update the Sinput object upon destruction,
    // but that is not how SEXP objects work.  If we assign it to
    // newly allocated data, this is not seen by the calling function.
    // std::cout<<xCopy[0]<<std::endl;
    //    Sinput = PROTECT(Rcpp::wrap(xCopy)); // Does not modify original object
    // UNPROTECT(1);
    //    REAL(Sinput)[0] = xCopy[0];
  }
private:
  Rcpp::Nullable<Rcpp::List> RxList_;
  std::string objStr;
  Rcpp::Environment Renv;
  IndexArray indexArray;
  EigenTensorType xCopy;
};

static SEXP Sexpr_2_data(SEXP Sexpr, SEXP Senv) {
  // Input should be a name (`A`) or a bracket expression (`[`(A, <indices>), parsed from A[<indices>].
  // add checks and flexibility for integer vs double
  //
  // If it is bracketed expression, Sexpr is pairlist:
  // ( `[`, (`A`, ( index1, (index 2, etc. ) ) ) )
  // where CAR pulls out the first and CDR the rest at each step and
  // CADR( ) is CAR(CAR( ) ), etc.

  SEXP Ssym;
  if(Rf_isSymbol(Sexpr)) { // argument is a name without indexing brackets
    Ssym = Sexpr;
  } else {
    if( Rf_isLanguage(Sexpr) ) {
      // It is unclear if PROTECT needs to be used for results or CAR, CADR, etc.  I don't think so.
      SEXP Sop = CAR(Sexpr); // should be `[` of A[ <inds> ]
      // If these get turned into error throws, check on PROTECT/UNPROTECT balance,
      // or use Rcpp::Shield.
      if(Sop != R_BracketSymbol) {
        Rcpp::stop("Problem: Argument to refBlock should be a name or indexing expression.");
      }
      Ssym = CADR(Sexpr);
      if(!Rf_isSymbol(Ssym)) {
        Rcpp::stop("Problem: Argument to refBlock has first arg that is not a symbol.");
      }
    } else {
      Rcpp::stop("Problem: Argument to refBlock should be a name or language object.");
    }
  }
  SEXP Robj = PROTECT(Rf_findVarInFrame(Senv, Ssym)); // This does not search up environments
  // SEXP Robj = PROTECT(Rf_findVar(Ssym, Senv)); // This does.
  // For now the reference behavior only works in the immediate calling environment.
  // Passing by reference is already dangerous, so the user should be careful,
  // and objects of the same name in higher environments should not be modified.
  if(Rf_isNull(Robj)) {
    Rcpp::stop("Problem: Variable in refBlock argument not found.");
  }
  UNPROTECT(1);
  return Robj;
}

static SEXP STMinput_2_expr(SEXP Sx) {
  // add checks
  return VECTOR_ELT(Sx, 0);
}

static SEXP STMinput_2_env(SEXP Sx) {
  // add checks
  return VECTOR_ELT(Sx, 1);
}

// From nimble
static int SEXP_2_int(SEXP Sn, int i, int offset ) {
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

static int SEXP_eval_to_single_int(SEXP Sx, SEXP Senv) {
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
   // std::cout<<"i = "<<i<<std::endl;
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
        // PRINTF("first last\n");
        indexBlockArray[i] = b__(first, last);
        UNPROTECT(3);
      } else { // index entry is a number, a variable, or a blank.
        bool isBlank(false);
        if(Rf_isSymbol(Sind)) {
          isBlank = PRINTNAME(Sind) == R_BlankString;
        }
        if(isBlank) {
        //  PRINTF("blank\n");
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


template<typename Scalar>
struct Rdataptr;

template<>
struct Rdataptr<double> {
  static double *PTR(SEXP Sin) {
    if(!Rf_isReal(Sin)) {
      std::string passed_type = "unknown";
      if(Rf_isInteger(Sin)) passed_type = "integer";
      if(Rf_isLogical(Sin)) passed_type = "logical";
      Rcpp::stop("Block reference argument expected type numeric but received type " + passed_type);
    }
    return REAL(Sin);
  }
};

template<>
struct Rdataptr<int> {
  static int *PTR(SEXP Sin) {
    if(!Rf_isInteger(Sin)) {
      std::string passed_type = "unknown";
      if(Rf_isReal(Sin)) passed_type = "numeric";
      if(Rf_isLogical(Sin)) passed_type = "logical";
      Rcpp::stop("Block reference argument expected type integer but received type " + passed_type);
    }
    return INTEGER(Sin);
  }
};

template<>
struct Rdataptr<bool> {
  static int *PTR(SEXP Sin) {
    if(!Rf_isLogical(Sin)) {
      std::string passed_type = "unknown";
      if(Rf_isReal(Sin)) passed_type = "numeric";
      if(Rf_isInteger(Sin)) passed_type = "integer";
      Rcpp::stop("Block reference argument expected type logical but received type " + passed_type);
    }
    return INTEGER(Sin);// R bools are integers
  }
};

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
    xMap(Rdataptr<Scalar>::PTR(Sdata), indexArray, indexBlockArray )
      {
  //      std::cout<<"hello to a StridedTensorMap"<<std::endl;
  }
  operator StridedTensorMapType() {
    //xMap = Eigen::MakeStridedTensorMap<2>::make(ans, indexBlockArray);
  //  std::cout<<"handling my StridedTensorMap for function input"<<std::endl;
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

#endif // TENSOR_SEXP_CONVERTER_H_
