#ifndef TENSOR_SEXP_CONVERTER_H_
#define TENSOR_SEXP_CONVERTER_H_

/* Non-template functions here are static to avoid multiple symbols error from C++ during nCompile via packaging */

#include <unsupported/Eigen/CXX11/Tensor>
#include "SEXP_2_EigenTensor.h"
#include "SEXP_indices_2_IndexArray.h"
#include <nCompiler/ET_ext/index_block.h>
#include <typeindex>

#define PRINTF Rprintf

template< typename Scalar, int nInd >
class nCompiler_Eigen_SEXP_converter {
 public:
  typedef Eigen::Tensor<Scalar, nInd> EigenTensorType;
  typedef Eigen::Tensor<Scalar, nInd>& EigenTensorRefType;
  typedef typename EigenTensorType::Index Index;
  typedef typename Eigen::array<Index, nInd> IndexArray;
 nCompiler_Eigen_SEXP_converter(SEXP Sx) :
  Rinput(Sx),
    indexArray(SEXP_indices_2_IndexArray<Index, nInd>(Sx)) {
  }
  operator EigenTensorType() {
    EigenTensorType xCopy;
    SEXP Sinput = static_cast<SEXP>(Rinput);
    xCopy = SEXP_2_EigenTensor<Scalar, nInd>
        ::template copy<EigenTensorType, IndexArray>(
                    Sinput, indexArray);
    return xCopy; // compiler should use copy elision
  }
 private:
  Rcpp::RObject Rinput;
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
    // Sx should contain a list with first element a symbol and second element an environment
    RobjExpr = static_cast<SEXP>(RxList[0]);
    SEXP SobjExpr = static_cast<SEXP>(RobjExpr);
    if(TYPEOF(SobjExpr) != SYMSXP && TYPEOF(SobjExpr) != LANGSXP) {
      Rcpp::stop("A reference argument should be a name or expression.");
    }
    Renv = static_cast<SEXP>(RxList[1]);
  }
  operator EigenTensorRefType() {
    Rcpp::RObject Robj = Rf_eval(RobjExpr, Renv);
    SEXP Sobj = static_cast<SEXP>(Robj);
    if(Sobj == R_NilValue) {
      Rcpp::stop("Problem: Could not obtain object for a ref arg.\n");
    }
    IndexArray indexArray( SEXP_indices_2_IndexArray<Index, nInd>(Sobj) );
    xCopy = SEXP_2_EigenTensor<Scalar, nInd>::
        template copy<EigenTensorType, IndexArray>(
          Sobj, indexArray);
    return xCopy; // compiler should use copy elision
  }
  ~nCompiler_EigenRef_SEXP_converter() {
//    Rprintf("goodbye to a tensor ref\n");
    Rcpp::RObject Rputback = Rcpp::wrap(xCopy);
    Language call("<-", RobjExpr, Rputback);
    Rf_eval(call, Renv);
  }
private:
  Rcpp::Nullable<Rcpp::List> RxList_;
  Rcpp::RObject RobjExpr;
  Rcpp::Environment Renv;
  IndexArray indexArray;
  EigenTensorType xCopy;
};

// We use Rcpp::RObject because we could have a symbol (name) or expression (language).
static Rcpp::RObject Rexpr_2_RvarExpr(Rcpp::RObject Rexpr) {
  if(Rcpp::is<Rcpp::Symbol>(Rexpr)) return Rexpr;
  if( Rcpp::is<Rcpp::Language>(Rexpr) ) {
    Rcpp::Language lang(Rexpr);
    if (lang[0] == Symbol("[")) {
      return static_cast<SEXP>(lang[1]);
    }
    return Rexpr;
  }
  Rcpp::stop("A refBlock argument should be an expression (with or without \"[]\").");
}

// From nimble
static int SEXP_2_int(SEXP Sn, int i, int offset ) {
  if(!(Rf_isNumeric(Sn) || Rf_isLogical(Sn))) PRINTF("Error: SEXP_2_int called for SEXP that is not numeric or logical\n");
  if(LENGTH(Sn) <= i) PRINTF("Error: SEXP_2_int called for element %i >= length of %i.\n", i, LENGTH(Sn));
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
std::vector<b__> Rinputs_2_indexBlockArray(Rcpp::RObject Rdata,
                                        Rcpp::RObject Rexpr,
                                        Rcpp::Environment Renv,
                                        InputArray &sizeArray) {
  // This will be called after Sexpr_2_data, so much of the checking
  // of Sexpr as a valid input will already be done.
  //
  // sizeArray is used if there are no indexing brackets.
  // It is also used to check that entries in indexing brackets
    //  are within bounds.
  
  int nDim = sizeArray.size();
  std::vector<b__> indexBlockArray(nDim);
  Rcpp::Language lang;
  bool isIndexed = false;
  if(Rcpp::is<Rcpp::Language>(Rexpr) ) {
    lang = Rcpp::Language(Rexpr);
    isIndexed = (lang[0] == Symbol("["));       
  }
  if(isIndexed) {
    if( (lang.size() - 2) != nDim ) {
      Rcpp::stop("Number of indices in refBlock does not match number of dimensions of object.");
    }
  }
  Rcpp::Language RcurrentIndex;
  for(int i = 0; i < nDim; ++i) {
    //std::cout<<"i = "<<i<<std::endl;
    if(isIndexed) {
      RcurrentIndex = lang[i + 2];
      if(Rcpp::is<Rcpp::Language>(RcurrentIndex)) { // should be `:`(start, end)
        Rcpp::Language indexLang(RcurrentIndex);
        if(indexLang[0] != Symbol(":")) {
          Rcpp::stop("Problem: Index in a refBlock argument has an operation that is not ':'.");
        }
        int first = SEXP_eval_to_single_int(indexLang[1], Renv);
        int last =  SEXP_eval_to_single_int(indexLang[2], Renv);
        //PRINTF("first %i last %i size %td\n", first, last, sizeArray[i]);
        if(first < 0) {
          PRINTF("Problem: First index in a range is <= 0. Using 0.\n");
          first = 0;
        }
        if(last > sizeArray[i] - 1) {
          PRINTF("Problem: Last index in a range is too large.  Using size of object instead.\n");
          last = sizeArray[i] - 1;
        }
        indexBlockArray[i] = b__(first, last);
      } else { // index entry is a number, a variable, or a blank.
        bool isBlank = false;
        if(Rcpp::is<Rcpp::Symbol>(RcurrentIndex)) {
          isBlank = Rcpp::Symbol(RcurrentIndex) == R_MissingArg;
        }
        if(isBlank) {
          //PRINTF("blank\n");
          indexBlockArray[i] = b__(0, sizeArray[i]-1);
        } else {
          indexBlockArray[i] = b__( SEXP_eval_to_single_int(RcurrentIndex, Renv) );
          //std::cout<<"Got singleton "<< SEXP_eval_to_single_int(RcurrentIndex, Renv)<<std::endl;
        }
      }
    } else {
      indexBlockArray[i] = b__(0, sizeArray[i]-1);
    }
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
  static bool matches_type(SEXP Sin) {
    return Rf_isReal(Sin);
  }
  static const SEXPTYPE Rtype = REALSXP;
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
  static bool matches_type(SEXP Sin) {
    return Rf_isInteger(Sin);
  }
  static const SEXPTYPE Rtype = INTSXP;
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
  static bool matches_type(SEXP Sin) {
    return Rf_isLogical(Sin);
  }
  static const SEXPTYPE Rtype = LGLSXP;

};

template<class fromT, class toT, int nDim, typename Index>
Eigen::Tensor<toT, nDim>
castedSTMcopy( fromT *  from,
                 const std::vector<Index> &indexArray,
                 const std::vector<b__> &indexBlockArray,
                 std::true_type same_types) {
  Eigen::StridedTensorMap<Eigen::Tensor<fromT, nDim> > xMap(from, indexArray, indexBlockArray);
  Eigen::Tensor<toT, nDim> to = xMap;
  return to;
}

template<class fromT, class toT, int nDim, typename Index>
Eigen::Tensor<toT, nDim>
castedSTMcopy( fromT *  from,
                 const std::vector<Index> &indexArray,
                 const std::vector<b__> &indexBlockArray,
                 std::false_type different_types) {
  Eigen::StridedTensorMap<Eigen::Tensor<fromT, nDim> > xMap(from, indexArray, indexBlockArray);
  Eigen::Tensor<toT, nDim> to = xMap.template cast<toT>();
  return to;
}

template<typename Scalar, int nInd> /* Scalar is the target scalar.  The input scalar is determined by TYPEOF(SINPUT). */
struct Rexpr_2_EigenTensor {
  typedef Eigen::Tensor<Scalar, nInd> EigenTensorType;
  typedef typename EigenTensorType::Index Index;
  //typedef typename Eigen::array<Index, nInd> IndexArray;

  static EigenTensorType copy(Rcpp::RObject &Rdata,
                              Rcpp::RObject &Rexpr,
                              Rcpp::Environment &Renv) {
    SEXP Sdata = static_cast<SEXP>(Rdata);
    std::vector<Index> indexArray(
      SEXP_indices_2_IndexArray_general<Index, std::vector<Index> >(
        static_cast<SEXP>(Rdata)));
    std::vector<b__> indexBlockArray(
      Rinputs_2_indexBlockArray(
        Rdata, Rexpr, Renv, indexArray));
    EigenTensorType xCopy;
    typedef typename std::is_same<Scalar, int>::type i_match_type;
    typedef typename std::is_same<Scalar, double>::type d_match_type;
    switch( TYPEOF(Sdata) ) {
    case REALSXP:
      //      std::cout<<"copying from REAL\n";
      xCopy =
        castedSTMcopy<double, Scalar, nInd, Index>(REAL(Sdata),
                                               indexArray,
                                               indexBlockArray,
                                               d_match_type()
                                               );
      break;
    case INTSXP:
      //      std::cout<<"copying from INTEGER\n";
      xCopy =
        castedSTMcopy<int, Scalar, nInd, Index>(INTEGER(Sdata),
                                            indexArray,
                                            indexBlockArray,
                                            i_match_type()
                                            );
      break;
    case LGLSXP:
      //      std::cout<<"copying from LOGICAL\n";
      // R represents logicals as int
      xCopy =
        castedSTMcopy<int, Scalar, nInd, Index>(INTEGER(Sdata),
                                            indexArray,
                                            indexBlockArray,
                                            i_match_type()
                                            );
      break;
    default:
      std::cout<<"Bad type\n"<<std::endl;
    }
    return xCopy; // compiler should use copy elision
  }
};

//
// Say we got f(x[6:10, 2]) as a refBlock double(1) argument.
// We will copy and store x,
// make a StridedTensorMap over that copy,
// and then evaluate "x <- our copy" upon destruction.
// That is the only way that if different slices of x are 
// passed in different refBlock arguments, they correctly access
// the same object.
// A problem is we DO NOT KNOW the underlying type of x
// Since we are getting some slice of it.
// So for an STM we might as copy the underlying R object.
template< typename Scalar, int nInd >
class nCompiler_StridedTensorMap_SEXP_converter {
 public:
  typedef Eigen::Tensor<Scalar, nInd> EigenTensorType;
  typedef Eigen::StridedTensorMap<EigenTensorType> StridedTensorMapType;
  typedef Eigen::Tensor<Scalar, nInd>& EigenTensorRefType;

  typedef typename EigenTensorType::Index Index;
// typedef typename Eigen::array<Index, nInd> IndexArray;

  static Rcpp::List check_input_Sx(Rcpp::Nullable<Rcpp::List> RxList_) {
    if(!(RxList_.isUsable()))
      Rcpp::stop("Problem: List was not provided for a refBlock arg.\n");
    Rcpp::List Rlist(RxList_);
    if(Rlist.size() != 2)
      Rcpp::stop("Problem: refBlock arg list should have two elements: expression and environment.\n");
    return Rlist;
  }

  nCompiler_StridedTensorMap_SEXP_converter(SEXP Sx) :
    Rlist(check_input_Sx(Sx)),
    Renv(Rlist[1]),
    Rexpr(Rlist[0]),
    RobjExpr(Rexpr_2_RvarExpr(Rexpr)),
    Rdata(Rf_eval(RobjExpr, Renv)),
    xCopy(Rexpr_2_EigenTensor<Scalar, nInd>
          ::copy(Rdata, Rexpr, Renv)),
    xMap(Eigen::MakeStridedTensorMap<nInd>::make(xCopy))
  {
  //  Rcpp::Language call("print", Rexpr);
  //  Rf_eval(call, Renv);
  //  std::cout<<"hello to a StridedTensorMap"<<std::endl;
  }

  operator StridedTensorMapType() {
    //xMap = Eigen::MakeStridedTensorMap<2>::make(ans, indexBlockArray);
  //  std::cout<<"handling my StridedTensorMap for function input"<<std::endl;
    return xMap; // compiler should use copy elision
  }
  ~nCompiler_StridedTensorMap_SEXP_converter() {
    Rcpp::RObject Rputback = Rcpp::wrap(xCopy);
    Rcpp::Language call("<-", Rexpr, Rputback);
    Rf_eval(call, Renv);
  }
 private:
  Rcpp::List Rlist;
  Rcpp::Environment Renv;
  Rcpp::RObject Rexpr;
  Rcpp::RObject RobjExpr;
  Rcpp::RObject Rdata;
  EigenTensorType xCopy;
  StridedTensorMapType xMap;
};

#endif // TENSOR_SEXP_CONVERTER_H_
