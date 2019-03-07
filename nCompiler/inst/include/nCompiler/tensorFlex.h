#ifndef __TENSOR_FLEX
#define __TENSOR_FLEX

// This file supports code like
// flex_(x) = y
// where flex_ creates a class with an overloaded operator=
// that can do smart things to accept assignment from y.
// The purposes is to simplify code-generation from nCompiler and
// also to handle decisions that can only be made at run-time.
//
// This system represents a first draft, and a more general system
// is envisioned.

#include <type_traits>
// Want to support flex_(y) = x;

// These structs are used for tag dispatch.
// A function can take an argument of one of these types that is never used but
// serves to determine which over-loaded function definition should be used.
struct same {};
struct less {};
struct more {};
struct zeroDim {};
struct nonZeroDim {};
struct trueScalar {};
struct eigenOp {};
struct eigenTensor {};

// type_category<SomeType>::type will be either trueScalar (for double, int or bool),
// eigenTensor (for Eigen::Tensor< > ), or eigenOp (default for anything else).
  template<typename T>
  struct type_category {
    typedef eigenOp type; // default
  };
  
  template<typename ScalarType, int nDim>
  struct type_category<Eigen::Tensor<ScalarType, nDim> > {
    typedef eigenTensor type;
  };
  
  template<>
  struct type_category<double> {
    typedef trueScalar type;
  };
  template<>
  struct type_category<int> {
    typedef trueScalar type;
  };
  template<>
  struct type_category<bool> {
    typedef trueScalar type;
  };
  

// checkDimsAllOne returns true if all dimensions are 1s, false otherwise
template<int Ndim>
bool checkDimsAllOne(const Eigen::array<long, Ndim> &dims) {
    typedef typename Eigen::array<long, Ndim>::const_iterator iterType;
  iterType iDims(dims.begin());
  for(; iDims != dims.end();) {
    if(*iDims++ != 1)
      return false;
  }
  return true;
}

// This function would be used for an assignment where the sizes of the LHS can't change.
// example: If we have A[1:10] <- B, where B is a matrix or array with nDim > 1,
// then the assignment is valid if all dimensions of B have size 1 except for one dimension
// that has size 10.  This function checks returns true if that is the case.
// Note that we may have a case like: A[1:10, 1, drop = FALSE] <- B, where B is a 3D array.
// B needs to have one dimension that is 10 and then be assigned to A as a 10x1 2D array.
template<int LessNdim, int MoreNdim>
  bool checkDims(const Eigen::array<long, LessNdim> &LessDims,
		 const Eigen::array<long, MoreNdim> &MoreDims) {
  // std::cout<<"In checkDims with LessNdim = "<<LessNdim<<" and MoreNdim = "<<MoreNdim<<std::endl;
  //  for(size_t i = 0; i < LessNdim; ++i) std::cout<<LessDims[i]<<" "; std::cout<<std::endl;
  //  for(size_t i = 0; i < MoreNdim; ++i) std::cout<<MoreDims[i]<<" "; std::cout<<std::endl;
  // Check that non-1 dimensions match
  typedef typename Eigen::array<long, LessNdim>::const_iterator LessDimsIterator;
  typedef typename Eigen::array<long, MoreNdim>::const_iterator MoreDimsIterator;

  LessDimsIterator iLess(LessDims.begin());
  // Move to the first non-1 dimension of LessDims
  while(*iLess == 1 & iLess != LessDims.end()) {
    ++iLess;
  }
  // iterate through dimensions of the larger one.
  // there should be exactly as many non-1 dimensions as the size of the shorter 1's non-1 dimensions.
  for(MoreDimsIterator iMore = MoreDims.begin();
      iMore != MoreDims.end();
      ++iMore) {
    if(*iMore != 1) { // If we found a non-1 dimension of MoreDims
      if(iLess == LessDims.end())  // If there are no more non-1 dimensions of LessDims: Fail
	return false;
      if(*iMore != *iLess) { // If the current non-1 dimensions don't match: Fail
	return false;
      }
      ++iLess; // Definitely increment iLess at least once.
      while(*iLess == 1 & iLess != LessDims.end()) { // Keep incrementing to the next non-1 dimension or the end
	++iLess;
      }
    }
  }
  // check that any remaining LessDims are all 1
  for( ; iLess != LessDims.end(); ++iLess) {
    if(*iLess != 1)
      return false;
  }
  return true;
}

// This function would be used for an assignment where the sizes of the LHS can change.
// example: If we have A <- X %*% b, where A is a vector and X %*% b returns a 10x1 vector at run-time.
// This function checks that the number of non-1 dimensions on the RHS is <= nDim of A.
// If the RHS has *fewer* non-1 dimensions that nDim of A, then dimensions of size 1 will be padded on.
// This function populates values of LhsDims so it can be an argument to reshape on the RHS.
template<int LhsNdim, int RhsNdim>
  bool checkAndSetupDims(Eigen::array<long, LhsNdim> &LhsDims,
			 const Eigen::array<long, RhsNdim> &RhsDims) {
    typedef typename Eigen::array<long, LhsNdim>::iterator LhsDimsIterator;
    typedef typename Eigen::array<long, RhsNdim>::const_iterator RhsDimsIterator;
    LhsDimsIterator iLhs(LhsDims.begin());
    for(RhsDimsIterator iRhs = RhsDims.begin();
        iRhs != RhsDims.end();
        ++iRhs) {
      if(*iRhs != 1) { // found a non-1 dimension
        if(iLhs == LhsDims.end()) // Ran out of Lhs dimensions: fail
          return false;
        *iLhs++ = *iRhs;
      }
    }
    // fill any remaining Lhs dimensions with ones
    for( ; iLhs != LhsDims.end(); ) {
      *iLhs++ = 1;
    }
    return true;
}

// nDimTraits<SomeEigenType>::NumDimensions will give the number of dimensions of SomeEigenType
// nDimTraits<SomeEigenType>::Dimensions will give a valid type for dimensions.
// nDimTraits<SomeEigenType>::EvaluatorType give the type returned by:
// nDimTraits<SomeEigenType>::getEvaluator() will return an EvaluatorType, from which
// one can get a reference to .dimensions().
template<typename XprType>
struct nDimTraits {
  static const int NumDimensions = Eigen::internal::traits<XprType>::NumDimensions;
  typedef Eigen::TensorEvaluator<XprType, Eigen::DefaultDevice> EvaluatorType;
  //typedef typename EvaluatorType::Dimensions Dimensions;
  typedef Eigen::array<long, NumDimensions> Dimensions;

  static const EvaluatorType getEvaluator(const XprType &x) {
    const typename Eigen::internal::remove_all<typename XprType::Nested>::type x_removed(x);
    // careful: m_impl might go out of scope
    return EvaluatorType(x_removed,
			 Eigen::DefaultDevice());
  }
  // We previously experimented with a getDimensions function here, but then the evaluator
  // is instantiated here, so the dimensions can't be returned by reference.
  // Hence it seems better to return an evalautor to the calling function, where it
  // will stay in scope for use of a reference returned by its .dimensions()
  /* static const Dimensions getDimensions(const XprType &x) { */
  /*   const typename Eigen::internal::remove_all<typename XprType::Nested>::type x_removed(x); */
  /*   // careful: m_impl might go out of scope */
  /*   Eigen::TensorEvaluator<XprType, Eigen::DefaultDevice> m_impl( */
  /* 								 x_removed, */
  /* 								 Eigen::DefaultDevice()); */
  /*   return m_impl.dimensions(); */
  /* } */
};

template<>
struct nDimTraits<double> {
  static const int NumDimensions = 0;
};
template<>
struct nDimTraits<int> {
  static const int NumDimensions = 0;
};
template<>
struct nDimTraits<bool> {
  static const int NumDimensions = 0;
};

// compare_nDim<SomeEigenType1,  SomeEigenType2>::type will be either less, same, or more,
// depending on whether LHSnDim is <, ==, or > than RHSnDim.
template<typename Xpr1, typename Xpr2>
  struct compare_nDim {
    static const int nDim1 = nDimTraits<Xpr1>::NumDimensions;
    static const int nDim2 = nDimTraits<Xpr2>::NumDimensions;
    typedef typename std::conditional<nDim1 == nDim2, 
      same, 
      typename std::conditional<nDim1 <= nDim2,
      less,
      more>::type >::type type;
 };

// is_zeroDim<SomeEigenType>::type will be zeroDim or nonZeroDim depending on whether
// someEigenType is 0-dimensional.
// This is used for tag dispatch, whereas nDimTraits<>::NumDimensions is used
// when the number of dimensions is needed as a template argument or actual value.
template<typename Xpr1>
struct is_zeroDim {
  static const int nDim1 = nDimTraits<Xpr1>::NumDimensions;
  typedef typename std::conditional<nDim1 == 0, 
                                    zeroDim,
                                    nonZeroDim>::type type;
};
template<>
struct is_zeroDim<double> {
  typedef zeroDim type; // This will be over-ridden by true_scalar in the next tag argument
  // but nevertheless we need this template to evaluate correctly.
};
template<>
struct is_zeroDim<int> {
  typedef zeroDim type; // This will be over-ridden by true_scalar in the next tag argument
  // but nevertheless we need this template to evaluate correctly.
};
template<>
struct is_zeroDim<bool> {
  typedef zeroDim type; // This will be over-ridden by true_scalar in the next tag argument
  // but nevertheless we need this template to evaluate correctly.
};

// smartAssignFixedSize functions handle cases where the sizes
// of the LHS can't change.  It is tag dispatched on whether
// the nDim of the LHS is <, ==, or > the nDim of the RHS.
// Lhs has fewer dimensions than Rhs
template<typename LhsXprType, typename RhsXprType>
LhsXprType& smartAssignFixedSize(LhsXprType &Lhs, const RhsXprType &Rhs, less) {
  static const int LhsNumDim = nDimTraits<LhsXprType>::NumDimensions;
  static const int RhsNumDim = nDimTraits<RhsXprType>::NumDimensions;
  typedef typename nDimTraits<LhsXprType>::EvaluatorType LhsEvaluatorType;
  typedef typename nDimTraits<LhsXprType>::Dimensions LhsDimensions;
  LhsEvaluatorType LhsEvaluator( nDimTraits<LhsXprType>::getEvaluator(Lhs) );
  const LhsDimensions &LhsDims = LhsEvaluator.dimensions();

  typedef typename nDimTraits<RhsXprType>::EvaluatorType RhsEvaluatorType;
  typedef typename nDimTraits<RhsXprType>::Dimensions RhsDimensions;
  RhsEvaluatorType RhsEvaluator( nDimTraits<RhsXprType>::getEvaluator(Rhs) );
  const RhsDimensions &RhsDims = RhsEvaluator.dimensions();

  //  std::cout<<"In LHSND < RHSND"<<std::endl;
  //std::cout<<"Lhs dim[0] = "<<LhsDims[0]<<std::endl;
  bool ok = checkDims<LhsNumDim, RhsNumDim>(LhsDims, RhsDims);
  if(!ok) {
    std::cout<<"Error: dimension mismatch\n"<<std::endl;
    return Lhs;
  }
  Lhs = Rhs.reshape(LhsDims);
  return Lhs;
}

// Lhs has same number of dimensions as Rhs
template<typename LhsXprType, typename RhsXprType>
LhsXprType& smartAssignFixedSize(LhsXprType &Lhs, const RhsXprType &Rhs, same) {
  Lhs = Rhs;
  return Lhs;
}

// Lhs has more dimensions than Rhs
template<typename LhsXprType, typename RhsXprType>
LhsXprType& smartAssignFixedSize(LhsXprType &Lhs, const RhsXprType &Rhs, more) {
  std::cout<<"more case need to be done"<<std::endl;
    Lhs = Rhs;
  return Lhs;
}

// flex__ is the key class.
// Usage is flex_(x) = expr.
// Note that flex_(x) returns a flex__<type of x> object.
// This case is for fixed-size Lhs
template<typename xType>
class flex__ {
public:
  xType &x;
  explicit flex__(xType &x_) : x(x_) {
    //  std::cout<<"building flex__ for fixed-size Lhs"<<std::endl;
  }
  template<typename oType>
  xType& operator=(const oType &other) {
    // std::cout<<"RHS NumDims = "<< nDimTraits<oType>::NumDimensions <<std::endl;
    // For now, assume nDim(oType) > nDim(xType). Figure out dispatching later.
    return smartAssignFixedSize(x, other,
                                typename compare_nDim<xType, oType>::type());
  }
};

// smartAssignWholeObject handles assignment when the sizes of the LHS
// can be changed because the entire object is assigned to.
// This is tag dispatched similarly to smartAssignFixedSize
// Lhs has fewer dimensions than Rhs
template<typename LhsXprType, typename RhsXprType>
LhsXprType& smartAssignWholeObject(LhsXprType &Lhs, const RhsXprType &Rhs, less) {
  static const int LhsNumDim = nDimTraits<LhsXprType>::NumDimensions;
  static const int RhsNumDim = nDimTraits<RhsXprType>::NumDimensions;
  typedef typename nDimTraits<LhsXprType>::Dimensions LhsDimensions;

  LhsDimensions NewLhsDims;
  
  typedef typename nDimTraits<RhsXprType>::EvaluatorType RhsEvaluatorType;
  typedef typename nDimTraits<RhsXprType>::Dimensions RhsDimensions;
  RhsEvaluatorType RhsEvaluator( nDimTraits<RhsXprType>::getEvaluator(Rhs) );
  const RhsDimensions &RhsDims = RhsEvaluator.dimensions();
  
  //  std::cout<<"In smartAssignWholeObject for LHSND < RHSND"<<std::endl;
  bool ok = checkAndSetupDims<LhsNumDim, RhsNumDim>(NewLhsDims, RhsDims);
  if(!ok) {
    std::cout<<"Error: dimension mismatch\n"<<std::endl;
    return Lhs;
  }
  Lhs = Rhs.reshape(NewLhsDims);
  return Lhs;
}
// Lhs has same number of dimensions as Rhs
template<typename LhsXprType, typename RhsXprType>
LhsXprType& smartAssignWholeObject(LhsXprType &Lhs, const RhsXprType &Rhs, same) {
  Lhs = Rhs;
  return Lhs;
}

// Lhs has more dimensions than Rhs
template<typename LhsXprType, typename RhsXprType>
LhsXprType& smartAssignWholeObject(LhsXprType &Lhs, const RhsXprType &Rhs, more) {
  std::cout<<"more case need to be done"<<std::endl;
  Lhs = Rhs;
  return Lhs;
}

//smartAssignToScalar handles cases like A  <- B where A is a true scalar (e.g. double),
// and at run-time we need to see if a potentially non-scalar B does in fact have size of 1.
// This is tag dispatched on two arguments: whether B is inherently zero-dimensional, such as
// what is returned from many Eigen::Tensor reduction operators, and a tag for the type_category of B,
// either eigenTensor, eigenOp, or trueScalar.
template<typename ScalarType, typename RhsXprType>
ScalarType& smartAssignToScalar(ScalarType &Lhs, const RhsXprType &Rhs, zeroDim, trueScalar) {
  // std::cout<<"In smartAssignToScalar zeroDim trueScalar"<<std::endl;
  Lhs = Rhs;
  return Lhs;
}
template<typename ScalarType, typename RhsXprType>
ScalarType& smartAssignToScalar(ScalarType &Lhs, const RhsXprType &Rhs, zeroDim, eigenTensor) {
  // std::cout<<"In smartAssignToScalar zeroDim eigenTensor"<<std::endl;
  Lhs = Rhs();
  return Lhs;
}

template<typename ScalarType, typename RhsXprType>
ScalarType& smartAssignToScalar(ScalarType &Lhs, const RhsXprType &Rhs, zeroDim, eigenOp) {
  // std::cout<<"In smartAssignToScalar zeroDim eigenOp"<<std::endl;
  Eigen::Tensor<ScalarType, 0> ans = Rhs;
  Lhs = ans();
  return Lhs;
}

template<typename ScalarType, typename RhsXprType>
ScalarType& smartAssignToScalar(ScalarType &Lhs, const RhsXprType &Rhs, nonZeroDim, eigenOp) {
  static const int RhsNumDim = nDimTraits<RhsXprType>::NumDimensions;
  
  typedef typename nDimTraits<RhsXprType>::EvaluatorType RhsEvaluatorType;
  typedef typename nDimTraits<RhsXprType>::Dimensions RhsDimensions;
  RhsEvaluatorType RhsEvaluator( nDimTraits<RhsXprType>::getEvaluator(Rhs) );
  const RhsDimensions &RhsDims = RhsEvaluator.dimensions();
  
  //  std::cout<<"In smartAssignToScalar nonzeroDim eigenOp"<<std::endl;
  
  bool ok = checkDimsAllOne<RhsNumDim>(RhsDims);
  if(!ok) {
    std::cout<<"Error: dimension mismatch\n"<<std::endl;
    return Lhs;
  }
  Lhs = Rhs.eval();
  return Lhs;
}

template<typename ScalarType, typename RhsXprType>
ScalarType& smartAssignToScalar(ScalarType &Lhs, const RhsXprType &Rhs, nonZeroDim, eigenTensor) {
  static const int RhsNumDim = nDimTraits<RhsXprType>::NumDimensions;
  
  typedef typename nDimTraits<RhsXprType>::EvaluatorType RhsEvaluatorType;
  typedef typename nDimTraits<RhsXprType>::Dimensions RhsDimensions;
  RhsEvaluatorType RhsEvaluator( nDimTraits<RhsXprType>::getEvaluator(Rhs) );
  const RhsDimensions &RhsDims = RhsEvaluator.dimensions();
  
  //  std::cout<<"In smartAssignToScalar nonzeroDim eigenTensor (GET INFO MORE DIRECTLY)"<<std::endl;
  
  bool ok = checkDimsAllOne<RhsNumDim>(RhsDims);
  if(!ok) {
    std::cout<<"Error: dimension mismatch\n"<<std::endl;
    return Lhs;
  }
  Lhs = Rhs(0);
  return Lhs;
}

// Specialization of flex_ to whole Eigen objects
template<typename ScalarType, int nDim>
class flex__<Eigen::Tensor<ScalarType, nDim> > {
public:
  typedef Eigen::Tensor<ScalarType, nDim> xType;
  xType &x;
  explicit flex__(xType &x_) : x(x_) {
    //  std::cout<<"building flex__ for flexible-size Lhs"<<std::endl;
  }
  template<typename oType>
  xType& operator=(const oType &other) {
    // std::cout<<"RHS NumDims = "<< nDimTraits<oType>::NumDimensions <<std::endl;
    // For now, assume nDim(oType) > nDim(xType). Figure out dispatching later.
    return smartAssignWholeObject(x, other,
                       typename compare_nDim<xType, oType>::type());
  }
};

// // Specialization of flex_ to scalar cases for double, int and bool.
// There should be a nice way to do this (like make flex_ use a get_flex_type struct), but for now I'm doing it directly:
template<>
class flex__<double> {
public:
  typedef double ScalarType;
  ScalarType &x;
  explicit flex__(ScalarType &x_) : x(x_) {
    //  std::cout<<"building flex__ for flexible-size Lhs"<<std::endl;
  }
  template<typename oType>
  ScalarType& operator=(const oType &other) {
    // std::cout<<"RHS NumDims = "<< nDimTraits<oType>::NumDimensions <<std::endl;
    // For now, assume nDim(oType) > nDim(xType). Figure out dispatching later.
    return smartAssignToScalar(x, other, 
                               typename is_zeroDim<oType>::type(),
                               typename type_category<oType>::type());
  }
};

template<>
class flex__<int> {
public:
  typedef int ScalarType;
  ScalarType &x;
  explicit flex__(ScalarType &x_) : x(x_) {
    //  std::cout<<"building flex__ for flexible-size Lhs"<<std::endl;
  }
  template<typename oType>
  ScalarType& operator=(const oType &other) {
    //std::cout<<"RHS NumDims = "<< nDimTraits<oType>::NumDimensions <<std::endl;
    // For now, assume nDim(oType) > nDim(xType). Figure out dispatching later.
   return smartAssignToScalar(x, other, 
                               typename is_zeroDim<oType>::type(),
                               typename type_category<oType>::type());
  }
};

template<>
class flex__<bool> {
public:
  typedef bool ScalarType;
  ScalarType &x;
  explicit flex__(ScalarType &x_) : x(x_) {
    //  std::cout<<"building flex__ for flexible-size Lhs"<<std::endl;
  }
  template<typename oType>
  ScalarType& operator=(const oType &other) {
    // std::cout<<"RHS NumDims = "<< nDimTraits<oType>::NumDimensions <<std::endl;
    // For now, assume nDim(oType) > nDim(xType). Figure out dispatching later.
   return smartAssignToScalar(x, other, 
                               typename is_zeroDim<oType>::type(),
                               typename type_category<oType>::type());
  }
};
// Notes and ideas:
// maybe try using decltype, constexpr
// maybe make constexpr on condition of LHSNumDim vs. RHSNumDim and dispatch on that.

// It is difficult to explicity name all Eigen types when creating a flex_.
// The following templated function can deduce type easily.
// This means we can code flex_(x) = foo(y);
template<typename xType>
flex__<xType> flex_(xType &x_) {return flex__<xType>(x_);}

#endif
