#ifndef _NCOMPILER_SEQ_CLASS
#define _NCOMPILER_SEQ_CLASS

#include "typedefs.h"

// TODO: find a better place for this
// A utility function that will return floor(x) unless x is within numerical imprecision of an integer, in which case it will return round(x)
int floorOrEquivalent(double x) {
  double roundX = round(x);
  double sqrtEpsilon = sqrt(std::numeric_limits<double>::epsilon());
  bool shouldBeExactInteger(false);
  if(fabs(x) > sqrtEpsilon) { // This algorithm for numerical equivalence imitates what happens in all.equal.numeric in R
    if(fabs(x - roundX) / fabs(x) < sqrtEpsilon)
      shouldBeExactInteger = true;
  } else {
    if(fabs(x - roundX) < sqrtEpsilon) // in the present context, this would mean length should be zero anyway
      shouldBeExactInteger = true;
  }
  if(shouldBeExactInteger)
    return(static_cast<int>(roundX));
  return(floor(x));
}

// a utility function used by seqClass and generated size expressions to determine the length of a sequence
template<typename fromT, typename toT, typename byT>
  int calcSeqLength(fromT from, toT to, byT by) { // we need this function because of imprecision issues
  double doubleLength = (static_cast<double>(to) - static_cast<double>(from))/static_cast<double>(by);
  return(1 + floorOrEquivalent(doubleLength));
}

typedef enum{useBy, useLength, useByAndLength} byOrLength;

template<typename DerivedOut, typename scalarFrom, typename scalarTo, typename scalarBy, byOrLength>
  class seqClass;

template<typename DerivedOut, typename scalarFrom, typename scalarTo, typename scalarBy>
  class seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useBy> {
 public:
  scalarFrom from;
  scalarBy by;
  unsigned int length_out;
  typedef typename DerivedOut::Index Index;
  typedef typename DerivedOut::Scalar result_type;
  
 seqClass(scalarFrom fromIn, scalarTo toIn, scalarBy byIn) :
  from(fromIn), by(byIn) {
    length_out = calcSeqLength(fromIn, toIn, byIn);
  };
  
  const result_type operator()(Index i) const
  {
    // printf("i: %ld\n", i);
    return( from + static_cast<int>(i) * by );
  }
};

namespace Eigen{
  namespace internal{
    template<typename DerivedOut, typename scalarFrom, typename scalarTo, typename scalarBy>
      struct functor_has_linear_access<seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useBy> > { enum { ret = 1 }; }; 
    template<typename DerivedOut, typename scalarFrom, typename scalarTo, typename scalarBy>
      struct functor_traits<seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useBy> >
      {
	enum
	{
	  Cost = 10, // there are templated costs available to pick up for this
	  PacketAccess = false, // happy to keep this false for now
	  IsRepeatable = true // default was false. 
	};
      };    
  }
}

template<typename DerivedOut>
struct seq_impl {
  // With returnType of DerivedOut, we force materialization of the Tensor.
  // However, using the return type TensorCwiseNullaryOp runs into scoping
  // issues.
  // seqBy
  template<typename scalarFrom, typename scalarTo, typename scalarBy>
  static DerivedOut seqBy(scalarFrom from, scalarTo to, scalarBy by) {
    typedef typename DerivedOut::Scalar result_type;
    seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useBy> seqObj(from, to, by);
    Eigen::TensorMap<DerivedOut> sizeProxy(static_cast<result_type*>(0), seqObj.length_out);
    return(sizeProxy.nullaryExpr(seqObj)); // seqObj is locally scoped
  }
  /* // seqLen */
  /* template<typename scalarFrom, typename scalarTo, typename scalarBy> */
  /*   static Eigen::TensorCwiseNullaryOp<seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useLength>, DerivedOut > seqLen(scalarFrom from, scalarTo to, scalarBy unsigned int len) { */
  /*   seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useLength> seqObj(from, to, len); */
  /*   return(Eigen::TensorCwiseNullaryOp<seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useLength> , DerivedOut >(seqObj.length_out, 1, seqObj)); */
  /* } */
  /* // seqByLen */
  /* template<typename scalarFrom, typename scalarTo, typename scalarBy> */
  /*   static Eigen::TensorCwiseNullaryOp<seqClass<DerivedOut, scalarFrom,  scalarTo, scalarBy, useByAndLength>, DerivedOut > seqByLen(scalarFrom from, scalarTo to, scalarBy by, unsigned int len) { */
  /*   seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useByAndLength> seqObj(from, by, len); */
  /*   return(Eigen::TensorCwiseNullaryOp<seqClass<DerivedOut, scalarFrom, scalarTo, scalarBy, useByAndLength> , DerivedOut >(seqObj.length_out, 1, seqObj)); */
  /* } */
};

#define nSeqByD seq_impl<Eigen1d>::seqBy

#endif
