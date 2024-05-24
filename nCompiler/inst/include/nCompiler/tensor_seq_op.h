#ifndef __TENSOR_SEQ_OP
#define __TENSOR_SEQ_OP

namespace Eigen {

  template<typename Scalar_>
  class SeqOp;


  namespace internal {
    template<typename Scalar_>
      struct traits<SeqOp<Scalar_> > : public traits< Tensor<Scalar_, 1> >
      {
        typedef Tensor<Scalar_, 1> XprType;
        typedef typename XprType::Scalar Scalar; // i.e. Scalar_
        typedef traits<XprType> XprTraits;
        typedef typename XprTraits::StorageKind StorageKind;
        typedef typename XprTraits::Index Index;
        typedef typename XprType::Nested Nested;
        typedef typename remove_reference<Nested>::type _Nested;
        static const int NumDimensions = XprTraits::NumDimensions;
        static const int Layout = XprTraits::Layout;
      };

template<typename Scalar_>
struct eval<SeqOp<Scalar_>, Eigen::Dense>
{
  typedef const SeqOp<Scalar_>& type;
};

template<typename Scalar_>
struct nested<SeqOp<Scalar_>, 1, typename eval<SeqOp<Scalar_> >::type>
{
  typedef SeqOp<Scalar_> type;
};

    // is_input_scalar
    //
    // is_input_scalar
    //
    // is_input_scalar

  } // end namespace internal
    //

  template<typename Scalar_>
    class SeqOp : public TensorBase<SeqOp<Scalar_>, ReadOnlyAccessors>
    {
    public:
      typedef typename Eigen::internal::traits<SeqOp>::Scalar Scalar;
      typedef typename Eigen::NumTraits<Scalar>::Real RealScalar;
      typedef Tensor<Scalar_, 1> XprType;
      typedef typename XprType::CoeffReturnType CoeffReturnType;
      typedef typename Eigen::internal::nested<SeqOp>::type Nested;
      typedef typename Eigen::internal::traits<SeqOp>::StorageKind StorageKind;
      typedef typename Eigen::internal::traits<SeqOp>::Index Index;

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE SeqOp(const Scalar_& Start, const Scalar_& End,
                                                  const Scalar_& By, const Index& LengthOut)
        :m_Start(Start), m_End(End), m_By(By), m_LengthOut(LengthOut) {
        //std::cout<<"making SeqOp"<<std::endl;
      }

      /*   EIGEN_DEVICE_FUNC */
      /*   const Broadcast& broadcast() const { return m_broadcast; } */

      /*   EIGEN_DEVICE_FUNC */
      /*   const typename internal::remove_all<typename XprType::Nested>::type& */
      /*   expression() const { return m_xpr; } */

      EIGEN_DEVICE_FUNC
        const Scalar_& Start() const {return m_Start;}
        const Scalar_& End() const {return m_End;}
        const Scalar_& By() const {return m_By;}
        const Index& LengthOut() const {return m_LengthOut;}

    protected:
      const Scalar_ m_Start;
      const Scalar_ m_End;
      const Scalar_ m_By;
      const Index m_LengthOut;
    };

  // Eval as rvalue
template<typename Scalar_, typename Device>
struct TensorEvaluator<const SeqOp<Scalar_>, Device>
{
  typedef SeqOp<Scalar_> XprType;
  typedef typename XprType::Index Index;
  static const int NumDims = 1;
  typedef DSizes<Index, NumDims> Dimensions;
  typedef typename XprType::Scalar Scalar;
  typedef Tensor<Scalar_, 1> ArgType; // Added to keep next changes minimal
  typedef typename TensorEvaluator<ArgType, Device>::Dimensions InputDimensions;
  typedef typename XprType::CoeffReturnType CoeffReturnType;
  typedef typename PacketType<CoeffReturnType, Device>::type PacketReturnType;
  static const int PacketSize = internal::unpacket_traits<PacketReturnType>::size;

  enum {
    IsAligned = true,
    PacketAccess = TensorEvaluator<ArgType, Device>::PacketAccess,
    Layout = TensorEvaluator<ArgType, Device>::Layout,
    RawAccess = false
  };

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorEvaluator(const XprType& op, const Device& device)
    : m_Start(op.Start()),m_End(op.End()),m_By(op.By()),m_LengthOut(op.LengthOut())
  {
    //std::cout<<"making TensorEvaluator<SeqOp> "<<m_LengthOut<<std::endl;
    m_dimensions[0] = m_LengthOut;
  }

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE const Dimensions& dimensions() const { return m_dimensions; }

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE bool evalSubExprsIfNeeded(Scalar* /*data*/) {
    return true;
  }

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE void cleanup() {
  }

  EIGEN_DEVICE_FUNC EIGEN_ALWAYS_INLINE CoeffReturnType coeff(Index index) const
  {
    //std::cout<<"returning coeff "<<index<<std::endl;
    return m_Start + index * m_By;
  }

  template<int LoadMode>
  EIGEN_DEVICE_FUNC EIGEN_ALWAYS_INLINE PacketReturnType packet(Index index) const
  {
    //std::cout<<"returning packet "<<index<<std::endl;
    eigen_assert(index+PacketSize-1 < m_LengthOut);

    EIGEN_ALIGN_MAX typename internal::remove_const<CoeffReturnType>::type values[PacketSize];

    values[0] = coeff(index);
    for (int i = 1; i < PacketSize; ++i) {
      values[i] = values[i-1] + m_By; // Could this lead to different rounded outputs than doing By*index each time?
    }
    PacketReturnType rslt = internal::pload<PacketReturnType>(values);
    return rslt;
  }

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorOpCost
  costPerCoeff(bool vectorized) const {
    // I am not sure if we need to multiply by packet size for vectorized = true,
    // or adjust for doing repeated Adds and only one Mult at the start?
    // But this looks something like the pattern to imitate.
    double compute_cost = TensorOpCost::MulCost<Index>() + TensorOpCost::AddCost<Index>();
    return TensorOpCost(0, 0, compute_cost, vectorized, PacketSize);
  }

  EIGEN_DEVICE_FUNC Scalar* data() const { return NULL; }

 protected:
      const Scalar_ m_Start;
      const Scalar_ m_End;
      const Scalar_ m_By;
      const Index m_LengthOut;
      Dimensions m_dimensions;
 };
} // End Eigen namespace

// seq cases:
// seq(scalar) returns 1:floor(scalar), i.e. seq(1, scalar, by = 1)
// seq(non-scalar) returns 1:length(non-scalar), i.e. seq(1, length(non-scalar), by = 1)
// seq(from, to) returns seq(from, to, by = 1), even if from is non-integer (i.e. result will be non-integer)
// seq(to) uses from = 1 and by = +1 or -1 depending on to > from or to < from
// seq(by = 1) returns 1
// seq(from, by) assumes to = 1
// seq(to, by) assumes from = 1
// seq(from, to, by) is the base case
// seq(length.out) uses from = 1, by = 1
// seq(from, length.out) uses by = 1
// seq(to, length.out) uses by = 1
// seq(from, to, length.out) calculate a by value
//
// There shouldn't be any type mismatching between static (nCompiler) and dynamic typing (R)
// because all returned types will be vectors.
//
// Run-time behavior is needed for seq(non-scalar)
// If length of the non-scalar is 1, it will be treated like seq(scalar)


template<typename Scalar_>
auto nSeqBy(const Scalar_& Start, const Scalar_& End, const Scalar_& By ) -> Eigen::SeqOp<Scalar_> {
  // This handles:
  // seq(by = 1) returns 1
  // seq(from, by) assumes to = 1
  // seq(to, by) assumes from = 1
  // seq(from, to, by) is the base case
  // Generated code for these will insert from = 1, by = 1 as defaults, which achieves all the behaviors
  //
  // Type issues: we will do type promotion among start, end, and by.
  //   e.g. for all integer, result is integer.  But for any double, result is double.

  // The relative buffer of 1e-10 comes from the description in help(seq)
  if(By==0) {
    Rcpp::stop("Bad argument: by=0 is not allowed.");
  }
  const typename Eigen::template Tensor<Scalar_, 1>::Index LengthOut( 1 + ((1.+1e-10)*(static_cast<double>(End)-static_cast<double>(Start)))/static_cast<double>(By) );
  if(LengthOut <= 0) {
    Rcpp::stop("Bad arguments in seq() using by=.");
  }
  return Eigen::SeqOp<Scalar_>(Start, End, By, LengthOut);
}

template<typename Scalar_>
auto nSeqTo(const Scalar_ &End) -> Eigen::SeqOp<int> {
  // seq(to) uses from = 1 and by = +1 or -1 depending on to > from or to < from
  // Type issues: This case always return integer in R, so we do too.
  Scalar_ Start = 1;
  Scalar_ By = End>=Start ? 1 : -1;
  const typename Eigen::template Tensor<Scalar_, 1>::Index LengthOut( 1 + ((1.+1e-10)*(static_cast<double>(End)-static_cast<double>(Start)))/static_cast<double>(By) );
  if(LengthOut <= 0) {
    Rcpp::stop("Bad arguments in seq() using to=.");
  }
  // Can't just call nSeqBy because we'd cast to int and get it wrong if End==0.5, e.g.
  return Eigen::SeqOp<int>(Start, End, By, LengthOut);
}

Eigen::SeqOp<double> nSeqLen(const double& Start, const double& End,
                             const double& LengthOut_ ) {
// seq(from, to, length.out) calculate a by value
// Type issues: We never want length passed in as an integer because the casting will
// round it, not ceil it as needed (next line).
//
// R's return type is dynamically determined in the same way as for nSeqBy:
//      It is type promoted among all three arguments.
// However there is also here the problem that By is calculated here
//   and if it's an integer, R will allow integer return, but otherwise numeric return.
//
// We can't have run-time return type, so we always return double.
// For that reason, there's no point in templating the Start and End types.
// *** HENCE RETURN TYPES MAY DIFFER FROM R's ***
  const typename Eigen::template Tensor<double, 1>::Index LengthOut = ceil(LengthOut_);
  if(LengthOut < 0) {
    Rcpp::stop("Invalid seq call with negative length.out.");
  }
  double By = LengthOut == 1 ? 0 : (LengthOut == 0 ? 0 : (End - Start)/(LengthOut-1.));
  return Eigen::SeqOp<double>(Start, End, By, LengthOut);
}

template<typename Scalar_>
auto nSeqLenFrom(const Scalar_& Start,
                 const double& LengthOut_ ) -> Eigen::SeqOp<Scalar_> {
// seq(length.out) uses from = 1, by = 1, which will be code-generated to seq(1, length.out)
// seq(from, length.out) uses by = 1
//
// Type issues: We always take length as double so that we do ceil, rather a possibly rounded cast to int
// In R, if both from (Start) and length.out are integer, result is integer.
// We will type-promote in code-generation, so the Scalar_ will be what we want.
  const typename Eigen::template Tensor<Scalar_, 1>::Index LengthOut = ceil(LengthOut_);
  if(LengthOut < 0) {
    Rcpp::stop("Invalid seq call with negative length.out.");
  }
  Scalar_ By = 1;
  Scalar_ End = LengthOut == 1 ? Start : (LengthOut == 0 ? Start : Start + By*(LengthOut-1));
  return Eigen::SeqOp<Scalar_>(Start, End, By, LengthOut);
}

template<typename Scalar_>
auto nSeqLenTo(const Scalar_& End,
               const double& LengthOut_ ) -> Eigen::SeqOp<Scalar_> {
// seq(to, length.out) uses by = 1
//
// Type issues: Same as for nSeqLenFrom
  const typename Eigen::template Tensor<Scalar_, 1>::Index LengthOut = ceil(LengthOut_);
  if(LengthOut < 0) {
    Rcpp::stop("Invalid seq call with negative length.out.");
  }
  Scalar_ By = 1;
  Scalar_ Start = LengthOut == 1 ? End : (LengthOut == 0 ? End : End - By*(LengthOut-1));
  return Eigen::SeqOp<Scalar_>(Start, End, By, LengthOut);
}

template<typename Scalar_>
auto nSeqFromTo(const Scalar_ Start, const double& End) -> Eigen::SeqOp<Scalar_> {
  // seq(from, to)
  // Type issues: Return type is determined by Start only, not by End.
  // This is how R does it.
  // We can't just call nSeqBy with End cast to Scalar_, because
  // if Scalar_ is int, End could get rounded, and that could make
  // the sequence one longer than wanted.
  Scalar_ By = Start <= End ? 1 : -1;
  const typename Eigen::template Tensor<Scalar_, 1>::Index LengthOut( 1 + ((1.+1e-10)*(static_cast<double>(End)-static_cast<double>(Start)))/static_cast<double>(By) );
  if(LengthOut <= 0) {
    Rcpp::stop("Bad arguments in seq() using to=.");
  }
  double dEnd = static_cast<double>(End);
  Scalar_ uEnd = static_cast<Scalar_>(dEnd >= static_cast<double>(Start) ? floor(dEnd) : ceil(dEnd));
  return Eigen::SeqOp<Scalar_>(Start, uEnd, By, LengthOut);
}

  // nSeqSingle_impl will handle cases with only one argument
template<typename T>
struct nSeqSingle_impl;

  // General case: It is an Eigen object and we check at run-time if it is length=1
  // These cases always return integer in R
template<typename T>
struct nSeqSingle_impl {
  typedef typename T::Scalar Scalar;
    static Eigen::SeqOp<int> go(const T& x) {
    long xsize = nDimTraits2_size(x);
    if(xsize == 1) {
      Scalar v;
      flex_(v) = x;
      return nSeqSingle_impl<Scalar>::go( v );
    } else {
      if(xsize == 0)
        return Eigen::SeqOp<int>(1, 1, 0, 0);
    }
    return nSeqBy<int>(1, xsize, 1);
  }
};

  template<>
struct nSeqSingle_impl<double>  {
  typedef double Scalar;
  static Eigen::SeqOp<int> go(const double& x) {
    return nSeqTo<double>( x );
  }
};

template<>
struct nSeqSingle_impl<int>  {
  typedef int Scalar;
  static Eigen::SeqOp<int> go(const int& x) {
    return nSeqTo<int>( x );
  }
};

template<>
struct nSeqSingle_impl<bool>  {
  typedef bool Scalar;
  static Eigen::SeqOp<int> go(const bool& x) {
    return nSeqBy<int>(1, 1, 1);
  }
};

  // Calling point for generated code.
  // These cases always return integer.
template<typename T>
auto nSeqSingle(const T& x) -> Eigen::SeqOp<int> {
  return nSeqSingle_impl<T>::go(x);
}

// General case for testing purposes. Not actually used by any generated code.
template<typename Scalar_>
auto nSeqGeneral(const Scalar_& Start, const Scalar_& End, const Scalar_& By, const typename Eigen::template Tensor<Scalar_, 1>::Index LengthOut ) -> Eigen::SeqOp<Scalar_> {
  return Eigen::SeqOp<Scalar_>(Start, End, By, LengthOut);
}

#endif // __TENSOR_SEQ_OP
