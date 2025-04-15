#ifndef _NCOMPILER_RECYCLYING_RULE
#define _NCOMPILER_RECYCLYING_RULE

#include <unsupported/Eigen/CXX11/Tensor>
#include "typedefs.h"

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


namespace Eigen {

// forward declaration of an Eigen tensor operation for a recycling rule
template<typename LeftXprType, typename RightXprType> class TensorCwiseRecyclingOp;

namespace internal {

//////////// Modified from TensorCwiseBinaryBlock in TensorBlock.h  ////////////

template <typename LhsTensorBlock, typename RhsTensorBlock>
class TensorCwiseRecyclingBlock {

  static const bool NoArgBlockAccess =
    internal::is_void<typename LhsTensorBlock::XprType>::value ||
    internal::is_void<typename RhsTensorBlock::XprType>::value;

public:

  typedef typename conditional<
      NoArgBlockAccess, 
      void,
      TensorCwiseRecyclingBlock<const typename LhsTensorBlock::XprType,
                                const typename RhsTensorBlock::XprType> 
  >::type XprType;

  typedef typename XprScalar<XprType>::type Scalar;

  TensorCwiseRecyclingBlock(
    const LhsTensorBlock& left_block, const RhsTensorBlock& right_block) : 
      m_left_block(left_block), m_right_block(right_block) { }

  TensorBlockKind kind() const { return internal::TensorBlockKind::kExpr; }

  XprType expr() const {
    return XprType(m_left_block.expr(), m_right_block.expr());
  }

  const Scalar* data() const { return NULL; }

  void cleanup() {
    m_left_block.cleanup();
    m_right_block.cleanup();
  }

private:

  LhsTensorBlock m_left_block;
  RhsTensorBlock m_right_block;

};

////////////// Modified from TensorCwiseBinaryOp in TensorExpr.h  //////////////

  template<typename LhsXprType, typename RhsXprType>
  struct traits<TensorCwiseRecyclingOp<LhsXprType, RhsXprType> >
  {
    // Result type copies LhsXprType, which provides data to be recycled
    typedef typename LhsXprType::Scalar Scalar;
    typedef traits<LhsXprType> XprTraits;
    typedef typename promote_storage_type<
        typename traits<LhsXprType>::StorageKind,
        typename traits<RhsXprType>::StorageKind>::ret StorageKind;
    typedef typename promote_index_type<
        typename traits<LhsXprType>::Index,
        typename traits<RhsXprType>::Index>::type Index;
    typedef typename LhsXprType::Nested LhsNested;
    typedef typename RhsXprType::Nested RhsNested;
    typedef typename remove_reference<LhsNested>::type _LhsNested;
    typedef typename remove_reference<RhsNested>::type _RhsNested;
    static const int NumDimensions = XprTraits::NumDimensions;
    static const int Layout = XprTraits::Layout;
    typedef typename TypeConversion<
      Scalar,
      typename conditional<
        Pointer_type_promotion<typename LhsXprType::Scalar, Scalar>::val,
        typename traits<LhsXprType>::PointerType,
        typename traits<RhsXprType>::PointerType
      >::type
    >::type PointerType;
    enum {
      Flags = 0
    };
  };
  
  template<typename LhsXprType, typename RhsXprType>
  struct eval<TensorCwiseRecyclingOp<LhsXprType, RhsXprType>, Eigen::Dense>
  {
    typedef const TensorCwiseRecyclingOp<LhsXprType, RhsXprType>& type;
  };
  
  template<typename LhsXprType, typename RhsXprType>
  struct nested<
    TensorCwiseRecyclingOp<LhsXprType, RhsXprType>, 
    1, 
    typename eval<TensorCwiseRecyclingOp<LhsXprType, RhsXprType>>::type
  >
  {
    typedef TensorCwiseRecyclingOp<LhsXprType, RhsXprType> type;
  };
  
  }  // end namespace internal
  
template<typename LhsXprType, typename RhsXprType>
class TensorCwiseRecyclingOp : 
  public TensorBase<TensorCwiseRecyclingOp<LhsXprType, RhsXprType>, ReadOnlyAccessors>
{

public:

  typedef typename Eigen::internal::traits<TensorCwiseRecyclingOp>::Scalar Scalar;
  typedef typename Eigen::NumTraits<Scalar>::Real RealScalar;
  typedef Scalar CoeffReturnType;
  typedef typename Eigen::internal::nested<TensorCwiseRecyclingOp>::type Nested;
  typedef typename Eigen::internal::traits<TensorCwiseRecyclingOp>::StorageKind StorageKind;
  typedef typename Eigen::internal::traits<TensorCwiseRecyclingOp>::Index Index;

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorCwiseRecyclingOp(
    const LhsXprType& lhs, const RhsXprType& rhs
  ) : m_lhs_xpr(lhs), m_rhs_xpr(rhs) {}

  /** \returns the nested expressions */
  EIGEN_DEVICE_FUNC
  const typename internal::remove_all<typename LhsXprType::Nested>::type&
  lhsExpression() const { return m_lhs_xpr; }

  EIGEN_DEVICE_FUNC
  const typename internal::remove_all<typename RhsXprType::Nested>::type&
  rhsExpression() const { return m_rhs_xpr; }

protected:

  LhsXprType m_lhs_xpr;
  RhsXprType m_rhs_xpr;

};

/////////// Modified from TensorCwiseBinaryOp in TensorEvaluator.h  ////////////

template<typename LeftArgType, typename RightArgType, typename Device>
struct TensorEvaluator<const TensorCwiseRecyclingOp<LeftArgType, RightArgType>, Device>
{
  typedef TensorCwiseRecyclingOp<LeftArgType, RightArgType> XprType;

  /* Note: Throughout this TensorEvaluator, we need to keep const in 
     TensorEvaluator<const LeftArgType, Device> to make sure the correct 
     specialization is found, which will evaluate LeftArgType if it is a 
     subexpression, etc. */

  enum {
    IsAligned         = int(TensorEvaluator<const LeftArgType, Device>::IsAligned) &
                        int(TensorEvaluator<const RightArgType, Device>::IsAligned),
    PacketAccess      = false, // packets use SIMD, not helpful for plain data access
    BlockAccess       = int(TensorEvaluator<const LeftArgType, Device>::BlockAccess) &
                        int(TensorEvaluator<const RightArgType, Device>::BlockAccess),
    PreferBlockAccess = int(TensorEvaluator<const LeftArgType, Device>::PreferBlockAccess) |
                        int(TensorEvaluator<const RightArgType, Device>::PreferBlockAccess),
    Layout            = TensorEvaluator<const LeftArgType, Device>::Layout,
    CoordAccess       = false,  // to be implemented
    RawAccess         = false
  };

  TensorEvaluator(const XprType& op, const Device& device)
    : m_device(device),
      m_leftImpl(op.lhsExpression(), device),
      m_rightImpl(op.rhsExpression(), device),
      m_leftSize(m_leftImpl.dimensions().TotalSize())
  {
    EIGEN_STATIC_ASSERT((static_cast<int>(TensorEvaluator<const LeftArgType, Device>::Layout) == static_cast<int>(TensorEvaluator<const RightArgType, Device>::Layout) || internal::traits<XprType>::NumDimensions <= 1), YOU_MADE_A_PROGRAMMING_MISTAKE);
  }

  typedef typename XprType::Index Index;
  typedef typename XprType::Scalar Scalar;
  typedef typename internal::traits<XprType>::Scalar CoeffReturnType;
  typedef typename TensorEvaluator<const RightArgType, Device>::Dimensions Dimensions;
  typedef StorageMemory<CoeffReturnType, Device> Storage;
  typedef typename Storage::Type EvaluatorPointerType;

  static const int NumDims = internal::array_size<
    typename TensorEvaluator<const RightArgType, Device>::Dimensions
  >::value;

  //===- Tensor block evaluation strategy (see TensorBlock.h) -------------===//
  typedef internal::TensorBlockDescriptor<NumDims, Index> TensorBlockDesc;
  typedef internal::TensorBlockScratchAllocator<Device> TensorBlockScratch;

  typedef typename TensorEvaluator<const LeftArgType, Device>::TensorBlock
      LeftTensorBlock;
  typedef typename TensorEvaluator<const RightArgType, Device>::TensorBlock
      RightTensorBlock;

  typedef internal::TensorCwiseRecyclingBlock<LeftTensorBlock, RightTensorBlock>
      TensorBlock;
  //===--------------------------------------------------------------------===//

  EIGEN_DEVICE_FUNC const Dimensions& dimensions() const
  {
    // Recycling rule uses rhs dimensions for output
    return m_rightImpl.dimensions();
  }

  EIGEN_STRONG_INLINE bool evalSubExprsIfNeeded(EvaluatorPointerType) {
    m_leftImpl.evalSubExprsIfNeeded(NULL);
    m_rightImpl.evalSubExprsIfNeeded(NULL);
    return true;
  }

#ifdef EIGEN_USE_THREADS
  template <typename EvalSubExprsCallback>
  EIGEN_STRONG_INLINE void evalSubExprsIfNeededAsync(
      EvaluatorPointerType, EvalSubExprsCallback done) {
    // TODO(ezhulenev): Evaluate two expression in parallel?
    m_leftImpl.evalSubExprsIfNeededAsync(nullptr, [this, done](bool) {
      m_rightImpl.evalSubExprsIfNeededAsync(nullptr,
                                            [done](bool) { done(true); });
    });
  }
#endif  // EIGEN_USE_THREADS

  EIGEN_STRONG_INLINE void cleanup() {
    m_leftImpl.cleanup();
    m_rightImpl.cleanup();
  }

  EIGEN_DEVICE_FUNC CoeffReturnType coeff(Index index) const
  {
    // recycle lhs data
    return m_leftImpl.coeff(index % m_leftSize);
  }

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorOpCost
  costPerCoeff(bool vectorized) const {
    return m_leftImpl.costPerCoeff(vectorized) +
           m_rightImpl.costPerCoeff(vectorized);
  }

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE
  internal::TensorBlockResourceRequirements getResourceRequirements() const {
    return internal::TensorBlockResourceRequirements::merge(
               m_leftImpl.getResourceRequirements(),
               m_rightImpl.getResourceRequirements());
  }

  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorBlock
  block(TensorBlockDesc& desc, TensorBlockScratch& scratch,
          bool /*root_of_expr_ast*/ = false) const {
    desc.DropDestinationBuffer();
    return TensorBlock(m_leftImpl.block(desc, scratch),
                         m_rightImpl.block(desc, scratch));
  }

  EIGEN_DEVICE_FUNC EvaluatorPointerType data() const { return NULL; }

  #ifdef EIGEN_USE_SYCL
  // binding placeholder accessors to a command group handler for SYCL
  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE void bind(cl::sycl::handler &cgh) const {
    m_leftImpl.bind(cgh);
    m_rightImpl.bind(cgh);
  }
  #endif
 private:
  const Device EIGEN_DEVICE_REF m_device;
  TensorEvaluator<const LeftArgType, Device> m_leftImpl;
  TensorEvaluator<const RightArgType, Device> m_rightImpl;
  std::size_t m_leftSize;
};

} // end namespace Eigen

/**
 * Construct a Tensor expression that builds a Tensor using a recycling rule.
 * 
 * @tparam SrcXprType An Eigen::Tensor or tensor expression type for the object
 *   that will contain data to recycle
 * @tparam DstXprType An Eigen::Tensor or tensor expression type for the object
 *   that will contain the size and dimensions to recycle to
 * 
 * @param src The tensor or tensor expression with data to reshape/resize via 
 *   a recycling rule
 * @param dst The tensor or tensor expression that will define the shape and 
 *   size of a recycling rule
 * 
 * @return A tensor expression that can be composed with other tensor 
 *  expressions (i.e., unary operators such as exponentiation, 
 *  binary operators such as addition and multiplication, etc.) or used to 
 *  define the contents of an Eigen::Tensor object.
 */
template<typename SrcXprType, typename DstXprType>
Eigen::TensorCwiseRecyclingOp<SrcXprType, DstXprType> recyclingTensor(
  const SrcXprType & src, const DstXprType & dst
) {
  return Eigen::TensorCwiseRecyclingOp<SrcXprType, DstXprType>(src, dst);
}

#endif
