#ifndef _NCOMPILER_INDEXING_OPS
#define _NCOMPILER_INDEXING_OPS

#include <unsupported/Eigen/CXX11/Tensor>

namespace Eigen {
  template<DenseIndex DimId, typename XprType, typename IV_XptrType, bool Rindexing>
    class TensorIndexByVecOp;

  /*
    TensorIndexByVecOp is modeled on TensorReverse.
    In TensorIndexByVecOp, a single index is replaced by an operation.  This provides nested indexing, what in R would appear as x[ , f(1:5),  ], for example
  */
  // DenseIndex DimId is how an ID template arg is done in TensorChipping.h
  // IV_XprType is the expression type of the index vector (or operation)
  namespace internal {
    template<DenseIndex DimId, typename XprType, typename IV_XprType, bool Rindexing>
      struct traits<TensorIndexByVecOp<DimId,
      XprType, IV_XprType, Rindexing> > : public traits<XprType>
    {
      typedef typename XprType::Scalar Scalar;
      typedef traits<XprType> XprTraits;
      typedef typename XprTraits::StorageKind StorageKind;
      typedef typename XprTraits::Index Index;
      typedef typename XprType::Nested Nested;
      typedef typename remove_reference<Nested>::type _Nested;
      static const int NumDimensions = XprTraits::NumDimensions;
      static const int Layout = XprTraits::Layout;
    };

    template<DenseIndex DimId, typename XprType, typename IV_XprType, bool Rindexing>
      struct eval<TensorIndexByVecOp<DimId, XprType, IV_XprType, Rindexing>, Eigen::Dense>
      {
        typedef const TensorIndexByVecOp<DimId, XprType, IV_XprType, Rindexing>& type;
      };

    template<DenseIndex DimId, typename XprType, typename IV_XprType, bool Rindexing>
      struct nested<TensorIndexByVecOp<DimId, XprType, IV_XprType, Rindexing>, 1,
      typename eval<TensorIndexByVecOp<DimId, XprType, IV_XprType, Rindexing> >::type>
      {
        typedef TensorIndexByVecOp<DimId, XprType, IV_XprType, Rindexing> type;
      };

    // The IVDimensionId is copied and renamed from DimensionId in TensorChipping.h
    template <DenseIndex DimId>
      struct IVDimensionId
      {
        EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE IVDimensionId(DenseIndex dim) {
          eigen_assert(dim == DimId);
        }
        EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE DenseIndex actualDim() const {
          return DimId;
        }
      };
    // As copied from TensorChipping.h, there is a specialization to the case that the
    // id is a variable.  We do not use this here, so it is commented out.
    /*  template <> */
    /* struct IVDimensionId<Dynamic> */
    /* { */
    /*   EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE IVDimensionId(DenseIndex dim) : actual_dim(dim) { */
    /*     eigen_assert(dim >= 0); */
    /*   } */
    /*   EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE DenseIndex actualDim() const { */
    /*     return actual_dim; */
    /*   } */
    /*  private: */
    /*   const DenseIndex actual_dim; */
    /* }; */

  }  // end namespace internal

  template<DenseIndex DimId, typename XprType, typename IV_XprType, bool Rindexing=false>
    class TensorIndexByVecOp : public TensorBase<TensorIndexByVecOp<DimId,
    XprType, IV_XprType, Rindexing>, WriteAccessors>
    {
    public:
      typedef typename Eigen::internal::traits<TensorIndexByVecOp>::Scalar Scalar;
      typedef typename Eigen::NumTraits<Scalar>::Real RealScalar;
      typedef typename XprType::CoeffReturnType CoeffReturnType;
      typedef typename Eigen::internal::nested<TensorIndexByVecOp>::type Nested;
      typedef typename Eigen::internal::traits<TensorIndexByVecOp>::StorageKind
        StorageKind;
      typedef typename Eigen::internal::traits<TensorIndexByVecOp>::Index Index;

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorIndexByVecOp(
                                                             const XprType& expr, const IV_XprType& IV_expr) // dim handled like in TensorChipping.h
        : m_xpr(expr), m_IV_xpr(IV_expr), m_dim(DimId) { } // If we ever mimic TensorChipping in letting the dim be a variable, add a dim arg and set m_dim(dim)

      EIGEN_DEVICE_FUNC
        const Index dim() const { return m_dim.actualDim(); }

      EIGEN_DEVICE_FUNC
        const typename internal::remove_all<typename XprType::Nested>::type&
        expression() const { return m_xpr; }

      EIGEN_DEVICE_FUNC
        const typename internal::remove_all<typename IV_XprType::Nested>::type& // not sure if this mimicry is correct
        IV_expression() const {return m_IV_xpr;}

      EIGEN_DEVICE_FUNC
        EIGEN_STRONG_INLINE TensorIndexByVecOp& operator = (const TensorIndexByVecOp& other)
        {
          typedef TensorAssignOp<TensorIndexByVecOp, const TensorIndexByVecOp> Assign;
          Assign assign(*this, other);
          internal::TensorExecutor<const Assign, DefaultDevice>::run(assign, DefaultDevice());
          return *this;
        }

      template<typename OtherDerived>
        EIGEN_DEVICE_FUNC
        EIGEN_STRONG_INLINE TensorIndexByVecOp& operator = (const OtherDerived& other)
        {
          typedef TensorAssignOp<TensorIndexByVecOp, const OtherDerived> Assign;
          Assign assign(*this, other);
          internal::TensorExecutor<const Assign, DefaultDevice>::run(assign, DefaultDevice());
          return *this;
        }

    protected:
      typename XprType::Nested m_xpr;
      typename IV_XprType::Nested m_IV_xpr; // I am not sure if mimicing use of ::Nested is correct here.
      const internal::IVDimensionId<DimId> m_dim;
    };

  // Eval as rvalue
  template<DenseIndex DimId, typename ArgType, typename IV_Type, typename Device, bool Rindexing>
    struct TensorEvaluator<const TensorIndexByVecOp<DimId, ArgType, IV_Type, Rindexing>, Device>
    {
      typedef TensorIndexByVecOp<DimId, ArgType, IV_Type, Rindexing> XprType;
      typedef typename XprType::Index Index;
      // next line adapted from TensorChipping.h
      static const int NumDims = internal::array_size<typename TensorEvaluator<ArgType, Device>::Dimensions>::value;
      typedef DSizes<Index, NumDims> Dimensions;
      typedef typename XprType::Scalar Scalar;
      typedef typename XprType::CoeffReturnType CoeffReturnType;
      typedef typename PacketType<CoeffReturnType, Device>::type PacketReturnType;
      static const int PacketSize = internal::unpacket_traits<PacketReturnType>::size;

      enum {
        IsAligned = false,
        PacketAccess = TensorEvaluator<ArgType, Device>::PacketAccess,
        Layout = TensorEvaluator<ArgType, Device>::Layout,
        BlockAccess = false,
        PreferBlockAccess = TensorEvaluator<ArgType, Device>::PreferBlockAccess,
        CoordAccess = false,  // to be implemented
        RawAccess = false
      };

      //===- Tensor block evaluation strategy (see TensorBlock.h) -------------===//
      typedef internal::TensorBlockNotImplemented TensorBlock;
      //===--------------------------------------------------------------------===//

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorEvaluator(const XprType& op,
                                                            const Device& device)
        : m_impl(op.expression(), device), m_IV_impl(op.IV_expression(), device),
        m_dim(op.dim()), DimId_is_first(m_dim.actualDim()==0), DimId_is_last(m_dim.actualDim()==NumDims-1)
        {
          EIGEN_STATIC_ASSERT((NumDims > 0), YOU_MADE_A_PROGRAMMING_MISTAKE);

          // Compute strides
          m_dimensions = m_impl.dimensions();
          Index updated_dimension = m_IV_impl.dimensions().TotalSize();
          if (static_cast<int>(Layout) == static_cast<int>(ColMajor)) {
            m_stride_DimId = 1;
            for (int i = 1; i <= m_dim.actualDim(); ++i) {
              m_stride_DimId *= m_dimensions[i-1];
            }
            if(!DimId_is_last) {
              m_stride_DimId_plus_1_input = m_stride_DimId * m_dimensions[m_dim.actualDim()];
              m_stride_DimId_plus_1 = m_stride_DimId * updated_dimension;
            }
            m_dimensions[m_dim.actualDim()] = updated_dimension;
          } else {
           std::cout<<"Error: nCompiler never uses row-major tensors!"<<std::endl;
          }
        }

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE
        const Dimensions& dimensions() const { return m_dimensions; }

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE bool evalSubExprsIfNeeded(Scalar*) {
        m_impl.evalSubExprsIfNeeded(NULL);
        return true;
      }
      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE void cleanup() {
        m_impl.cleanup();
      }

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE Index processIndex(
                                                               Index index) const {
        eigen_assert(index < dimensions().TotalSize());
        Index inputIndex;
        Index idx;
        //    std::cout<<"entering processIndex for "<<index<<"\n"<<std::endl;

        if (static_cast<int>(Layout) == static_cast<int>(ColMajor)) {
          if(DimId_is_last) {
            // No need to worry about strides after the target dim
            if(DimId_is_first) {
              return(m_IV_impl.coeff(index)-Rindexing);
            }
            inputIndex = 0;
          } else {
            idx = index / m_stride_DimId_plus_1; // This is number of strides after target dim
            inputIndex = idx * m_stride_DimId_plus_1_input; // Move that many strides for input sizes,
            // which might be different because size of
            // target dim might be different.
            index -= idx * m_stride_DimId_plus_1; // Now index includes only target dim and lower
          }
          if(DimId_is_first) {
            inputIndex += m_IV_impl.coeff(index)-Rindexing;
          } else {
            idx = index / m_stride_DimId; // Now idx is index in the target dim
            inputIndex += (m_IV_impl.coeff(idx)-Rindexing)*m_stride_DimId; // Replace with the nested index value.
            index -= idx * m_stride_DimId; // Now index is index before the target dim
            inputIndex += index; // which gets added on unmodified
          }
        } else {
          std::cout<<"Error: nCompiler never uses row-major tensors!"<<std::endl;
          inputIndex = 0;
        }
        return inputIndex;
      }

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE CoeffReturnType coeff(
                                                                  Index index) const  {
        return m_impl.coeff(processIndex(index));
      }

      template<int LoadMode>
        EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE
        PacketReturnType packet(Index index) const
        {
          EIGEN_STATIC_ASSERT((PacketSize > 1), YOU_MADE_A_PROGRAMMING_MISTAKE)
            eigen_assert(index+PacketSize-1 < dimensions().TotalSize());

          // TODO(ndjaitly): write a better packing routine that uses
          // local structure.
          EIGEN_ALIGN_MAX typename internal::remove_const<CoeffReturnType>::type
            values[PacketSize];
          for (int i = 0; i < PacketSize; ++i) {
            values[i] = coeff(index+i);
          }
          PacketReturnType rslt = internal::pload<PacketReturnType>(values);
          return rslt;
        }

      /* I tried to adapt from what TensorReverse does for costPerCoeff  */
      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorOpCost costPerCoeff(bool vectorized) const {
// double compute_cost = NumDims * (2 * TensorOpCost::AddCost<Index>() +
//                                      2 * TensorOpCost::MulCost<Index>() +
//                                      TensorOpCost::DivCost<Index>());
//     for (int i = 0; i < NumDims; ++i) {
//       if (i == m_dim.actualDim()) {
//         compute_cost += 2 * TensorOpCost::AddCost<Index>();
//       }
//     }
//     return m_impl.costPerCoeff(vectorized) +
//            TensorOpCost(0, 0, compute_cost, false /* vectorized */, PacketSize);
// }
        double compute_cost = 0;
        double index_cost = m_IV_impl.costPerCoeff( false /* vectorized */); // My guess at how to do this.

 //       std::cout<<"In costPerCoeff"<<std::endl;
        if(DimId_is_last) {
          if(DimId_is_first) {
            compute_cost = index_cost;
          }
        } else {
          compute_cost += TensorOpCost::AddCost<Index>() +
            2 * TensorOpCost::MulCost<Index>() +
            TensorOpCost::DivCost<Index>();
        }
        if(DimId_is_first) {
          compute_cost += index_cost;
        } else {
          compute_cost += 3*TensorOpCost::AddCost<Index>() +
            2 * TensorOpCost::MulCost<Index>() +
            TensorOpCost::DivCost<Index>()+
            index_cost;
        }
        return m_impl.costPerCoeff(vectorized) +
          TensorOpCost(0, 0, compute_cost, false /* vectorized */, PacketSize);
      }

      EIGEN_DEVICE_FUNC Scalar* data() const { return NULL; }

    protected:
      Dimensions m_dimensions;
      array<Index, NumDims> m_strides;
      Index m_stride_DimId, m_stride_DimId_plus_1_input, m_stride_DimId_plus_1;
      TensorEvaluator<ArgType, Device> m_impl;
      TensorEvaluator<IV_Type, Device> m_IV_impl;
      const internal::IVDimensionId<DimId> m_dim;
      bool DimId_is_first, DimId_is_last;
    };


  // Eval as lvalue

  template <DenseIndex DimId, typename ArgType, typename IV_Type, typename Device>
    struct TensorEvaluator<TensorIndexByVecOp<DimId, ArgType, IV_Type>, Device>
    : public TensorEvaluator<const TensorIndexByVecOp<DimId, ArgType, IV_Type>,
    Device> {
    typedef TensorEvaluator<const TensorIndexByVecOp<DimId, ArgType, IV_Type>,
      Device> Base;
    typedef TensorIndexByVecOp<DimId, ArgType, IV_Type> XprType;
    typedef typename XprType::Index Index;
    static const int NumDims = internal::array_size<typename TensorEvaluator<ArgType, Device>::Dimensions>::value;
    typedef DSizes<Index, NumDims> Dimensions;

    enum {
      IsAligned = false,
      PacketAccess = TensorEvaluator<ArgType, Device>::PacketAccess,
      Layout = TensorEvaluator<ArgType, Device>::Layout,
      CoordAccess = false,  // to be implemented
      RawAccess = false
    };
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorEvaluator(const XprType& op,
                                                          const Device& device)
      : Base(op, device) {}

    typedef typename XprType::Scalar Scalar;
    typedef typename XprType::CoeffReturnType CoeffReturnType;
    typedef typename PacketType<CoeffReturnType, Device>::type PacketReturnType;
    static const int PacketSize = internal::unpacket_traits<PacketReturnType>::size;

    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE
      const Dimensions& dimensions() const { return this->m_dimensions; }

    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE Scalar& coeffRef(Index index) {
      return this->m_impl.coeffRef(this->processIndex(index));
    }

    template <int StoreMode> EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE
      void writePacket(Index index, const PacketReturnType& x) {
      EIGEN_STATIC_ASSERT((PacketSize > 1), YOU_MADE_A_PROGRAMMING_MISTAKE)
        eigen_assert(index+PacketSize-1 < dimensions().TotalSize());

      // This code is pilfered from TensorMorphing.h
      EIGEN_ALIGN_MAX CoeffReturnType values[PacketSize];
      internal::pstore<CoeffReturnType, PacketReturnType>(values, x);
      for (int i = 0; i < PacketSize; ++i) {
        this->coeffRef(index+i) = values[i];
      }
    }
  };
}  // end namespace Eigen

#define ISINGLE_(DIM, I, X) nCompiler::IndexByScalar<DIM>().op(I, X)
#define IVEC_(DIM, IVEC, X, R) nCompiler::IndexByVec<DIM, R>().op(IVEC, X) // R is bool to indicate whether to use R indexing (starting at 1)
#define ISEQS_(NUMSEQS, SEQINFO, X) nCompiler::IndexBySeqs<NUMSEQS>::go(SEQINFO, X)
#define SEQ_(DIM, START, END) {I_(DIM), I_(START), I_(END)}
#define SEQS_(...) {{__VA_ARGS__}}
#define I_(x) static_cast<Eigen::Index>(x)

namespace nCompiler {

  // template meta-programming aid
  template<typename T>
  struct BaseType { typedef T type; };

  // partial specialization to get non-ref. type from template ref. params
  template<typename T>
  struct BaseType<T&> { typedef T type; };

  // MakeTensorRef returns a TensorRef around an operator that allows
  // singleton access and assignment.  For access, follow by () of .coeff().
  // For assignment, follow by .coeffRef() = value.
  template<class T >
  auto MakeTensorRef(T &&x) ->   Eigen::TensorRef<
    Eigen::Tensor<typename BaseType<T>::type::Scalar,
                  BaseType<T>::type::NumDimensions>
    >
  {
    return Eigen::TensorRef<
      Eigen::Tensor<typename BaseType<T>::type::Scalar,
                    BaseType<T>::type::NumDimensions>
      >(std::forward<T>(x));
  }

  template<Eigen::DenseIndex DimId, bool Rindexing=false>
  struct IndexByVec {
    // constructor specifies parameters of the operation
    IndexByVec() {}
    // pass const-ness from x to returned Op.  This (indirectly) imitates what I see in TensorBase
    template<typename T, typename IV>
    Eigen::TensorIndexByVecOp<DimId, T, IV, Rindexing>
    op(const IV& iv, const T& x) const {
      // It does not work to return const Eigen::TensorIndexByVecOp<DimId, T, IV>
      // because this method might be chosen just due to x being a temporary (rvalue)
      // We might be able to do it with T&& combined with std::is_lvalue tools
      std::cout<<"got const version of IndexByVec"<<std::endl;
      return Eigen::TensorIndexByVecOp<DimId, T, IV, Rindexing>(x,
                                                                iv);
    }
    template<typename T, typename IV>
    Eigen::TensorIndexByVecOp<DimId, T, IV, Rindexing>
    op(const IV& iv, T& x) const {
      std::cout<<"got non-const version of IndexByVec"<<std::endl;
      return Eigen::TensorIndexByVecOp<DimId, T, IV, Rindexing>(x,
                                                                iv);
    }
    // template<typename T, typename IV>
    //   auto operator()(IV &&iv, T&& x) const -> decltype(op(iv, x)) { return op(iv, x); }
  };

  // /**
  //  * Do not perform any subsetting
  //  */
  // struct FullDim {
  //   template<typename T>
  //   auto op(T&& x) -> decltype(x) { return std::forward<T>(x); }
  //   template<typename T>
  //   auto operator()(T&& x) -> decltype(op(x)) { return op(x); }
  // };

  /**
   * Implement an Eigen chip operation
   */
  // This template was set up to try to use the x.template chip<DimId>(offset) form,
  // but it did not chain correctly.
  template<Eigen::DenseIndex DimId>
  struct IndexByScalar {
    IndexByScalar() {}
    template<typename T>
    auto op(Eigen::Index offset, T&& x) const -> decltype(
                               // return type will be an Eigen operation
                              x.template chip<DimId>(offset)
                            //         x.chip(m_offset, m_dim)
 ) {
      return x.template chip<DimId>(offset);
    }
  };

  /**
   * Implement a slice on N dimensions of a tensor, yielding a subview
   * of the input
   */
  template<int NumSourceDims, Eigen::Index NumSeqDims >
  struct SubViewN {
    // array elements assumed to be {dimension, start, end}
    typedef std::array<Eigen::Index, 3> SliceDetail;
    typedef std::array<SliceDetail, NumSeqDims> AllSliceDetails;
    typedef Eigen::array<Eigen::Index, NumSourceDims> SourceIndArr;
    SourceIndArr offsets, extents;
    SubViewN() {}
    template<typename T>
    auto op(const AllSliceDetails& config,
            T&& x) ->
      decltype(
               // return type will be an Eigen operation
               x.slice(
                       SourceIndArr(),
                       SourceIndArr()
                       ))
        {
          // initialize slice offsets and extents
          offsets.fill(0);
          extents.fill(0);
          // get dimension information for the object being subsetted
          // Eigen::TensorRef<
          //   Eigen::Tensor<typename BaseType<T>::type::Scalar,
          //                 NumSourceDims>
          // > xref(x);
          typedef typename std::remove_reference<T>::type cleanT;
          auto xref = nDimTraits<cleanT>::getEvaluator(std::forward<T>(x));
          auto dim = xref.dimensions();
          // initialize subview to fully span all dimensions
          for(Eigen::Index i = 0; i < BaseType<T>::type::NumDimensions; ++i) {
            extents[i] = dim[i];
          }
          // transfer slice parameters
          auto cfgend = config.end();
          for(auto cfg = config.begin(); cfg != cfgend; ++cfg) {
            offsets[(*cfg)[0]] = (*cfg)[1];
            // must modify extents twice in fn. b/c cfg might not be sorted
            extents[(*cfg)[0]] = (*cfg)[2] - (*cfg)[1] + 1;
          }
          // execute slice
          return x.slice(offsets, extents);
        }
    };

  // The purpose of this layer is extract NumDimensions from T and have that
  // be a template argument to the next layer in: SubViewN<...>::op
  template<Eigen::Index NumSeqDims>
  struct IndexBySeqs {
    typedef std::array<Eigen::Index, 3> SliceDetail;
    typedef std::array<SliceDetail, NumSeqDims> AllSliceDetails;
    IndexBySeqs() {};
    template<typename T>
    static auto go(const AllSliceDetails& config,
                      T &&x) -> decltype(x.slice(
                                          Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>(),
                                          Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>())
                                  )
      {
       return SubViewN<BaseType<T>::type::NumDimensions, NumSeqDims>().op(config, std::forward<T>(x));
      }
  };
}

#endif // _NCOMPILER_INDEXING_OPS
