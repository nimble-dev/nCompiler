#ifndef _NCOMPILER_INDEXING_OPS
#define _NCOMPILER_INDEXING_OPS

namespace Eigen {
  template<DenseIndex DimId, typename XprType, typename IV_XptrType>
    class TensorIndexVecOp;

  /*
    TensorIndexVecOp is modeled on TensorReverse.
    In TensorIndexVecOp, a single index is replaced by an operation.  This provides nested indexing, what in R would appear as x[ , f(1:5),  ], for example
  */
  // DenseIndex DimId is how an ID template arg is done in TensorChipping.h
  // IV_XprType is the expression type of the index vector (or operation)
  namespace internal {
    template<DenseIndex DimId, typename XprType, typename IV_XprType>
      struct traits<TensorIndexVecOp<DimId,
      XprType, IV_XprType> > : public traits<XprType>
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

    template<DenseIndex DimId, typename XprType, typename IV_XprType>
      struct eval<TensorIndexVecOp<DimId, XprType, IV_XprType>, Eigen::Dense>
      {
        typedef const TensorIndexVecOp<DimId, XprType, IV_XprType>& type;
      };

    template<DenseIndex DimId, typename XprType, typename IV_XprType>
      struct nested<TensorIndexVecOp<DimId, XprType, IV_XprType>, 1,
      typename eval<TensorIndexVecOp<DimId, XprType, IV_XprType> >::type>
      {
        typedef TensorIndexVecOp<DimId, XprType, IV_XprType> type;
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

  template<DenseIndex DimId, typename XprType, typename IV_XprType>
    class TensorIndexVecOp : public TensorBase<TensorIndexVecOp<DimId,
    XprType, IV_XprType>, WriteAccessors>
    {
    public:
      typedef typename Eigen::internal::traits<TensorIndexVecOp>::Scalar Scalar;
      typedef typename Eigen::NumTraits<Scalar>::Real RealScalar;
      typedef typename XprType::CoeffReturnType CoeffReturnType;
      typedef typename Eigen::internal::nested<TensorIndexVecOp>::type Nested;
      typedef typename Eigen::internal::traits<TensorIndexVecOp>::StorageKind
        StorageKind;
      typedef typename Eigen::internal::traits<TensorIndexVecOp>::Index Index;

      EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorIndexVecOp(
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
        EIGEN_STRONG_INLINE TensorIndexVecOp& operator = (const TensorIndexVecOp& other)
        {
          typedef TensorAssignOp<TensorIndexVecOp, const TensorIndexVecOp> Assign;
          Assign assign(*this, other);
          internal::TensorExecutor<const Assign, DefaultDevice>::run(assign, DefaultDevice());
          return *this;
        }

      template<typename OtherDerived>
        EIGEN_DEVICE_FUNC
        EIGEN_STRONG_INLINE TensorIndexVecOp& operator = (const OtherDerived& other)
        {
          typedef TensorAssignOp<TensorIndexVecOp, const OtherDerived> Assign;
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
  template<DenseIndex DimId, typename ArgType, typename IV_Type, typename Device>
    struct TensorEvaluator<const TensorIndexVecOp<DimId, ArgType, IV_Type>, Device>
    {
      typedef TensorIndexVecOp<DimId, ArgType, IV_Type> XprType;
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
        CoordAccess = false,  // to be implemented
        RawAccess = false
      };

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
              return(m_IV_impl.coeff(index));
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
            inputIndex += m_IV_impl.coeff(index);
          } else {
            idx = index / m_stride_DimId; // Now idx is index in the target dim
            inputIndex += m_IV_impl.coeff(idx)*m_stride_DimId; // Replace with the nested index value.
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
        double compute_cost = 0;
        double index_cost = m_IV_impl.costPerCoeff( false /* vectorized */); // My guess at how to do this.
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
    struct TensorEvaluator<TensorIndexVecOp<DimId, ArgType, IV_Type>, Device>
    : public TensorEvaluator<const TensorIndexVecOp<DimId, ArgType, IV_Type>,
    Device> {
    typedef TensorEvaluator<const TensorIndexVecOp<DimId, ArgType, IV_Type>,
      Device> Base;
    typedef TensorIndexVecOp<DimId, ArgType, IV_Type> XprType;
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


namespace nCompiler {

  // template meta-programming aid
  template<typename T>
    struct BaseType { typedef T type; };

  // partial specialization to get non-ref. type from template ref. params
  template<typename T>
    struct BaseType<T&> { typedef T type; };

  template<Eigen::DenseIndex DimId>
    struct IndexVec {
      // constructor specifies parameters of the operation
      IndexVec() {}

      template<typename T, typename IV>
        auto op(T& x, IV &iv) const -> const Eigen::TensorIndexVecOp<DimId, T, IV> {
        return Eigen::TensorIndexVecOp<DimId, T, IV>(x, iv);
      }

      template<typename T, typename IV>
        auto operator()(T&& x, IV &&iv) const -> decltype(op(x, iv)) { return op(x, iv); }

    };

  /**
   * Do not perform any subsetting
   */
  struct FullDim {
    template<typename T>
    auto op(T&& x) -> decltype(x) { return std::forward<T>(x); }

    template<typename T>
    auto operator()(T&& x) -> decltype(op(x)) { return op(x); }
  };

  /**
   * Implement an Eigen chip operation
   */
  struct DropDim {

    Eigen::Index m_dim, m_offset;

    // constructor specifies parameters of the operation
  DropDim(Eigen::Index dim, Eigen::Index offset) :
    m_dim(dim), m_offset(offset) { }

    template<typename T>
    auto op(T&& x) -> decltype(
                               // return type will be an Eigen operation
                               x.chip(m_offset, m_dim)
                               ) {
      return x.chip(m_offset, m_dim);
    }

    template<typename T>
    auto operator()(T&& x) -> decltype(op(x)) { return op(x); }

  };

  /**
   * Implement a slice on a single dimension of a tensor, yielding a subview
   * of the input
   */
  struct SubView {

    Eigen::Index m_dim, m_start, m_end;

    // constructor specifies parameters of the operation
  SubView(Eigen::Index dim, Eigen::Index start, Eigen::Index end) :
    m_dim(dim), m_start(start), m_end(end) { }

    template<typename T>
    auto op(T&& x) -> decltype(
                               // return type will be an Eigen operation
                               x.slice(
                                       Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>(),
                                       Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>()
                                       )
                               ) {
      // initialize slice offsets and extents
      Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions> offsets;
      Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions> extents;
      offsets.fill(0);
      extents.fill(0);
      // set the start of the subview in the specified dimension
      offsets[m_dim] = m_start;
      // get dimension information for the object being subsetted
      Eigen::TensorRef<
        Eigen::Tensor<typename BaseType<T>::type::Scalar,
        BaseType<T>::type::NumDimensions>
        > xref(x);
      auto dim = xref.dimensions();
      // subview fully spans all dimensions except the subsetted dim.
      for(Eigen::Index i = 0; i < BaseType<T>::type::NumDimensions; ++i) {
        extents[i] = i == m_dim ? m_end - m_start + 1 : dim[i];
      }
      // execute slice
      return x.slice(offsets, extents);
    }

    template<typename T>
    auto operator()(T&& x) -> decltype(op(x)) { return op(x); }

  };

  /**
   * Implement a slice on N dimensions of a tensor, yielding a subview
   * of the input
   */
  template<Eigen::Index N>
    struct SubViewN {

      // array elements assumed to be {dimension, start, end}
      typedef std::array<Eigen::Index, 3> SliceDetails;

      std::array<SliceDetails, N> m_config;

      // couldn't get this to compile with std::array<SliceDetails> as arg
      SubViewN(std::initializer_list<SliceDetails> config) {
        // couldn't seem to transfer objects using operator= or memcpy
        auto cfg = config.begin();
        auto mcfg = m_config.begin();
        auto cfgend = config.end();
        while(cfg != cfgend) {
          *(mcfg++) = *(cfg++);
        }
      }

      template<typename T>
      auto op(T&& x) -> decltype(
                                 // return type will be an Eigen operation
                                 x.slice(
                                         Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>(),
                                         Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions>()
                                         )
                                 ) {
        // initialize slice offsets and extents
        Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions> offsets;
        Eigen::array<Eigen::Index, BaseType<T>::type::NumDimensions> extents;
        offsets.fill(0);
        extents.fill(0);
        // get dimension information for the object being subsetted
        Eigen::TensorRef<
          Eigen::Tensor<typename BaseType<T>::type::Scalar,
          BaseType<T>::type::NumDimensions>
          > xref(x);
        auto dim = xref.dimensions();
        // initialize subview to fully span all dimensions
        for(Eigen::Index i = 0; i < BaseType<T>::type::NumDimensions; ++i) {
          extents[i] = dim[i];
        }
        // transfer slice parameters
        auto cfgend = m_config.end();
        for(auto cfg = m_config.begin(); cfg != cfgend; ++cfg) {
          offsets[(*cfg)[0]] = (*cfg)[1];
          // must modify extents twice in fn. b/c cfg might not be sorted
          extents[(*cfg)[0]] = (*cfg)[2] - (*cfg)[1] + 1;
        }
        // execute slice
        return x.slice(offsets, extents);
      }

      template<typename T>
      auto operator()(T&& x) -> decltype(op(x)) { return op(x); }

    };



}



#endif // _NCOMPILER_INDEXING_OPS
