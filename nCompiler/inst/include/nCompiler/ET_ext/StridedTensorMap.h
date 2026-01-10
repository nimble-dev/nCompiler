// Attempt to extend TensorMap to use strides.
// Copyright (C) 2019 Perry de Valpine

// Contents are copied and modified from Eigen TensorMap.h

// Begin Eigen license and copyright statement:

// This file is part of Eigen, a lightweight C++ template library
// for linear algebra.
//
// Copyright (C) 2014 Benoit Steiner <benoit.steiner.goog@gmail.com>
//
// This Source Code Form is subject to the terms of the Mozilla
// Public License v. 2.0. If a copy of the MPL was not distributed
// with this file, You can obtain one at http://mozilla.org/MPL/2.0/.

// End Eigen license and copyright statement.

// nimble is licensed under GPL.
// According to https://www.mozilla.org/en-US/MPL/2.0/combining-mpl-and-gpl/#unmodified-mpl-licensed-files-adding-an-lgpl-notice,
// this file is therefore licensed under both GPL and MPL.


#ifndef EIGEN_CXX11_TENSOR_STRIDED_TENSOR_MAP_H
#define EIGEN_CXX11_TENSOR_STRIDED_TENSOR_MAP_H

#include <unsupported/Eigen/CXX11/Tensor>
#include "StridedTensorMapInfo.h"

namespace Eigen {
  template<typename PlainObjectType, int Options_ = Unaligned, template <class> class MakePointer_ = MakePointer>
  class StridedTensorMap;
}

#undef USE_VARIADIC_TEMPLATES_IN_STRIDED_TENSOR_MAP
// #define DEBUG_STRIDED_TENSOR_MAP

namespace Eigen {

  // forward declaration moved to forward_declarations.h
  //  template<typename PlainObjectType, int Options_ = Unaligned, template <class> class MakePointer_ = MakePointer> class StridedTensorMap;

  namespace internal {

    template<typename PlainObjectType, int Options_, template <class> class MakePointer_>
    struct traits<StridedTensorMap<PlainObjectType, Options_, MakePointer_> >
      : public traits<PlainObjectType>
    {
      typedef traits<PlainObjectType> BaseTraits;
      typedef typename BaseTraits::Scalar Scalar;
      typedef typename BaseTraits::StorageKind StorageKind;
      typedef typename BaseTraits::Index Index;
      static const int NumDimensions = BaseTraits::NumDimensions;
      static const int Layout = BaseTraits::Layout;
      enum {
        Options = Options_,
        Flags = BaseTraits::Flags
      };
      template <class T> struct MakePointer {
        // Intermediate typedef to workaround MSVC issue.
        typedef MakePointer_<T> MakePointerT;
        typedef typename MakePointerT::Type Type;
      };
    };

    template<typename PlainObjectType, int Options, template <class> class MakePointer>
    struct eval<StridedTensorMap<PlainObjectType, Options, MakePointer>, Eigen::Dense>
    {
      typedef const StridedTensorMap<PlainObjectType, Options, MakePointer>& type;
    };

    template<typename PlainObjectType, int Options, template <class> class MakePointer>
    struct eval<const StridedTensorMap<PlainObjectType, Options, MakePointer>, Eigen::Dense>
    {
      typedef const StridedTensorMap<PlainObjectType, Options, MakePointer>& type;
    };

    template <typename PlainObjectType, int Options, template <class> class MakePointer>
    struct nested<StridedTensorMap<PlainObjectType, Options, MakePointer> >
    {
      typedef const StridedTensorMap<PlainObjectType, Options, MakePointer>& type;
    };

    template <typename PlainObjectType, int Options, template <class> class MakePointer>
    struct nested<const StridedTensorMap<PlainObjectType, Options, MakePointer> >
    {
      typedef const StridedTensorMap<PlainObjectType, Options, MakePointer>& type;
    };

  }// end namespace internal
  
  template<typename PlainObjectType, int Options_, template <class> class MakePointer_> class StridedTensorMap : public TensorBase<StridedTensorMap<PlainObjectType, Options_, MakePointer_> >
  {
  public:
    typedef StridedTensorMap<PlainObjectType, Options_, MakePointer_> Self;
    typedef typename PlainObjectType::Base Base;
    typedef typename Eigen::internal::nested<Self>::type Nested;
    typedef typename internal::traits<PlainObjectType>::StorageKind StorageKind;
    typedef typename internal::traits<PlainObjectType>::Index Index;
    typedef typename internal::traits<PlainObjectType>::Scalar Scalar;
    typedef typename NumTraits<Scalar>::Real RealScalar;
    typedef typename Base::CoeffReturnType CoeffReturnType;

    /*    typedef typename internal::conditional<
          bool(internal::is_lvalue<PlainObjectType>::value),
          Scalar *,
          const Scalar *>::type
          PointerType;*/
    typedef typename MakePointer_<Scalar>::Type PointerType;
    typedef PointerType PointerArgType;

    static const int Options = Options_;

    static const Index NumIndices = PlainObjectType::NumIndices; // StridedTensorMap: This is output number of dimensions (indices).
    typedef typename PlainObjectType::Dimensions Dimensions;

    enum {
      IsAligned = ((int(Options_)&Aligned)==Aligned),
      Layout = PlainObjectType::Layout,
      CoordAccess = true,
      RawAccess = true
    };

    // See Eigen::TensorMap for alternative constructor ideas that have been removed.
    // Constructors added for StridedTensorMap

    template<typename InputType>
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE StridedTensorMap(InputType &inputTensor) // default to mapping the full tensor
      : m_data(inputTensor.data())
    {
      createSubTensorInfo<InputType::NumIndices, NumIndices, Scalar>(inputTensor.dimensions(),
                                                                     m_dimensions, // sizes
                                                                     m_strides,
                                                                     m_startIndices,
                                                                     m_stopIndices);
#ifdef DEBUG_STRIDED_TENSOR_MAP
      std::cout<<"sizes\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_dimensions[i]<<" "; std::cout<<std::endl;
      std::cout<<"starts\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_startIndices[i]<<" "; std::cout<<std::endl;
      std::cout<<"stops\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_stopIndices[i]<<" "; std::cout<<std::endl;
      std::cout<<"strides\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_strides[i]<<" "; std::cout<<std::endl;
#endif
    }

    template<typename InputType>
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE StridedTensorMap(InputType &inputTensor,
                                                           const Eigen::array<b__, InputType::NumIndices> &ss)
      : m_data(inputTensor.data())
    {
      createSubTensorInfo<InputType::NumIndices, NumIndices, Scalar>(ss,
                                                                     inputTensor.dimensions(),
                                                                     m_dimensions, // sizes
                                                                     m_strides,
                                                                     m_startIndices,
                                                                     m_stopIndices);
#ifdef DEBUG_STRIDED_TENSOR_MAP
      std::cout<<"sizes\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_dimensions[i]<<" "; std::cout<<std::endl;
      std::cout<<"starts\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_startIndices[i]<<" "; std::cout<<std::endl;
      std::cout<<"stops\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_stopIndices[i]<<" "; std::cout<<std::endl;
      std::cout<<"strides\t"; for(size_t i = 0; i < NumIndices; ++i) std::cout<<m_strides[i]<<" "; std::cout<<std::endl;
#endif
    }

    template<typename ss_type, typename input_sizes_type>
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE StridedTensorMap(Scalar *data,
                                                           const input_sizes_type &input_sizes,
                                                           const ss_type &ss)
      : m_data(data)
    {
      createSubTensorInfoGeneral<ss_type, input_sizes_type, NumIndices, Scalar>(ss,
                                                                                input_sizes,
                                                                                m_dimensions, // sizes
                                                                                m_strides,
                                                                                m_startIndices,
                                                                                m_stopIndices);
    }
   
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Index rank() const { return m_dimensions.rank(); }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Index dimension(Index n) const { return m_dimensions[n]; }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Index stride(Index n) const { return m_strides[n]; } // Added for StridedTensorMap
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Index offset() const { return m_offset; } // Added for StridedTensorMap
    
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Dimensions& dimensions() const { return m_dimensions; }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Dimensions& strides() const { return m_strides; } // Added for StridedTensorMap
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Dimensions& startIndices() const { return m_startIndices; } // Added for StridedTensorMap
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Dimensions& stopIndices() const { return m_stopIndices; } // Added for StridedTensorMap
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Index size() const { return m_dimensions.TotalSize(); }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE PointerType data() { return m_data; }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const PointerType data() const { return m_data; }

    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()(const array<Index, NumIndices>& indices) const
    {
      // StridedTensorMap: TO-DO
      //      eigen_assert(checkIndexRange(indices));
      if (PlainObjectType::Options&RowMajor) {
        const Index index = m_dimensions.IndexOfRowMajor(indices);
        return m_data[index];
      } else {
        const Index index = m_dimensions.IndexOfColMajor(indices);
        return m_data[index];
      }
    }

    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()() const
    {
      EIGEN_STATIC_ASSERT(NumIndices == 0, YOU_MADE_A_PROGRAMMING_MISTAKE)
        return m_data[0];
    }

    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()(Index index) const
    {
      eigen_internal_assert(index >= 0 && index < size());
      return m_data[m_offset + m_strides[0] * index]; // Modified for StridedTensorMap.
    }

#if USE_VARIADIC_TEMPLATES_IN_STRIDED_TENSOR_MAP //EIGEN_HAS_VARIADIC_TEMPLATES
    // StridedTensorMap: TO-DO
    template<typename... IndexTypes> EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()(Index firstIndex, Index secondIndex, IndexTypes... otherIndices) const
    {
      std::cout<<"in variadic const case"<<std::endl;
      EIGEN_STATIC_ASSERT(sizeof...(otherIndices) + 2 == NumIndices, YOU_MADE_A_PROGRAMMING_MISTAKE)
        if (PlainObjectType::Options&RowMajor) {
          const Index index = m_dimensions.IndexOfRowMajor(array<Index, NumIndices>{{firstIndex, secondIndex, otherIndices...}});
          return m_data[index];
        } else {
          const Index index = m_dimensions.IndexOfColMajor(array<Index, NumIndices>{{firstIndex, secondIndex, otherIndices...}});
          return m_data[index];
        }
    }
#else
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()(Index i0, Index i1) const
    {
      if (PlainObjectType::Options&RowMajor) {
        // StridedTensorMap: TO-DO
        const Index index = i1 + i0 * m_dimensions[1];
        return m_data[index];
      } else {
        const Index index = m_startIndices[0] + i0*m_strides[0] + m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1])); // Modified for StridedTensorMap
        m_offset +  m_strides[0] * (i0 + i1 * m_strides[1]); // Modified for StridedTensorMap
        return m_data[index];
      }
    }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()(Index i0, Index i1, Index i2) const
    {
      std::cout<<"in const operator()"<<std::endl;
      if (PlainObjectType::Options&RowMajor) {
        // StridedTensorMap: TO-DO
        const Index index = i2 + m_dimensions[2] * (i1 + m_dimensions[1] * i0);
        return m_data[index];
      } else {
        std::cout<<"in column major"<<std::endl;
        const Index index = (m_startIndices[0] + i0*m_strides[0]) +
          m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1]) +
                            m_dimensions[1] * ( (i2 * m_strides[2] + m_startIndices[2]) ) ); // Modified for StridedTensorMap
        return m_data[index];
      }
    }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()(Index i0, Index i1, Index i2, Index i3) const
    {
      if (PlainObjectType::Options&RowMajor) {
        // StridedTensorMap: TO-DO
        const Index index = i3 + m_dimensions[3] * (i2 + m_dimensions[2] * (i1 + m_dimensions[1] * i0));
        return m_data[index];
      } else {
        const Index index = (m_startIndices[0] + i0*m_strides[0]) +
          m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1]) +
                            m_dimensions[1] * ( (i2 * m_strides[2] + m_startIndices[2]) +
                                                m_dimensions[2] * ( (i3 * m_strides[3] + m_startIndices[3]) ) ) ) ; // Modified for StridedTensorMap
        return m_data[index];
      }
    }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE const Scalar& operator()(Index i0, Index i1, Index i2, Index i3, Index i4) const
    {
      if (PlainObjectType::Options&RowMajor) {
        const Index index = i4 + m_dimensions[4] * (i3 + m_dimensions[3] * (i2 + m_dimensions[2] * (i1 + m_dimensions[1] * i0)));
        return m_data[index];
      } else {
        const Index index = (m_startIndices[0] + i0*m_strides[0]) +
          m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1]) +
                            m_dimensions[1] * ( (i2 * m_strides[2] + m_startIndices[2]) +
                                                m_dimensions[2] * ( (i3 * m_strides[3] + m_startIndices[3]) +
                                                                    m_dimensions[3] * ( ( i4 * m_strides[4] + m_startIndices[4] ) ) ) ) )  ; // Modified for StridedTensorMap
        return m_data[index];
      }
    }
#endif

    // Above are const operator() cases for access to indexed single elements.
    // Below are corresponding non-const operator() cases.
    
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()(const array<Index, NumIndices>& indices)
    {
      // StridedTensorMap: TO-DO
      //      eigen_assert(checkIndexRange(indices));
      if (PlainObjectType::Options&RowMajor) {
        const Index index = m_dimensions.IndexOfRowMajor(indices);
        return m_data[index];
      } else {
        const Index index = m_dimensions.IndexOfColMajor(indices);
        return m_data[index];
      }
    }

    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()()
    {
      EIGEN_STATIC_ASSERT(NumIndices == 0, YOU_MADE_A_PROGRAMMING_MISTAKE)
        return m_data[0];
    }

    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()(Index index)
    {
      eigen_internal_assert(index >= 0 && index < size());
      return m_data[m_offset + m_strides[0] * index]; // Modified for StridedTensorMap.
    }

#if USE_VARIADIC_TEMPLATES_IN_STRIDED_TENSOR_MAP //EIGEN_HAS_VARIADIC_TEMPLATES
    // StridedTensorMap: TO-DO
    template<typename... IndexTypes> EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()(Index firstIndex, Index secondIndex, IndexTypes... otherIndices)
    {
      std::cout<<"in variadic non-const case"<<std::endl;
      static_assert(sizeof...(otherIndices) + 2 == NumIndices || NumIndices == Dynamic, "Number of indices used to access a tensor coefficient must be equal to the rank of the tensor.");
      const std::size_t NumDims = sizeof...(otherIndices) + 2;
      if (PlainObjectType::Options&RowMajor) {
        const Index index = m_dimensions.IndexOfRowMajor(array<Index, NumDims>{{firstIndex, secondIndex, otherIndices...}});
        return m_data[index];
      } else {
        const Index index = m_dimensions.IndexOfColMajor(array<Index, NumDims>{{firstIndex, secondIndex, otherIndices...}});
        return m_data[index];
      }
    }
#else
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()(Index i0, Index i1)
    {
      if (PlainObjectType::Options&RowMajor) {
        // StridedTensorMap: TO-DO
        const Index index = i1 + i0 * m_dimensions[1];
        return m_data[index];
      } else {
        const Index index = m_startIndices[0] + i0*m_strides[0] + m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1])); // Modified for StridedTensorMap
        m_offset +  m_strides[0] * (i0 + i1 * m_strides[1]); // Modified for StridedTensorMap
        return m_data[index];
      }
    }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()(Index i0, Index i1, Index i2)
    {
      std::cout<<"in non-variadic non-const case"<<std::endl;
      if (PlainObjectType::Options&RowMajor) {
        // StridedTensorMap: TO-DO
        const Index index = i2 + m_dimensions[2] * (i1 + m_dimensions[1] * i0);
        return m_data[index];
      } else {
        const Index index = (m_startIndices[0] + i0*m_strides[0]) +
          m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1]) +
                            m_dimensions[1] * ( (i2 * m_strides[2] + m_startIndices[2]) ) ); // Modified for StridedTensorMap
        return m_data[index];
      }
    }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()(Index i0, Index i1, Index i2, Index i3)
    {
      if (PlainObjectType::Options&RowMajor) {
        // StridedTensorMap: TO-DO
        const Index index = i3 + m_dimensions[3] * (i2 + m_dimensions[2] * (i1 + m_dimensions[1] * i0));
        return m_data[index];
      } else {
        const Index index = (m_startIndices[0] + i0*m_strides[0]) +
          m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1]) +
                            m_dimensions[1] * ( (i2 * m_strides[2] + m_startIndices[2]) +
                                                m_dimensions[2] * ( (i3 * m_strides[3] + m_startIndices[3]) ) ) ) ; // Modified for StridedTensorMap
        return m_data[index];
      }
    }
    EIGEN_DEVICE_FUNC
    EIGEN_STRONG_INLINE Scalar& operator()(Index i0, Index i1, Index i2, Index i3, Index i4)
    {
      if (PlainObjectType::Options&RowMajor) {
        // StridedTensorMap: TO-DO
        const Index index = i4 + m_dimensions[4] * (i3 + m_dimensions[3] * (i2 + m_dimensions[2] * (i1 + m_dimensions[1] * i0)));
        return m_data[index];
      } else {
        const Index index = (m_startIndices[0] + i0*m_strides[0]) +
          m_dimensions[0]*( (i1*m_strides[1] + m_startIndices[1]) +
                            m_dimensions[1] * ( (i2 * m_strides[2] + m_startIndices[2]) +
                                                m_dimensions[2] * ( (i3 * m_strides[3] + m_startIndices[3]) +
                                                                    m_dimensions[3] * ( ( i4 * m_strides[4] + m_startIndices[4] ) ) ) ) )  ; // Modified for StridedTensorMap
        return m_data[index];
      }
    }
#endif

    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE Self& operator=(const Self& other)
    {
      typedef TensorAssignOp<Self, const Self> Assign;
      Assign assign(*this, other);
      internal::TensorExecutor<const Assign, DefaultDevice>::run(assign, DefaultDevice());
      return *this;
    }

    template<typename OtherDerived>
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE
    Self& operator=(const OtherDerived& other)
    {
      typedef TensorAssignOp<Self, const OtherDerived> Assign;
      Assign assign(*this, other);
      internal::TensorExecutor<const Assign, DefaultDevice>::run(assign, DefaultDevice());
      return *this;
    }

  private:
    typename MakePointer_<Scalar>::Type m_data;
    Dimensions m_dimensions;
    Dimensions m_strides; // Added for StridedTensorMap.  Could this be a simple array?
    Dimensions m_startIndices; // ditto 
    Dimensions m_stopIndices;  // ditto
    Index m_offset;
  };

  // TensorEvaluator cases are modified from TensorSlidingSlicingOp

  // Eval as rvalue
  template<typename PlainObjectType, int Options_, template <class> class MakePointer_, typename Device>
  struct TensorEvaluator<const StridedTensorMap<PlainObjectType, Options_, MakePointer_>, Device>
  {
    typedef StridedTensorMap<PlainObjectType, Options_, MakePointer_> XprType;
    typedef typename XprType::Dimensions Dimensions;
    typedef Dimensions Strides;
    static const int NumDims = internal::array_size<Strides>::value;
    typedef PlainObjectType ArgType; /* Defined for StridedTensorMap.  ArgType is a template parameter in StridingSlicingOp. Is this a correct replacement? */

    enum {
      // Alignment can't be guaranteed at compile time since it depends on the
      // slice offsets and sizes.
      IsAligned = false,
      PacketAccess = false,
      BlockAccess = false,
      PreferBlockAccess = TensorEvaluator<ArgType, Device>::PreferBlockAccess,
      Layout = TensorEvaluator<ArgType, Device>::Layout,
      RawAccess = false
    };

    //===- Tensor block evaluation strategy (see TensorBlock.h) -------------===//
    typedef internal::TensorBlockNotImplemented TensorBlock;
    //===--------------------------------------------------------------------===//

  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorEvaluator(const XprType& STM, const Device& device)
      : m_STM(STM),
        m_device(device),
        m_strides(m_STM.strides())
    {
      // Handle degenerate intervals by gracefully clamping and allowing m_dimensions to be zero
      DSizes<Index,NumDims> startIndicesClamped, stopIndicesClamped;
      for (size_t i = 0; i < internal::array_size<Dimensions>::value; ++i) {
        eigen_assert(m_strides[i] != 0 && "0 stride is invalid");
        if(m_strides[i]>0){
          //     // StridedTensorMap note: clamp ensures start and stop values between 0 and m_impl.dimensions()[i]
          startIndicesClamped[i] = clamp(m_STM.startIndices()[i], 0, m_STM.dimensions()[i]);
          stopIndicesClamped[i] = clamp(m_STM.stopIndices()[i], 0, m_STM.dimensions()[i]);
        }else{
          /* implies m_strides[i]<0 by assert */
          startIndicesClamped[i] = clamp(m_STM.startIndices()[i], -1, m_STM.dimensions()[i] - 1);
          stopIndicesClamped[i] = clamp(m_STM.stopIndices()[i], -1, m_STM.dimensions()[i] - 1);
        }
        m_startIndices[i] = startIndicesClamped[i]; // defines m_startIndices
      }
    
      const typename TensorEvaluator<ArgType, Device>::Dimensions& input_dims = m_STM.dimensions();
     
      // check for degenerate intervals and compute output tensor shape
      bool degenerate = false;;
      for(int i = 0; i < NumDims; i++){
        Index interval = stopIndicesClamped[i] - startIndicesClamped[i];
        // defines dimensions
        if(interval == 0 || ((interval<0) != (m_strides[i]<0))){
          m_dimensions[i] = 0;
          degenerate = true;
        }else{
          m_dimensions[i] = interval / m_strides[i]
            + (interval % m_strides[i] != 0 ? 1 : 0);
          eigen_assert(m_dimensions[i] >= 0);
        }
      }
      Strides output_dims = m_dimensions;
      // defines m_inputStrides, m_outputStrides, and m_offsets
      if (static_cast<int>(Layout) == static_cast<int>(ColMajor)) {
        m_inputStrides[0] = m_strides[0];
        m_offsets[0] = startIndicesClamped[0];
        Index previousDimProduct = 1;
        for (int i = 1; i < NumDims; ++i) {
          previousDimProduct *= input_dims[i-1];
          m_inputStrides[i] = previousDimProduct * m_strides[i];
          m_offsets[i] = startIndicesClamped[i] * previousDimProduct;
        }
       
        // Don't initialize m_fastOutputStrides[0] since it won't ever be accessed.
        m_outputStrides[0] = 1;
        for (int i = 1; i < NumDims; ++i) {
          m_outputStrides[i] = m_outputStrides[i-1] * output_dims[i-1];
          // NOTE: if tensor is degenerate, we send 1 to prevent TensorIntDivisor constructor crash
          m_fastOutputStrides[i] = internal::TensorIntDivisor<Index>(degenerate ? 1 : m_outputStrides[i]);
        }
      } else {
        m_inputStrides[NumDims-1] = m_strides[NumDims-1];
        m_offsets[NumDims-1] = startIndicesClamped[NumDims-1];
        Index previousDimProduct = 1;
        for (int i = NumDims - 2; i >= 0; --i) {
          previousDimProduct *= input_dims[i+1];
          m_inputStrides[i] = previousDimProduct * m_strides[i];
          m_offsets[i] = startIndicesClamped[i] * previousDimProduct;
        }
      
        m_outputStrides[NumDims-1] = 1;
        for (int i = NumDims - 2; i >= 0; --i) {
          m_outputStrides[i] = m_outputStrides[i+1] * output_dims[i+1];
          // NOTE: if tensor is degenerate, we send 1 to prevent TensorIntDivisor constructor crash
          m_fastOutputStrides[i] = internal::TensorIntDivisor<Index>(degenerate ? 1 : m_outputStrides[i]);
        }
      }
      m_block_total_size_max = numext::maxi(static_cast<std::size_t>(1),
                                            device.lastLevelCacheSize() /
                                            sizeof(Scalar));
    }
  
    typedef typename XprType::Index Index;
    typedef typename XprType::Scalar Scalar;
    typedef typename internal::remove_const<Scalar>::type ScalarNonConst;
    typedef typename XprType::CoeffReturnType CoeffReturnType;
    typedef typename PacketType<CoeffReturnType, Device>::type PacketReturnType;
    //typedef Strides Dimensions;
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE const Dimensions& dimensions() const { return m_dimensions; }
  
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE bool evalSubExprsIfNeeded(CoeffReturnType*) {
      return true;
    }
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE void cleanup() {
    }
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE CoeffReturnType coeff(Index index) const
    {
      return m_STM.data()[srcCoeff(index)];
    }
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorOpCost costPerCoeff(bool vectorized) const {
      //  return m_impl.costPerCoeff(vectorized) + TensorOpCost(0, 0, NumDims);
      return TensorOpCost(0, 0, NumDims); // How do we set this up for our case?
    }
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE Scalar* data() const {
      return NULL;
    }
  
  protected:
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE Index srcCoeff(Index index) const
    {
      Index inputIndex = 0;
      if (static_cast<int>(Layout) == static_cast<int>(ColMajor)) {
        for (int i = NumDims - 1; i >= 0; --i) {
          const Index idx = index / m_fastOutputStrides[i];
          inputIndex += idx * m_inputStrides[i] + m_offsets[i];
          index -= idx * m_outputStrides[i];
        }
      } else {
        for (int i = 0; i < NumDims; ++i) {
          const Index idx = index / m_fastOutputStrides[i];
          inputIndex += idx * m_inputStrides[i] + m_offsets[i];
          index -= idx * m_outputStrides[i];
        }
      }
      return inputIndex;
    }
  
    static EIGEN_STRONG_INLINE Index clamp(Index value, Index min, Index max) {
      return numext::maxi(min, numext::mini(max,value));
    }
  
    array<Index, NumDims> m_outputStrides;
    array<internal::TensorIntDivisor<Index>, NumDims> m_fastOutputStrides;
    array<Index, NumDims> m_inputStrides;
    //TensorEvaluator<ArgType, Device> m_impl;
    const XprType& m_STM; // const because this is rvalue version of TensorEvaluator.
    const Device& m_device;
    DSizes<Index, NumDims> m_startIndices; // clamped startIndices
    DSizes<Index, NumDims> m_dimensions;
    DSizes<Index, NumDims> m_offsets; // offset in a flattened shape
    const Strides m_strides;
    std::size_t m_block_total_size_max;
  };

template<int numInd>
struct IndexBlocks {
  typedef Eigen::array<b__, numInd> type;
};

template <typename ...P>
struct MakeIndexBlocksTypes {
  const static int nInd = sizeof...(P);
  typedef typename IndexBlocks<nInd>::type type;
};

template <typename ...P>
typename MakeIndexBlocksTypes<P...>::type MakeIndexBlocks(P ...p) {
  typedef typename MakeIndexBlocksTypes<P...>::type returnType;
  returnType ans = {{p...}};
  return ans;
  // return returnType({p...}); // does not work
}

// e.g. MakeStridedTensorMap<2>::make(myEigenTensor, MakeIndexBlocks(s(1, 2), s(), s(3)))
// or   MakeStridedTensorMap<2>::make(myEigenTensor, Eigen::array<b__, 3>({s(1, 2), s(), s(3)}))
template<int output_nInd>
struct MakeStridedTensorMap {
  template<typename EigenInputType>
   struct MakeOutputType {
     typedef typename EigenInputType::Scalar Scalar;
     typedef Tensor<Scalar, output_nInd> EigenOutputType;
     typedef StridedTensorMap< EigenOutputType > type;
   };
  template<typename EigenInputType, typename IndexBlocksType>
  static typename MakeOutputType<EigenInputType>::type make(EigenInputType &x, const IndexBlocksType &indexBlockArray) {
    return typename MakeOutputType<EigenInputType>::type(x, indexBlockArray);
  }
  template<typename EigenInputType>
  static typename MakeOutputType<EigenInputType>::type make(EigenInputType &x) {
    return typename MakeOutputType<EigenInputType>::type(x);
  }
};

// Eval as lvalue
template<typename PlainObjectType, int Options_, template <class> class MakePointer_, typename Device>
struct TensorEvaluator< StridedTensorMap<PlainObjectType, Options_, MakePointer_>, Device>
  : public TensorEvaluator<const StridedTensorMap<PlainObjectType, Options_, MakePointer_>, Device>
{
  typedef TensorEvaluator<const StridedTensorMap<PlainObjectType, Options_, MakePointer_>, Device> Base;
  typedef StridedTensorMap<PlainObjectType, Options_, MakePointer_> XprType;
  typedef typename XprType::Dimensions Dimensions;
  typedef Dimensions Strides;
  typedef PlainObjectType ArgType;
  static const int NumDims = internal::array_size<Strides>::value;
  
  enum {
    IsAligned = false,
    PacketAccess = false,
    BlockAccess = false,
    Layout = TensorEvaluator<ArgType, Device>::Layout,
    CoordAccess = TensorEvaluator<ArgType, Device>::CoordAccess,
    RawAccess = false
  };
  
  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorEvaluator(const XprType& STM, const Device& device)
    : Base(STM, device)
  { }
  
  typedef typename XprType::Index Index;
  typedef typename XprType::Scalar Scalar;
  typedef typename internal::remove_const<Scalar>::type ScalarNonConst;
  typedef typename XprType::CoeffReturnType CoeffReturnType;
  typedef typename PacketType<CoeffReturnType, Device>::type PacketReturnType;
  
  EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE CoeffReturnType& coeffRef(Index index)
  {
    return this->m_STM.data()[Base::srcCoeff(index)];
  }
};




} // end namespace Eigen

#endif // EIGEN_CXX11_TENSOR_TENSOR_MAP_H

