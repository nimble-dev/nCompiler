#ifndef __TENSOR_UTILS
#define __TENSOR_UTILS

template<typename other>
long nDimTraits2_size(const other& x);

namespace Eigen {

  // Usage will be
  // nDimTraits<const Type, DefaultDevice>(obj, DefaultDevice()).dimensions();
  // This merges TensorAssignOp and TensorEvaluator< TensorAssignOp, DefaultDevice>
  template<typename XprType, typename Device>
  class nDimTraitsInternal2 {
  public:
    typedef typename TensorEvaluator<XprType, Device>::Dimensions Dimensions;
    typedef typename XprType::Index Index;

    EIGEN_DEVICE_FUNC nDimTraitsInternal2(const XprType& xpr, const Device& device) :
      m_xpr(xpr), // what happens in constructor of TensorAssignOp
      m_Impl(m_xpr, device) // what happens in ctr of TensorEvaluator
    {}


    EIGEN_DEVICE_FUNC const Dimensions& dimensions() const
    {
      return m_Impl.dimensions();
    }

    EIGEN_DEVICE_FUNC Index size() const
    {
      return Eigen::internal::array_prod(dimensions());
    }

  private:
    const typename internal::remove_all<typename XprType::Nested>::type& m_xpr;
    TensorEvaluator<XprType, Device> m_Impl;

  };

  template<typename XprType>
  class nDimTraits2 {
  public:
    typedef nDimTraitsInternal2<const XprType, DefaultDevice> Internal;
    typedef typename nDimTraitsInternal2<const XprType, DefaultDevice>::Dimensions Dimensions;
    static const std::size_t NumDimensions = internal::traits<XprType>::NumDimensions;

    EIGEN_DEVICE_FUNC nDimTraits2(const XprType& xpr) :
      m_Internal(xpr, Eigen::DefaultDevice())
    {}

    EIGEN_DEVICE_FUNC const Dimensions& dimensions() const
    {
      return m_Internal.dimensions();
    }

    EIGEN_DEVICE_FUNC Index size() const
    {
      return Eigen::internal::array_prod(m_Internal.dimensions());
    }

  private:
    Internal m_Internal;
  };

  template<>
  struct nDimTraits2<double> {
    static const std::size_t NumDimensions = 0;
  };
  template<>
  struct nDimTraits2<long> {
    static const  std::size_t NumDimensions = 0;
  };
  template<>
  struct nDimTraits2<int> {
    static const  std::size_t NumDimensions = 0;
  };
  template<>
  struct nDimTraits2<bool> {
    static const  std::size_t NumDimensions = 0;
  };


  template<typename Scalar_>
  auto Make_Length1_Tensor(const Scalar_ &x) -> Eigen::Tensor<Scalar_, 1> {
    Eigen::Tensor<Scalar_, 1> ans(1);
    ans[0] = x;
    return(ans);
  }

  template<typename XprType>
  struct as_1D_tensor_impl {
    // see binaryOpReshapeRHS in tensorOperations.h
    typedef typename Eigen::Tensor<typename XprType::Scalar, XprType::NumDimensions>::Index Index;
    typedef Eigen::array<Index, 1> DimArr; // simple examples show reshaping to a DimArr, but
    typedef Eigen::DSizes<Index, 1> DSizes; // for later chained ops such as concatenate, it must be a DSizes.
    typename XprType::Nested m_expr;
    typedef decltype(m_expr.reshape(DSizes())) returnType;
    //typedef const T constT;
    //typedef const decltype(x.reshape(DimArr())) returnType;
    as_1D_tensor_impl(const XprType &expr) : m_expr(expr) {}
    returnType go() const {
      const DSizes newDim( DimArr{{
            nDimTraits2_size(m_expr)
//            Eigen::internal::array_prod( nDimTraits< typename std::remove_reference<typename XprType::Nested>::type >::getEvaluator(m_expr).dimensions() )
//            Eigen::TensorRef<
//            Eigen::Tensor<typename T::Scalar, T::NumDimensions>
 //           >(x).dimensions().TotalSize()
          }});
      return m_expr.reshape(newDim);
    }
    template<typename S>
    returnType go(const S &size) const {
      const DSizes newDim( DimArr{{
            size
          }});
      return m_expr.reshape(newDim);
    }
  };

  // This is to allow creation of c(1, 2, 3) via as_1D_tensor(std::vector<double>{1, 2, 3});
  // In the past I have observed that TensorMap's are not fully endowed with all tensor capabilities,
  // so I am not sure if this will be fully general.  If not, we may need to create a new op
  // for this.
  template<typename Scalar_>
  struct as_1D_tensor_impl<std::vector<Scalar_> > {
    typedef typename Eigen::Tensor<Scalar_, 1>::Index Index;
    typedef Eigen::array<Index, 1> DimArr; // simple examples show reshaping to a DimArr, but
    typedef Eigen::DSizes<Index, 1> DSizes; // for later chained ops such as concatenate, it must be a DSizes.
    const std::vector<Scalar_> &m_values;
    typedef const Eigen::TensorMap< const Eigen::Tensor<const Scalar_, 1> > returnType;
    as_1D_tensor_impl(const std::vector<Scalar_> &values) : m_values(values) {}
    returnType go() const {
      returnType ans(&m_values[0], m_values.size());
      return(ans);
    }
    template<typename S>
    returnType go(const S& size) const {
      returnType ans(&m_values[0], size);
      return(ans);
    }
  };
  //We can't have a scalar T=double implementation
  //because then we'd return a local object that could into an op
  //that would refer to it but the local object would go out of scope.

  template<typename T>
  auto as_1D_tensor(const T& x) -> decltype(as_1D_tensor_impl<T>(x).go()) { //typename as_1D_tensor_impl<T>::returnType {
    return as_1D_tensor_impl<T>(x).go();
  }

  template<typename T, typename S>
  auto as_1D_tensor(const T& x, const S &size) -> decltype(as_1D_tensor_impl<T>(x).go(size)) { //typename as_1D_tensor_impl<T>::returnType {
    return as_1D_tensor_impl<T>(x).go(size);
  }


  template<typename XprType>
  struct as_2D_flat_tensor_impl {
    // see binaryOpReshapeRHS in tensorOperations.h
    typedef typename Eigen::Tensor<typename XprType::Scalar, XprType::NumDimensions>::Index Index;
    typedef Eigen::array<Index, 2> DimArr;
    typedef Eigen::DSizes<Index, 2> DSizes;
    typename XprType::Nested m_expr;
    typedef decltype(m_expr.reshape(DSizes())) returnType;
    as_2D_flat_tensor_impl(const XprType &expr) : m_expr(expr) {}
    //typedef const T constT;
    //typedef const decltype(x.reshape(DimArr())) returnType;
    returnType go() const {
      const DSizes newDim(DimArr{{1,
                                  nDimTraits2_size(m_expr)
//            Eigen::internal::array_prod( nDimTraits<typename std::remove_reference<typename XprType::Nested>::type >::getEvaluator(m_expr).dimensions() )
//            Eigen::TensorRef<
//            Eigen::Tensor<typename T::Scalar, T::NumDimensions>
 //           >(x).dimensions().TotalSize()
          }});
      return m_expr.reshape(newDim);
    }
  };

  //We can't have a scalar T=double implementation
  //because then we'd return a local object that could into an op
  //that would refer to it but the local object would go out of scope.

  template<typename T>
  auto as_2D_flat_tensor(const T& x) -> decltype(as_2D_flat_tensor_impl<T>(x).go()) { //typename as_1D_tensor_impl<T>::returnType {
    return as_2D_flat_tensor_impl<T>(x).go();
  }
}

template<typename other>
long nDimTraits2_size(const other& x) {
  return Eigen::nDimTraits2<const other>(x).size();
}

template<typename other>
typename Eigen::nDimTraits2<const other>::Dimensions nDimTraits2_dimensions(const other& x) {
  return Eigen::nDimTraits2<const other>(x).dimensions();
}


#endif // __TENSOR_UTILS
