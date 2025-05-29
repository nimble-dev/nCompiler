#ifndef _NCOMPILER_RECYCLYING_RULE
#define _NCOMPILER_RECYCLYING_RULE

#include <unsupported/Eigen/CXX11/Tensor>
#include <tuple>

namespace Eigen {

// forward declaration of an Eigen tensor operation for a recycling rule
template<typename LeftXprType, typename RightXprType> class TensorCwiseRecyclingOp;

namespace internal {

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
  ) : m_lhs_xpr(lhs), m_rhs_xpr(rhs) { }

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
    BlockAccess       = false,
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
  typedef internal::TensorBlockNotImplemented TensorBlock;
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

namespace nCompiler {

  /**
   * Provides a class with the member constant value equal to true if all of 
   * the int values vs... equal true.
   * 
   * The base template provides the default value of true when the parameter 
   * pack is empty.
   * 
   * Examples: 
   *  all_equal<1,2,3>::value;  // equals false
   *  all_equal<1,1,2>::value;  // equals false
   *  all_equal<1,1,1>::value;  // equals true
   *  all_equal<8>::value;      // equals true
   *  all_equal<>::value;       // equals true
   */
  template<int... vs>
  struct all_equal { static const bool value = true; };

  /**
   * Partial specialization that uses recursion to implement the features of 
   * the template struct all_equal<vs...> described above
   */
  template<int v1, int v2, int... vs>
  struct all_equal<v1, v2, vs...> { 
    static const bool value = (v1 == v2) && all_equal<v2, vs...>::value;
  };

  /**
   * Provides a class with the member constant value equal to true if all of 
   * the boolean values vs... equal true.
   * 
   * The base template provides the default value of true when the parameter 
   * pack is empty.
   * 
   * Examples: 
   *  all<std::true_type::type>::value;                          // equals true
   *  all<std::true_type::type, std::false_type::type>::value;   // equals false
   *  all<std::false_type::type>::value;                         // equals false
   *  all<std::false_type::type, std::false_type::type>::value;  // equals false
   */
  template<bool... vs>
  struct all { static const bool value = true; };

  /**
   * Partial specialization that uses recursion to implement the features of 
   * the template struct all<vs...> described above
   */
  template<bool v, bool... vs>
  struct all<v, vs...> { 
    static const bool value = v && all<vs...>::value;
  };

  /**
   * Provides a class with the member constant value equal to true if any of 
   * the boolean values vs... equal true.
   * 
   * The base template provides the default value of false when the parameter 
   * pack is empty.
   * 
   * Examples: 
   *  any<std::true_type::type>::value;                          // equals true
   *  any<std::true_type::type, std::false_type::type>::value;   // equals true
   *  any<std::false_type::type>::value;                         // equals false
   *  any<std::false_type::type, std::false_type::type>::value;  // equals false
   */
  template<bool... vs>
  struct any { static const bool value = false; };

  /**
   * Partial specialization that uses recursion to implement the features of 
   * the template struct any<vs...> described above
   */
  template<bool v, bool... vs>
  struct any<v, vs...> { 
    static const bool value = v || any<vs...>::value;
  };

  /**
   * Template methods to construct the type 
   * Outer<Args1::XprType, Args2::XprType, ...> from the template parameters
   * 
   */
  template<template<typename...> typename Outer,  typename... Args>
  struct unwrapXprTypes {
      typedef Outer<const typename Args::XprType...> type;
  };

  /**
   * Recursively find the total dimension size across the Tuple elements
   * 
   * @tparam i index of Tuple element being processed
   * @tparam Tuple type
   * @param m Container for sizes
   */
  template<std::size_t i, typename Tuple, typename Container>
  struct dimension_sizes_impl {
      static Container& run(Tuple & t, Container & m) {
        m[i-1] = static_cast<std::size_t>(
          std::get<i-1>(t).dimensions().TotalSize()
        );
        return dimension_sizes_impl<i-1, Tuple, Container>::run(t, m);
      }
  };

  /**
   * Partial template specialization to implement the end of the recursion
   * 
   * @tparam Tuple 
   * @param m Current set of results
   */
  template<typename Tuple, typename Container>
  struct dimension_sizes_impl<0, Tuple, Container> {
      static Container& run(Tuple & t, Container & m) { return m; }
  };

  /**
   * Wrapper to template programming technique to get the size of the Tuple t's 
   * elements
   * 
   * @tparam Tuple type
   * @param t Tuple to iterate over
   */
  template<typename Tuple>
  std::array<std::size_t, std::tuple_size<Tuple>::value> dimension_sizes(
    Tuple & t
  ) {
    typedef std::array<std::size_t, std::tuple_size<Tuple>::value> Container; 
    Container sizes;
    return dimension_sizes_impl<
      std::tuple_size<Tuple>::value, Tuple, Container
    >::run(t, sizes);
  }

  /**
   * Recursively find the maximum total dimension size across the Tuple elements
   * 
   * @tparam i index of Tuple element being processed
   * @tparam Tuple type
   * @param m Current maximum value
   */
  template<std::size_t i, typename Tuple>
  struct max_size_impl {
      static std::size_t run(Tuple & t, std::size_t m) {
        m = std::max(
          m, 
          static_cast<std::size_t>(std::get<i-1>(t).dimensions().TotalSize())
        );
        return max_size_impl<i-1, Tuple>::run(t, m);
      }
  };

  /**
   * Partial template specialization to implement the end of the recursion
   * 
   * @tparam Tuple 
   * @param m Current maximum value
   */
  template<typename Tuple>
  struct max_size_impl<0, Tuple> {
      static std::size_t run(Tuple & t, std::size_t m) { return m; }
  };

  /**
   * Wrapper to template programming technique to get the maximum size of the
   * Tuple t's elements
   * 
   * @tparam Tuple type
   * @param t Tuple to iterate over
   */
  template<typename Tuple>
  std::size_t max_dimension_size(Tuple & t) {
    return max_size_impl<std::tuple_size<Tuple>::value, Tuple>::run(t, 0);
  }

  /**
   * Recursively aggregate the value of the getResourceRequirements member 
   * function from each Tuple element
   * 
   * @tparam i index of Tuple element being processed
   * @tparam Tuple type
   */
  template<typename ResultType, std::size_t i, typename Tuple>
  struct resourceRequirements_impl {
      static ResultType run(ResultType & agg, Tuple & t) {
        agg = Eigen::internal::TensorBlockResourceRequirements::merge(
          agg, std::get<i-1>(t).getResourceRequirements()
        );
        return resourceRequirements_impl<ResultType, i-1, Tuple>::run(agg, t);
      }
  };

  /**
   * Partial template specialization to implement the end of the recursion
   * 
   * @tparam Tuple 
   */
  template<typename ResultType, typename Tuple>
  struct resourceRequirements_impl<ResultType, 0, Tuple> {
      static ResultType run(ResultType & agg, Tuple & t) { 
        return agg;
      }
  };

  /**
   * Wrapper to template programming technique to merge 
   * getResourceRequirements() member function value for all elements of Tuple t
   * 
   * @tparam Tuple type
   * @tparam ResultType output object type, for Eigen Tensors or Tensor 
   *  expressions, will often be a TensorBlockResourceRequirements object
   * @param t Tuple to iterate over
   */
  template<typename ResultType, typename Tuple>
  ResultType merged_resourceRequirements(Tuple & t) {
    ResultType agg;
    return resourceRequirements_impl<
      ResultType, std::tuple_size<Tuple>::value, Tuple
    >::run(agg, t);
  }

  /**
   * Recursively aggregate the value of the costPerCoeff member function from 
   *  each Tuple element
   * 
   * @tparam i index of Tuple element being processed
   * @tparam Tuple type
   */
  template<typename ResultType, std::size_t i, typename Tuple>
  struct costPerCoeff_impl {
      static ResultType run(ResultType & agg, Tuple & t, bool vectorized) {
        return agg + 
          std::get<i-1>(t).costPerCoeff(vectorized) +
          costPerCoeff_impl<ResultType, i-1, Tuple>::run(agg, t, vectorized);
      }
  };

  /**
   * Partial template specialization to implement the end of the recursion
   * 
   * @tparam Tuple 
   */
  template<typename ResultType, typename Tuple>
  struct costPerCoeff_impl<ResultType, 0, Tuple> {
      static ResultType run(ResultType & agg, Tuple & t, bool vectorized) { 
        return agg;
      }
  };

  /**
   * Wrapper to template programming technique to sum costPerCoeff() member 
   * function value for all elements of Tuple t
   * 
   * @tparam Tuple type
   * @tparam ResultType output object type, for Eigen Tensors or Tensor 
   *  expressions, will often be a TensorOpCost object
   * @param t Tuple to iterate over
   */
  template<typename ResultType, typename Tuple>
  ResultType total_costPerCoeff(Tuple & t, bool vectorized) {
    ResultType agg;
    return costPerCoeff_impl<
      ResultType, std::tuple_size<Tuple>::value, Tuple
    >::run(agg, t, vectorized);
  }

  /**
   * Recursively call the evalSubExprsIfNeeded member function of each Tuple 
   * element
   * 
   * @tparam i index of Tuple element being processed
   * @tparam Tuple type
   */
  template<std::size_t i, typename Tuple>
  struct evalSubExprs_impl {
      static void run(Tuple & t) {
          std::get<i-1>(t).evalSubExprsIfNeeded(NULL);
          evalSubExprs_impl<i-1, Tuple>::run(t);
      }
  };

  /**
   * Partial template specialization to implement the end of the recursion
   * 
   * @tparam Tuple 
   */
  template<typename Tuple>
  struct evalSubExprs_impl<0, Tuple> {
      static void run(Tuple & t) { }
  };

  /**
   * Wrapper to template programming technique to call evalSubExprsIfNeeded
   * member for all elements of Tuple t
   * 
   * @tparam Tuple type
   * @param t Tuple to iterate over
   */
  template<typename Tuple>
  void call_evalSubExprsIfNeeded(Tuple & t) {
    evalSubExprs_impl<std::tuple_size<Tuple>::value, Tuple>::run(t);
  }

  /**
   * Recursively call the cleanup member function of each Tuple element
   * 
   * @tparam i index of Tuple element being processed
   * @tparam Tuple type
   */
  template<std::size_t i, typename Tuple>
  struct cleanup_impl {
      static void run(Tuple & t) {
          std::get<i-1>(t).cleanup();
          cleanup_impl<i-1, Tuple>::run(t);
      }
  };

  /**
   * Partial template specialization to implement the end of the recursion
   * 
   * @tparam Tuple 
   */
  template<typename Tuple>
  struct cleanup_impl<0, Tuple> {
      static void run(Tuple & t) { }
  };

  /**
   * Wrapper to template programming technique to call cleanup member for all
   * elements of Tuple t
   * 
   * @tparam Tuple type
   * @param t Tuple to iterate over, calling cleanup member for each element
   */
  template<typename Tuple>
  void call_cleanup(Tuple & t) {
      cleanup_impl<std::tuple_size<Tuple>::value, Tuple>::run(t);
  }

  namespace tupleGeneration {
      //modified from source: https://stackoverflow.com/questions/687490/how-do-i-expand-a-tuple-into-variadic-template-functions-arguments

      // ------------- UTILITY---------------

      /**
       * Templated class useful for template metaprogramming.  Useful for 
       * passing along a compile-time sequence of int's (i.e., the template 
       * parameters) to other templated structs, functions, etc.
       * 
       * @tparam  
       */
      template<int...> struct index_tuple{}; 

      /**
       * Base case for templated recursion
       */
      template<int I, typename IndexTuple, typename... Types> 
      struct make_indexes_impl; 

      /**
       * Template metaprogramming struct to recursively build the member typedef
       * "type" with value index_tuple<0,1,2,...>.  Gets called since it 
       * represents a partial specialization of the base case, since the 
       * template parameters T and Types... are interpreted as "...Types" in 
       * the base case for the templated recursion.
       */
      template<int I, int... Indexes, typename T, typename ... Types> 
      struct make_indexes_impl<I, index_tuple<Indexes...>, T, Types...> 
      { 
          typedef typename make_indexes_impl<
              I + 1, index_tuple<Indexes..., I>, Types...
          >::type type;
      }; 

      /**
       * Template metaprogramming struct to create the final struct, when the 
       * make_indexes_impl Types parameter pack is empty
       */
      template<int I, int... Indexes> 
      struct make_indexes_impl<I, index_tuple<Indexes...> > 
      { 
          typedef index_tuple<Indexes...> type; 
      }; 

      /**
       * Templated struct with member typedef "type" whose value is 
       * index_tuple<0, 1, 2, ...>, with one integer for each Type in the 
       * parameter pack Types...
       * 
       * The make_indexes class inherits from make_indexes_impl, instantiated 
       * with an empty index_tuple<> template specialization, which will be 
       * recursively built up with indices via template metaprogramming 
       * techniques on the template class make_indexes_impl and its partial 
       * specializations
       * 
       * @tparam Types list of types that will be enumerated
       */
      template<typename ... Types> 
      struct make_indexes : make_indexes_impl<0, index_tuple<>, Types...> 
      {}; 

      // ----------UNPACK TUPLE AND APPLY TO FUNCTION ---------

      template<
        typename ReturnType, typename Functor, typename Index, typename Sizes,
        class... Args, int... Indexes 
      > 
      ReturnType functor_tuple_helper(
          index_tuple< Indexes... >, const std::tuple<Args...>& t, 
          const Functor & func, Index index, const Sizes & sizes
      ) { 
        return func(
          std::get<Indexes>(t).coeff(index % sizes[Indexes])...
        );
      } 

      /**
       * Use the argument t to create a std::tuple of TensorEvaluator objects
       * for each tuple element
       * 
       * @param func function to call using the source tuple's entries
       * @param t source tuple with data to pass to func with recycling rule
       * @param index the index of t's data to send to func
       * @param sizes container (i.e., std::array) with the length of the data 
       *  in each tuple's element, to help implement the recycling rule
       */
      template<
        typename ReturnType, typename Functor, typename Index, typename Sizes, 
        class... Args
      > 
      ReturnType recycling_call_with_tuple(
        const Functor & func, const std::tuple<Args...>& t, Index index,
        const Sizes & sizes
      ) {
          return functor_tuple_helper<ReturnType>(
              /* trick to pass Indexes... type information to generate_helper. 
                generate_helper does not use the actual object created, which is 
                clear since this represents an unnamed argument in the 
                definition for generate_helper. */
              typename make_indexes<Args...>::type(),
              // pass argument t to generate_helper as appropriate r/lvalue type
              std::forward<const std::tuple<Args...>>(t),
              // additional arguments
              func, index, sizes
          );
      }

      template<class... Args, int... Indexes > 
      std::tuple<Args...> generate_helper(
          index_tuple< Indexes... >, const std::tuple<Args...>& t
      ) { 
          return std::tuple<Args...>( 
              // pack expansion acts as a macro creating sequential get<i> calls
              std::forward<Args>(std::get<Indexes>(t).expr())... 
          );
      } 

      /**
       * Use the argument t to create a std::tuple<Args...> with entries
       * std::tuple<Args...>(
       *    std::get<0>(t).expr(),
       *    std::get<1>(t).expr(),
       *    std::get<2>(t).expr(),
       *    ...
       *    std::get<N-1>(t).expr()
       * )
       * 
       * @param t source tuple, used to generate return object
       */
      template<class ... Args> 
      std::tuple<Args...> generate_via_expr(const std::tuple<Args...>& t)
      {
          return generate_helper(
              /* trick to pass Indexes... type information to generate_helper. 
                generate_helper does not use the actual object created, which is 
                clear since this represents an unnamed argument in the 
                definition for generate_helper. */
              typename make_indexes<Args...>::type(),
              // pass argument t to generate_helper as appropriate r/lvalue type
              std::forward<const std::tuple<Args...>>(t)
          );
      }

      template<typename Device, class... Args, int... Indexes > 
      std::tuple<
        Eigen::TensorEvaluator<const Args, Device>...
      > evaluator_tuple_helper(
          index_tuple< Indexes... >, const std::tuple<Args...>& t, 
          const Device& device
      ) { 
        return std::tuple<Eigen::TensorEvaluator<const Args, Device>...> (
          // pack expansion acts as a macro creating sequential get<i> calls
          Eigen::TensorEvaluator<const Args, Device>(
            std::forward<const Args>(std::get<Indexes>(t)), device
          )... 
        );
      } 

      /**
       * Use the argument t to create a std::tuple of TensorEvaluator objects
       * for each tuple element
       * 
       * @param t source tuple, used to generate return object
       */
      template<typename Device, class... Args> 
      std::tuple<
        Eigen::TensorEvaluator<const Args, Device>...
      > generate_evaluator_tuple(
        const std::tuple<Args...>& t, const Device& device
      ) {
          return evaluator_tuple_helper(
              /* trick to pass Indexes... type information to generate_helper. 
                generate_helper does not use the actual object created, which is 
                clear since this represents an unnamed argument in the 
                definition for generate_helper. */
              typename make_indexes<Args...>::type(),
              // pass argument t to generate_helper as appropriate r/lvalue type
              std::forward<const std::tuple<Args...>>(t),
              // additional arguments
              device
          );
      }

      template<
        typename Ret, typename Desc, typename Scratch, class... Args, 
        int...Indexes 
      > 
      Ret generate_block_helper(
          index_tuple< Indexes... >, const std::tuple<Args...>& t, Desc &desc,
          Scratch &scratch
      ) { 
          return Ret( 
              // pack expansion acts as a macro creating sequential get<i> calls
              std::forward<Args>(std::get<Indexes>(t).block(desc, scratch))... 
          );
      } 

      /**
       * Use the argument t to create a Ret from the pack expansion constructor 
       * Ret(
       *    std::get<0>(t).block(desc, scratch),
       *    std::get<1>(t).block(desc, scratch),
       *    std::get<2>(t).block(desc, scratch),
       *    ...
       *    std::get<0>(N-1).block(desc, scratch)
       * )
       * 
       * @param t source tuple, used to generate return object
       */
      template<typename Ret, typename Desc, typename Scratch, class ... Args> 
      Ret generate_via_block(
        const std::tuple<Args...>& t, Desc &desc, Scratch &scratch
      ) {
          return generate_block_helper<Ret, Desc, Scratch>(
              /* trick to pass Indexes... type information to generate_helper. 
                generate_helper does not use the actual object created, which is 
                clear since this represents an unnamed argument in the 
                definition for generate_helper. */
              typename make_indexes<Args...>::type(),
              // pass argument t to generate_helper as appropriate r/lvalue type
              std::forward<const std::tuple<Args...>>(t),
              // pass additional arguments
              desc, scratch
          );
      }
  };

};

namespace Eigen {

  // forward declaration of an Eigen tensor operation for a recycling functor rule
  template<typename Functor, typename LeadXprType, typename... XprTypes> 
  class TensorCwiseRecyclingFunctorOp;
  
  namespace internal {
  
  ////////////// Modified from TensorCwiseBinaryOp in TensorExpr.h  //////////////

  /**
   * Note: TensorCwiseRecyclingFunctorOp inherits its scalar type, dimension, 
   * etc. from LeadXprType
   * 
   * @tparam Functor A callable type that will be the functor
   * @tparam LeadXprType The type that the expression will copy key types from
   * @tparam XprTypes 
   */
  template<typename Functor, typename LeadXprType, typename... XprTypes>
  struct traits<TensorCwiseRecyclingFunctorOp<Functor, LeadXprType, XprTypes...> >
  {
    // Result type copies LeadXprType, which provides data to be operated on
    typedef typename LeadXprType::Scalar Scalar;
    typedef traits<LeadXprType> XprTraits;
    typedef typename traits<LeadXprType>::StorageKind StorageKind;
    typedef typename traits<LeadXprType>::Index Index;
    typedef std::tuple<
      typename LeadXprType::Nested, 
      typename XprTypes::Nested...
    > Nested;
    typedef std::tuple<
      typename remove_reference<typename LeadXprType::Nested>::type, 
      typename remove_reference<typename XprTypes::Nested>::type...
    > _Nested;
    static const int NumDimensions = 1; // always materializes as a vector
    static const int Layout = XprTraits::Layout;
    typedef typename TypeConversion<
      Scalar,
      typename traits<LeadXprType>::PointerType
    >::type PointerType;
    enum {
      Flags = 0
    };
  };
  
  template<typename Functor, typename LeadXprType, typename... XprTypes>
  struct eval<TensorCwiseRecyclingFunctorOp<Functor, LeadXprType, XprTypes...>, 
              Eigen::Dense>
  {
    typedef const TensorCwiseRecyclingFunctorOp<
      Functor, LeadXprType, XprTypes...
    >& type;
  };
  
  template<typename Functor, typename LeadXprType, typename... XprTypes>
  struct nested<
  TensorCwiseRecyclingFunctorOp<Functor, LeadXprType, XprTypes...>, 
    1, 
    typename eval<
      TensorCwiseRecyclingFunctorOp<Functor, LeadXprType, XprTypes...>
    >::type
  >
  {
    typedef TensorCwiseRecyclingFunctorOp<
      Functor, LeadXprType, XprTypes...
    > type;
  };
  
  }  // end namespace internal
  
  template<typename Functor, typename LeadXprType, typename... XprTypes>
  class TensorCwiseRecyclingFunctorOp : 
    public TensorBase<
      TensorCwiseRecyclingFunctorOp<Functor, LeadXprType, XprTypes...>, 
      ReadOnlyAccessors
    >
  {
  
  public:
  
    typedef typename Eigen::internal::traits<
      TensorCwiseRecyclingFunctorOp
    >::Scalar Scalar;
    typedef typename Eigen::NumTraits<Scalar>::Real RealScalar;
    typedef Scalar CoeffReturnType;
    typedef typename Eigen::internal::nested<
      TensorCwiseRecyclingFunctorOp
    >::type Nested;
    typedef typename Eigen::internal::traits<
      TensorCwiseRecyclingFunctorOp
    >::StorageKind StorageKind;
    typedef typename Eigen::internal::traits<
      TensorCwiseRecyclingFunctorOp
    >::Index Index;
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorCwiseRecyclingFunctorOp(
      const Functor & func, const LeadXprType & leadXpr, const XprTypes&... xprs
    ) : m_func(func), m_xpr(leadXpr, xprs...) { }

    EIGEN_DEVICE_FUNC
    const Functor& functor() const { return m_func; }
  
    /** \returns the nested expressions */
    EIGEN_DEVICE_FUNC
    const std::tuple<
      typename internal::remove_all<typename LeadXprType::Nested>::type,
      typename internal::remove_all<typename XprTypes::Nested>::type...
    >& expression() const {
      return m_xpr;
    }
  
  protected:
  
    Functor & m_func;
    std::tuple<LeadXprType, XprTypes...> m_xpr;
  
  };
  
  /////////// Modified from TensorCwiseBinaryOp in TensorEvaluator.h  ////////////
  
  template<
    typename Functor, typename LeadXprType, typename Device, 
    typename... XprTypes
  >
  struct TensorEvaluator<
    const TensorCwiseRecyclingFunctorOp<Functor, LeadXprType, XprTypes...>, 
    Device
  >
  {

    typedef TensorCwiseRecyclingFunctorOp<
      Functor, LeadXprType, XprTypes...
    > XprType;
  
    /* Note: Throughout this TensorEvaluator, we need to keep const in 
       TensorEvaluator<const LeadXprType, Device> to make sure the correct 
       specialization is found, which will evaluate LeadXprType if it is a 
       subexpression, etc. */
  
    enum {
      IsAligned         = nCompiler::all<
          int(TensorEvaluator<const LeadXprType, Device>::IsAligned),
          int(TensorEvaluator<const XprTypes, Device>::IsAligned)...
        >::value,
      PacketAccess      = false, // packets use SIMD, not helpful for plain data access
      BlockAccess       = false,
      PreferBlockAccess = nCompiler::any<
        int(TensorEvaluator<const LeadXprType, Device>::PreferBlockAccess),
        int(TensorEvaluator<const XprTypes, Device>::PreferBlockAccess)...
      >::value,
      Layout            = TensorEvaluator<const LeadXprType, Device>::Layout,
      CoordAccess       = false,  // to be implemented
      RawAccess         = false
    };
  
    TensorEvaluator(const XprType& op, const Device& device)
      : m_device(device),
        m_xpr_impl(
          nCompiler::tupleGeneration::generate_evaluator_tuple(
            op.expression(), device
          )
        ),
        m_func(op.functor()),
        m_xpr_sizes(nCompiler::dimension_sizes(m_xpr_impl)),
        m_dimensions(nCompiler::max_dimension_size(m_xpr_impl))
        
    {
      EIGEN_STATIC_ASSERT(
        (
          nCompiler::all_equal<
            static_cast<int>(TensorEvaluator<const LeadXprType, Device>::Layout),
            static_cast<int>(TensorEvaluator<const XprTypes, Device>::Layout)...
          >::value
          || 
          // implement requirement to always materialize as a vector
          internal::traits<XprType>::NumDimensions != 1
        ),
        YOU_MADE_A_PROGRAMMING_MISTAKE
      );
    }
  
    typedef typename XprType::Index Index;
    typedef typename XprType::Scalar Scalar;
    typedef typename internal::traits<XprType>::Scalar CoeffReturnType;
    static const int NumDims = 1; // always materialize as a vector
    typedef DSizes<Index, NumDims> Dimensions;
    typedef StorageMemory<CoeffReturnType, Device> Storage;
    typedef typename Storage::Type EvaluatorPointerType;
  
    //===- Tensor block evaluation strategy (see TensorBlock.h) -------------===//
  typedef internal::TensorBlockNotImplemented TensorBlock;
  //===--------------------------------------------------------------------===//
  
    EIGEN_DEVICE_FUNC const Dimensions& dimensions() const
    {
      // set dimensions for output
      return m_dimensions;
    }
  
    EIGEN_STRONG_INLINE bool evalSubExprsIfNeeded(EvaluatorPointerType) {
      nCompiler::call_evalSubExprsIfNeeded(m_xpr_impl);
      return true;
    }
  
  // #ifdef EIGEN_USE_THREADS
  //   template <typename EvalSubExprsCallback>
  //   EIGEN_STRONG_INLINE void evalSubExprsIfNeededAsync(
  //       EvaluatorPointerType, EvalSubExprsCallback done) {
  //     // TODO(ezhulenev): Evaluate two expression in parallel?
  //     m_leftImpl.evalSubExprsIfNeededAsync(nullptr, [this, done](bool) {
  //       m_rightImpl.evalSubExprsIfNeededAsync(nullptr,
  //                                             [done](bool) { done(true); });
  //     });
  //   }
  // #endif  // EIGEN_USE_THREADS
  
    EIGEN_STRONG_INLINE void cleanup() {
      nCompiler::call_cleanup(m_xpr_impl);
    }
  
    EIGEN_DEVICE_FUNC CoeffReturnType coeff(Index index) const
    {
      // call the functor, applying the recycling rule
      return nCompiler::tupleGeneration::recycling_call_with_tuple<
        CoeffReturnType, Functor
        >(m_func, m_xpr_impl, index, m_xpr_sizes);
    }
  
    EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE TensorOpCost
    costPerCoeff(bool vectorized) const {
      return nCompiler::total_costPerCoeff<TensorOpCost>(
        m_xpr_impl, vectorized
      );
    }
  
    EIGEN_DEVICE_FUNC EvaluatorPointerType data() const { return NULL; }
  
    // #ifdef EIGEN_USE_SYCL
    // // binding placeholder accessors to a command group handler for SYCL
    // EIGEN_DEVICE_FUNC EIGEN_STRONG_INLINE void bind(cl::sycl::handler &cgh) const {
    //   m_leftImpl.bind(cgh);
    //   m_rightImpl.bind(cgh);
    // }
    // #endif

   private:
    const Device EIGEN_DEVICE_REF m_device;
    std::tuple<
      TensorEvaluator<const LeadXprType, Device>,
      TensorEvaluator<const XprTypes, Device>...
    > m_xpr_impl;
    std::array<std::size_t, sizeof...(XprTypes) + 1> m_xpr_sizes;
    Dimensions m_dimensions;
    const Functor & m_func;
  };
  
  } // end namespace Eigen

/**
 * Create an Eigen Tensorexpression that will evaluate the functor func at the 
 * values of the tensors (or tensor expressions) represented by args...  A
 * recycling rule will be used to ensure that func always has data.  
 * 
 * The recyclingFunctor will always materialize as a one dimensional tensor.  
 * The tensor's size will be equal to the largest args... tensor or 
 * tensorexpression---i.e., the arg with the most coefficients.
 */
template<typename Functor, typename... XprTypes>
Eigen::TensorCwiseRecyclingFunctorOp<Functor, XprTypes...> recyclingFunctor(
  const Functor & func, const XprTypes&... args
) {
  return Eigen::TensorCwiseRecyclingFunctorOp<Functor, XprTypes...>(
    func, args...
  );
}

/**
 * Informal template metaprogramming check via SFINAE to see if type T is an 
 * Eigen::Tensor or tensor expression.  Check is informal because it assumes the 
 * type is an Eigen::Tensor or tensorexpression if type T has a member 
 * T::NumDimensions.
 * 
 * This is the default type, for when T is (implicitly assumed to be) a true 
 * scalar type (e.g., int, double, etc.).  The check is informal because it does 
 * not use std::is_scalar to more formally check 
 */
template<typename T, typename HasNumDimensionsMember = int>
struct TypeLike {
  typedef T Scalar;
  const static bool true_scalar = true;
  const static bool scalar_tensor = false;
  const static bool true_tensor = false;
  const static std::size_t ndim = 0;
};

/**
 * Partial specialization to help implement the template metaprogramming check 
 * described above
 */
template<typename T>
struct TypeLike<T, decltype(T::NumDimensions, 0)> {
  typedef typename T::Scalar Scalar;
  const static bool true_scalar = false;
  const static std::size_t ndim = T::NumDimensions;
  const static bool scalar_tensor = ndim == 0;
  const static bool true_tensor = ndim > 0;
};

/**
 * Convenience class using template metaprogramming to choose a static run 
 * method (based on ShapeXprType) that will create an Eigen Tensor expression to
 * evaluate the functor func at the values of the tensors 
 * (or tensor expressions) passed as leadArg and args... The output will be a 
 * vector (i.e., a 1-dimensional tensor) with size (i.e., number of elements) of 
 * the output determined by shapeArg.  The functor does not use any of 
 * the data stored in shapeArg; shapeArg only provides shape information for the 
 * output.
 * 
 * This struct is useful for creating tensors with random variables as 
 * elements, using the arguments leadArg and args... to parameterize the 
 * distribution referenced by func.
 * 
 * A recycling rule is used to reuse leadArg and args..., as needed, to create 
 * as much output is requested via shapeArg.  The output size is determined 
 * using the following rules, generally reproducing those of R's rXXX random
 * generator functions:
 * 
 *   1) If shapeArg is an Eigen::Tensor or tensor expression, the output will be 
 *      a vector (i.e., a 1-dimensional tensor) with the same size (i.e., 
 *      number of elements/coefficients) as in shapeArg (i.e., shapeArg.size())
 * 
 *   2) If shapeArg is either a) a 0-dimensional Eigen::Tensor or tensor 
 *      expression (i.e., a scalar wrapped in a tensor type, perhaps resulting 
 *      from a reduction operation), or b) a scalar type (e.g., int, double)
 *      then the output will be a vector (i.e., a 1-dimensional tensor) with 
 *      "shapeArg" number of elements.
 * 
 */
template<
  typename Functor, typename ShapeXprType, typename LeadXprType, 
  typename... XprTypes
>
struct recyclingGenerator {

  // run_impl requires Tensor or tensor expressions with non-trivial size
  typedef typename std::conditional<
    // determine if ShapeXprType is a scalar or wraps one in a 0-dim tensor
    TypeLike<ShapeXprType>::true_tensor,
    // use ShapeXprType directly for non-trivial tensors
    ShapeXprType,
    // use dummy tensors to represent size info when ShapeXprType is scalar-like
    Eigen::TensorMap<Eigen::Tensor<typename LeadXprType::Scalar, 1>>
  >::type ShapeXprImpl;

  // type of the tensor expression to generate
  typedef Eigen::TensorCwiseRecyclingFunctorOp<
      Functor, 
      Eigen::TensorCwiseRecyclingOp<LeadXprType, ShapeXprImpl>,
      Eigen::TensorCwiseRecyclingOp<XprTypes, ShapeXprImpl>...
  > GeneratorXprType;

  /**
   * Common method to create the tensor expression that implements generation
   * 
   * See documentation for struct for more details about the generation rules,
   * limitations, etc.
   * 
   * @param func A callable object, who will be called with leadArg and args...
   * @param shapeArg A tensor or tensor expression, whose shape will determine 
   *  the output size
   * @param leadArg 
   * @param args 
   * @return GeneratorXprType 
   */
  static GeneratorXprType run_impl(
    const Functor & func, const ShapeXprImpl & shapeArg, 
    const LeadXprType& leadArg, const XprTypes&... args
  ) {
    return recyclingFunctor(
      func, 
      recyclingTensor(leadArg, shapeArg), 
      recyclingTensor(args, shapeArg)...
    );
  }

  /**
   * Partial specialization for when shapeArg is a non-trivial tensor or tensor
   * expression with more than one coefficient.  This is the case run_impl 
   * supports, so this function works as a pass-through
   */
  template<
    typename T = ShapeXprType,
    typename std::enable_if<TypeLike<T>::true_tensor, bool>::type = true
  >
  static GeneratorXprType run(
    const Functor & func, const ShapeXprType & shapeArg, 
    const LeadXprType & leadArg, const XprTypes&... args
  ) {
    return run_impl(func, shapeArg, leadArg, args...);
  }

  /**
   * Partial specialization for when shapeArg is a scalar wrapped in a 
   * zero-dimension Tensor.  Extracts the scalar value of shapeArg and creates a 
   * dummy tensor (ShapeXprImpl) for run_impl whose length is equal to the value 
   * stored in shapeArg
   */
  template<
    typename T = ShapeXprType,
    typename std::enable_if<TypeLike<T>::scalar_tensor, bool>::type = true
  >
  static GeneratorXprType run(
    const Functor & func, const ShapeXprType & shapeArg, 
    const LeadXprType & leadArg, const XprTypes&... args
  ) {
    return run_impl(
      // TODO: Recall that tensor expressions don't provide a coeff accessor, 
      // so consider wrapping shapeArg in a tensorref or similar object first
      func, ShapeXprImpl(nullptr, shapeArg.coeff(0)), leadArg, args...
    );
  }

  /**
   * Partial specialization for when shapeArg is a true scalar type 
   * (e.g., double, int, etc.).  Uses the value of shapeArg to create a 
   * dummy tensor (ShapeXprImpl) for run_impl whose length is equal to shapeArg
   */
  template<
    typename T = ShapeXprType,
    typename std::enable_if<TypeLike<T>::true_scalar, bool>::type = true
  >
  static GeneratorXprType run(
    const Functor & func, const ShapeXprType & shapeArg, 
    const LeadXprType & leadArg, const XprTypes&... args
  ) {
    return run_impl(func, ShapeXprImpl(nullptr, shapeArg), leadArg, args...);
  }

};

namespace nCompiler {

  /**
   * nCompiler uses the function references in scalarArgDist during C++ code
   * generation to ensure the same distribution function implementations are 
   * called regardless of whether the calling nFunctions in R use scalar or 
   * tensor arguments (by having tensor arguments use the tensorArgDist 
   * namespace, which references the scalarArgDist namespace)
   */
  namespace scalarArgDist {

    /**
     * Macro to write methods that call the R implementations for common 
     * density functions.
     */
    #define R_WRAPPER(LOCAL_NAME, R_NAME)                                      \
    template<typename... ArgTypes>                                             \
    double LOCAL_NAME(ArgTypes... args) {                                      \
      return R_NAME(args...);                                                  \
    }

    R_WRAPPER(dbeta, Rf_dbeta)
    R_WRAPPER(dbinom, Rf_dbinom)
    R_WRAPPER(dexp, Rf_dexp)
    R_WRAPPER(dgamma, Rf_dgamma)
    R_WRAPPER(dlnorm, Rf_dlnorm)
    R_WRAPPER(dnbinom, Rf_dnbinom)
    R_WRAPPER(dnorm, Rf_dnorm4)
    R_WRAPPER(dlogis, Rf_dlogis)
    R_WRAPPER(dt, Rf_dt)
    R_WRAPPER(dunif, Rf_dunif)
    R_WRAPPER(dweibull, Rf_dweibull)
    
    // match R's usage of internal besselK function
    // use template to satisfy C++ one definition rule
    template<int shift = 1> 
    double besselK(double x, double alpha, double expo) {
      return Rf_bessel_k(x, alpha, expo + static_cast<double>(shift));
    }
    
    R_WRAPPER(rbeta, Rf_rbeta)
    R_WRAPPER(rbinom, Rf_rbinom)
    R_WRAPPER(rexp, Rf_rexp)
    R_WRAPPER(rgamma, Rf_rgamma)
    R_WRAPPER(rlnorm, Rf_rlnorm)
    R_WRAPPER(rnbinom, Rf_rnbinom)
    R_WRAPPER(rnorm, Rf_rnorm)
    R_WRAPPER(rlogis, Rf_rlogis)
    R_WRAPPER(rt, Rf_rt)
    R_WRAPPER(runif, Rf_runif)
    R_WRAPPER(rweibull, Rf_rweibull)
    
  };

  /**
   * wrappers to make recycling rule tensor expressions for common distributions
   */
  namespace tensorArgDist {

    /* function signature templates must match what will be called */

    template <typename... XprTypes>
    using distn_d4i = Eigen::TensorCwiseRecyclingFunctorOp<
      double(double, double, double, double, int), XprTypes...
    >;

    template <typename... XprTypes>
    using distn_d3i = Eigen::TensorCwiseRecyclingFunctorOp<
      double(double, double, double, int), XprTypes...
    >;

    template <typename... XprTypes>
    using distn_d2i = Eigen::TensorCwiseRecyclingFunctorOp<
      double(double, double, int), XprTypes...
    >;

    template <typename... XprTypes>
    using fn_d3 = Eigen::TensorCwiseRecyclingFunctorOp<
      double(double, double, double), XprTypes...
    >;

    /* end function signature templates */

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dbeta(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dbeta, args...);
    }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dbinom(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dbinom, args...);
    }

    template<typename... XprTypes>
    distn_d2i<XprTypes...> dexp(const XprTypes&... args) {
      return distn_d2i<XprTypes...>(scalarArgDist::dexp, args...);
    }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dgamma(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dgamma, args...);
    }

    // template<typename... XprTypes>
    // distn_d3i<XprTypes...> dinvgamma(const XprTypes&... args) {
    //   return distn_d3i<XprTypes...>(nimble_dinvgamma, args...);
    // }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dlnorm(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dlnorm, args...);
    }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dnbinom(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dnbinom, args...);
    }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dnorm(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dnorm, args...);
    }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dlogis(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dlogis, args...);
    }

    template<typename... XprTypes>
    distn_d2i<XprTypes...> dt(const XprTypes&... args) {
      return distn_d2i<XprTypes...>(scalarArgDist::dt, args...);
    }

    // template<typename... XprTypes>
    // distn_d4i<XprTypes...> dt_nonstandard(const XprTypes&... args) {
    //   return distn_d4i<XprTypes...>(nimble_dt_nonstandard, args...);
    // }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dunif(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dunif, args...);
    }

    template<typename... XprTypes>
    distn_d3i<XprTypes...> dweibull(const XprTypes&... args) {
      return distn_d3i<XprTypes...>(scalarArgDist::dweibull, args...);
    }

    template<typename... XprTypes>
    fn_d3<XprTypes...> besselK(const XprTypes&... args) {
      return fn_d3<XprTypes...>(scalarArgDist::besselK, args...);
    }

    /**
     * Macro to write methods that call the static recyclyingGenerator::run 
     * method for common distributions.  A limitation is that the function name
     * NAME must be a member in the nCompiler::scalarArgDist namespace.
     */
    #define RANDOM_GENERATOR(NAME, SIGNATURE)                                  \
    template<typename ShapeXprType, typename... XprTypes>                      \
    auto NAME(const ShapeXprType & shapeArg, const XprTypes&... args) ->       \
    decltype(                                                                  \
      recyclingGenerator<SIGNATURE, ShapeXprType, XprTypes...>::run(           \
        scalarArgDist::NAME, shapeArg, args...                                 \
      )                                                                        \
    ) {                                                                        \
      return recyclingGenerator<SIGNATURE, ShapeXprType, XprTypes...>::run(    \
        scalarArgDist::NAME, shapeArg, args...                                 \
      );                                                                       \
    }

    RANDOM_GENERATOR(rbeta, double(double, double))
    RANDOM_GENERATOR(rbinom, double(double, double))
    RANDOM_GENERATOR(rexp, double(double))
    RANDOM_GENERATOR(rgamma, double(double, double))
    RANDOM_GENERATOR(rlnorm, double(double, double))
    RANDOM_GENERATOR(rnbinom, double(double, double))
    RANDOM_GENERATOR(rnorm, double(double, double))
    RANDOM_GENERATOR(rlogis, double(double, double))
    RANDOM_GENERATOR(rt, double(double))
    RANDOM_GENERATOR(runif, double(double, double))
    RANDOM_GENERATOR(rweibull, double(double, double))

  };
};

#endif
