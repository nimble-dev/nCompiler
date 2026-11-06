#ifndef ET_Rcpp_ext_FORWARD_DECLARATIONS_H_
#define ET_Rcpp_ext_FORWARD_DECLARATIONS_H_

#include <type_traits>
#include <memory>
#include <unsupported/Eigen/CXX11/Tensor>

// There are three modes of conversion between R and C++ types via Rcpp.
// 1. Rcpp::as< T >(x) converts SEXP x to type T. This is done via an Exporter class in Rcpp::Traits
// 2. Rcpp::wrap(x) converts C++ type x to SEXP. This is done via a wrap function in Rcpp.
// 3. Rcpp::traits::input_parameter< T >::type(x) is used to convert SEXP x to a type that has a conversion operator to type T.
//.   This allows the converting type (e.g. nCompiler_Eigen_SEXP_converter below) to hold intermediates or be clever,
//.     and that is how we allow ref args and blockRef args to take action upon destruction to set a variable in R.
//.   This is used in two cases:
//.   1. When Rcpp handles a function annotated with "// [[Rcpp::export]]", it uses this method.
//.   2. We use this method in the set_value pathway of the generic interface.
//.   Thus scheme is somewhat duplicative of as<>, but we support both.

// The following Exporter implements Rcpp::as< Eigen::Tensor<T, nDim> >
namespace Rcpp {
  namespace traits {
    // Casting here will be important to support.
    template <typename T, int nDim>
    class Exporter< Eigen::Tensor<T, nDim> >;

    template <typename T, int nDim>
    class Exporter< Eigen::Tensor<T, nDim>& >;

  }
}

namespace Rcpp {
  // Casting should not be necessary here but might be
  // safe to provide.
  // But note that the as<> system could invoke an unnecessary
  // eigen evaluation, prior to a copy.
  template <int nDim>
  SEXP wrap( const Eigen::Tensor<double, nDim> &x );

  template <int nDim>
  SEXP wrap( const Eigen::Tensor<int, nDim> &x );

  template <int nDim>
  SEXP wrap( const Eigen::Tensor<bool, nDim> &x );

  template <int nDim>
  SEXP wrap( const Eigen::Tensor<std::string, nDim> &x );
} // end namespace Rcpp

template< typename Scalar, int nInd >
class nCompiler_Eigen_SEXP_converter;

template< typename Scalar, int nInd >
class nCompiler_EigenRef_SEXP_converter;

template< typename Scalar, int nInd >
class nCompiler_StridedTensorMap_SEXP_converter;

namespace Rcpp {
  namespace traits {
    template<typename Scalar, int nInd>
      struct input_parameter< Eigen::Tensor<Scalar, nInd> > {
      typedef nCompiler_Eigen_SEXP_converter<Scalar, nInd> type;
    };
    template<typename Scalar, int nInd>
      struct input_parameter< Eigen::Tensor<Scalar, nInd>& > {
      typedef nCompiler_EigenRef_SEXP_converter<Scalar, nInd> type;
    };
    template<typename Scalar, int nInd>
      struct input_parameter< Eigen::StridedTensorMap< Eigen::Tensor<Scalar, nInd> > > {
      typedef nCompiler_StridedTensorMap_SEXP_converter<Scalar, nInd> type;
    };
  } // end namespace traits
} // end namespace Rcpp

#endif // ET_Rcpp_ext_FORWARD_DECLARATIONS_H_
