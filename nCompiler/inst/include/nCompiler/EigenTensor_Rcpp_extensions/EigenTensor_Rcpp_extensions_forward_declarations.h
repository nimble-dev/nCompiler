#ifndef EIGENTENSOR_RCPP_EXTENSIONS_FORWARD_DECLARATIONS_H_
#define EIGENTENSOR_RCPP_EXTENSIONS_FORWARD_DECLARATIONS_H_

#include <type_traits>
#include <memory>
#include <unsupported/Eigen/CXX11/Tensor>

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

#endif // EIGENTENSOR_RCPP_EXTENSIONS_FORWARD_DECLARATIONS_H_
