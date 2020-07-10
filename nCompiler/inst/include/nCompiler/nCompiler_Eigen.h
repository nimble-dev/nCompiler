#ifndef __NCOMPILER_EIGEN
#define __NCOMPILER_EIGEN

#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS

// should be in RcppEigen installation
#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>
#include <memory>
#include "tensorFlex.h"
#include "StridedTensorMap.h"
// Exporters go from SEXP to internal type

// This could be organized in a better place, but it is here for now:
#include "shared_ptr_as_wrap.h"

#include "typedefs.h"
#include "setWhich.h"
#include "repClass.h"
#include "seqClass.h"
#include "recyclingRule.h"
#include "nCompiler_Rcpp.h"
#include "cWiseUnary_external.h"

// The following Exporter implements Rcpp::as< Eigen::Tensor<T, nDim> >
namespace Rcpp {
  namespace traits {
    // Casting here will be important to support.
    template <typename T, int nDim>
      class Exporter< Eigen::Tensor<T, nDim> > {
    public:
      typedef typename Eigen::Tensor<T, nDim>::Index Index;
      typedef typename Eigen::array<Index, nDim> IndexArray;
    Exporter(SEXP Sx) : indexArray(SEXP_indices_2_IndexArray<Index, nDim>(Sx)) {
	//std::cout<<"In Exporter"<<std::endl;
	// This whole system seems to impose an extra copy.
	// That is because we're being dispatched from ::Rcpp::traits::r_type_generic_tag
	// We may need to work around that but will try not to need to.
	// Meanwhile a copy here could be spared if it is only done
	// by get().
	Sinput = Sx;
      }
      inline Eigen::Tensor<T, nDim> get(){
	typedef Eigen::Tensor<T, nDim> EigenTensorType;
	EigenTensorType t = SEXP_2_EigenTensor<T, nDim>::template copy<EigenTensorType, IndexArray>(Sinput, indexArray);
	return t;	
      }
    private:
      IndexArray indexArray;
      SEXP Sinput;
    };
  }
}

#endif
