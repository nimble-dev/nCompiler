#ifndef _STRIDEDTENSORMAPINFO
#define _STRIDEDTENSORMAPINFO

#include <unsupported/Eigen/CXX11/Tensor>
#include "index_block.h"

// output_sizes gives output-dimensional sizes.
// prod(output_sizes) will equal prod(input_sizes).
// So output_sizes is a logical change in dimension for the same allocated memory as the full input.
// output_strides gives strides necessary to move in each dimension of the output.
//   These strides are not cumulatively multiplied.  Each stride is the number of steps to take in its
//    dimension for a change in index.  The native values would be (1, 1, 1), not (1, dim[0], dim[0] * dim[1]), e.g.
// We can think of the output_strides as "inner strides" and the output_sizes as "outer strides".
// To move in the 0th dim, move by output_strides[0] * 1 (1 for outer strides).
// To move in the 1st dim, move by output_strides[1] * output_sizes[0]
// To move in the 2nd dim, move by output_strides[2] * output_sizes[0] * output_sizes[1];
// The output_startIndices can be used, first, for an overall offset.
// The output_startIndices and output_stopIndices can be used for checking valid indices.
// So the (i, j, k) element would be at raw flat index
//   offset + i * output_strides[0] + output_sizes[0] * (j * output_strides[1] + output_sizes[1] * k * output_strides[2])
// where offset is the index calculation of output_startIndices in the output_sizes tensor view.
// i.e. offset = startIndices[0] + output_sizes[0] * (startIndices[1] + output_sizes[1] * startIndices[2]), etc.
// Note that the offset could be written into the (i,j,k) calculation in terms of the startIndices:
//    The (i, j, k) element would be at raw flat index
//     startIndices[0] + i*output_strides[0] + output_sizes[0]*( (j*output_strides[1] + startIndices[1]) + output_sizes[1] * ( (k * output_strides[2] + startIndices[2]) )
// Note also that quantities like output_sizes[1] * output_strides[2] could be cached.

// backward compatibility with first version that used fixed input_nDim

template<typename ss_type, typename input_sizes_type, size_t output_nDim, typename ScalarType=double>
void createSubTensorInfoGeneral(const ss_type &ss,
                                const input_sizes_type &input_sizes,
                                Eigen::array<long, output_nDim> &output_sizes,
                                Eigen::array<long, output_nDim> &output_strides,
                                Eigen::array<long, output_nDim> &output_startIndices,
                                Eigen::array<long, output_nDim> &output_stopIndices) {
  typedef typename Eigen::Tensor<ScalarType, output_nDim>::Index Index;
  size_t currentOutputDim(output_nDim-1);
  if(output_nDim < 1) {
    std::cout<<"Shouldn't be calling createSubTensorInfo with output_nDim of 0.  That should be a scalar"<<std::endl;
  }
  
  Index currentOutputDimSize(1);
  Index currentOutputDimStart(0);
  //  long currentOutputDimStop(0);
  Index currentOutputStride(1);
  bool onLeadingSingletons(false);
  Index currentExtent(0);
  size_t i;
  int input_nDim = input_sizes.size();
  for(size_t ip1 = input_nDim; ip1 > 0; --ip1) {
    i = ip1-1; // weirdness of decrement sequence with unsigned type
    //   std::cout<<"i = "<<i<<" currentOutputDim = "<<currentOutputDim<<std::endl;
    if(!ss[i].isSingle()) { // not single
      currentOutputDimSize *= input_sizes[i];
      currentOutputDimStart *= input_sizes[i];
      currentOutputDimStart += ss[i].start();
      if(ss[i].isEmpty())
        currentExtent = input_sizes[i];
      else
        currentExtent = ss[i].extent();
      // finish this outputDim:
      if(currentOutputDim > 0 || i == 0) {
        if(currentOutputDim < output_nDim) { // if it was decremented below 0 (which should never happen anyway), it will actually be MAX_INT or something like that, so this is really ">= 0"
          output_sizes[currentOutputDim] = currentOutputDimSize;
          output_startIndices[currentOutputDim] = currentOutputDimStart;
          output_stopIndices[currentOutputDim] = currentOutputDimStart + currentExtent;
          output_strides[currentOutputDim] = currentOutputStride;
          --currentOutputDim;
          currentOutputDimSize = 1;
          currentOutputDimStart = 0;
          currentOutputStride = 1;
        }
      } else {
        onLeadingSingletons = true;
      }
    } else { // single
      currentOutputDimSize *= input_sizes[i];
      currentOutputDimStart *= input_sizes[i];
      currentOutputDimStart += ss[i].start();
      if(onLeadingSingletons) {
        currentOutputStride *= input_sizes[i];
      }
    }
  }
  if(onLeadingSingletons) {
    output_sizes[0] = currentOutputDimSize;
    output_startIndices[0] = currentOutputDimStart;
    output_stopIndices[0] = currentOutputDimStart + currentExtent * currentOutputStride;
    output_strides[0] = currentOutputStride;
  }
}
    
template<size_t input_nDim, size_t output_nDim, typename ScalarType=double>
void createSubTensorInfo(const Eigen::array<b__, input_nDim> &ss,
                         const Eigen::array<long, input_nDim> &input_sizes,
                         Eigen::array<long, output_nDim> &output_sizes,
                         Eigen::array<long, output_nDim> &output_strides,
                         Eigen::array<long, output_nDim> &output_startIndices,
                         Eigen::array<long, output_nDim> &output_stopIndices) {
  createSubTensorInfoGeneral< Eigen::array<b__, input_nDim>, Eigen::array<long, input_nDim>, output_nDim, ScalarType>(ss, input_sizes, output_sizes, output_strides, output_startIndices, output_stopIndices);
}
  
template<size_t input_nDim, size_t output_nDim, typename ScalarType=double>
void createSubTensorInfo(const Eigen::array<long, input_nDim> &input_sizes,
                         Eigen::array<long, output_nDim> &output_sizes,
                         Eigen::array<long, output_nDim> &output_strides,
                         Eigen::array<long, output_nDim> &output_startIndices,
                         Eigen::array<long, output_nDim> &output_stopIndices) {
  Eigen::array<b__, input_nDim> ss {}; // all empty blocks, resulting in full tensor
  createSubTensorInfoGeneral< Eigen::array<b__, input_nDim>, Eigen::array<long, input_nDim>, output_nDim, ScalarType>(ss, input_sizes, output_sizes, output_strides, output_startIndices, output_stopIndices);
}

template<size_t input_nDim, size_t output_nDim, typename ScalarType=double>
void showSubTensorInfo(const Eigen::array<b__, input_nDim> &ss,
                       const Eigen::array<long, input_nDim> &input_sizes) {
  typedef typename Eigen::Tensor<ScalarType, output_nDim>::Index Index;
  Eigen::array<Index, output_nDim> output_sizes; // I need to figure out the right type
  Eigen::array<Index, output_nDim> output_strides;
  Eigen::array<Index, output_nDim> output_startIndices;
  Eigen::array<Index, output_nDim> output_stopIndices;
  createSubTensorInfo<input_nDim, output_nDim, ScalarType>(ss, input_sizes, output_sizes, output_strides, output_startIndices, output_stopIndices);
  std::cout<<"sizes\t"; for(size_t i = 0; i < output_nDim; ++i) std::cout<<output_sizes[i]<<" "; std::cout<<std::endl;
  std::cout<<"starts\t"; for(size_t i = 0; i < output_nDim; ++i) std::cout<<output_startIndices[i]<<" "; std::cout<<std::endl;
  std::cout<<"stops\t"; for(size_t i = 0; i < output_nDim; ++i) std::cout<<output_stopIndices[i]<<" "; std::cout<<std::endl;
  std::cout<<"strides\t"; for(size_t i = 0; i < output_nDim; ++i) std::cout<<output_strides[i]<<" "; std::cout<<std::endl;
}


#endif
