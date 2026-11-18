#ifndef _STRIDEDTENSORMAPINFO
#define _STRIDEDTENSORMAPINFO

#include <unsupported/Eigen/CXX11/Tensor>
#include "index_block.h"
#include "flexDims.h"
#include <sstream>
#include <stdexcept>

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

// The input dimensions in the view are those not indexed by a scalar (not ss[i].isSingle()).
// Their extents (from ss, or the full input size for an empty block) are matched to
// output_nDim by the flex rule (see flexDims.h): extents other than 1 are used in order,
// spare output dimensions keep the leftmost 1s, and remaining output dimensions are padded
// trailing 1s. Nothing is flattened: it is an error if more extents other than 1 are in the
// view than output_nDim.
// Input dimensions that are not used for an output dimension (scalar-indexed ones, and 1s that
// are not kept) contribute only to the offset and to output_sizes.

template<typename ss_type, typename input_sizes_type, size_t output_nDim, typename ScalarType=double>
void createSubTensorInfoGeneral(const ss_type &ss,
                                const input_sizes_type &input_sizes,
                                Eigen::array<long, output_nDim> &output_sizes,
                                Eigen::array<long, output_nDim> &output_strides,
                                Eigen::array<long, output_nDim> &output_startIndices,
                                Eigen::array<long, output_nDim> &output_stopIndices) {
  typedef typename Eigen::Tensor<ScalarType, output_nDim>::Index Index;
  if(output_nDim < 1) {
    std::cout<<"Shouldn't be calling createSubTensorInfo with output_nDim of 0.  That should be a scalar"<<std::endl;
    return;
  }
  const int input_nDim = static_cast<int>(input_sizes.size());
  // Extent of input dimension i in the view, or -1 if it is indexed by a scalar.
  auto input_extents = [&ss, &input_sizes](int i) -> Index {
    if(ss[i].isSingle()) return -1;
    if(ss[i].isEmpty()) return input_sizes[i];
    return ss[i].extent();
  };
  // outIndex_2_inIndex[j] is the input dimension used for output dimension j, or -1 for a padded trailing 1.
  Eigen::array<int, output_nDim> outIndex_2_inIndex;
  if(!flexDimsMap(input_extents, input_nDim, static_cast<int>(output_nDim), outIndex_2_inIndex)) {
    std::ostringstream msg;
    msg << "Cannot create requested variable block with " << output_nDim << " dimension(s) from extents (";
    bool first = true;
    for(int i = 0; i < input_nDim; ++i) {
      if(input_extents(i) < 0) continue;
      msg << (first ? "" : ", ") << input_extents(i);
      first = false;
    }
    msg << ").";
    throw std::runtime_error(msg.str());
  }
  int nUsed = 0;
  while(nUsed < static_cast<int>(output_nDim) && outIndex_2_inIndex[nUsed] >= 0) ++nUsed;

  // Walk the input dimensions from last to first. Each output dimension j > 0 accumulates the
  // sizes and offset of the input dimensions from outIndex_2_inIndex[j] up to (but not including) outIndex_2_inIndex[j+1]
  // (or up to the last input dimension for the last used output dimension).
  // Output dimension 0 also accumulates the input dimensions below outIndex_2_inIndex[0], whose sizes
  // multiply into its stride. It is finished after the loop.
  // If no input dimension is used, output dimension 0 is a single element at the offset.
  int currentOutputDim = nUsed - 1;
  Index currentOutputDimSize(1);
  Index currentOutputDimStart(0);
  Index currentOutputStride(1);
  Index dim0Extent(1);
  bool belowDim0(false);
  for(int i = input_nDim - 1; i >= 0; --i) {
    currentOutputDimSize *= input_sizes[i];
    currentOutputDimStart *= input_sizes[i];
    currentOutputDimStart += ss[i].start(); // 0 for an empty block
    if(belowDim0) currentOutputStride *= input_sizes[i];
    if(currentOutputDim >= 0 && i == outIndex_2_inIndex[currentOutputDim]) {
      if(currentOutputDim > 0) {
        output_sizes[currentOutputDim] = currentOutputDimSize;
        output_startIndices[currentOutputDim] = currentOutputDimStart;
        output_stopIndices[currentOutputDim] = currentOutputDimStart + input_extents(i);
        output_strides[currentOutputDim] = 1;
        currentOutputDimSize = 1;
        currentOutputDimStart = 0;
      } else {
        dim0Extent = input_extents(i);
        belowDim0 = true;
      }
      --currentOutputDim;
    }
  }
  output_sizes[0] = currentOutputDimSize;
  output_startIndices[0] = currentOutputDimStart;
  output_stopIndices[0] = currentOutputDimStart + dim0Extent * currentOutputStride;
  output_strides[0] = currentOutputStride;
  // padded trailing 1s
  for(int j = (nUsed > 0 ? nUsed : 1); j < static_cast<int>(output_nDim); ++j) {
    output_sizes[j] = 1;
    output_startIndices[j] = 0;
    output_stopIndices[j] = 1;
    output_strides[j] = 1;
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
