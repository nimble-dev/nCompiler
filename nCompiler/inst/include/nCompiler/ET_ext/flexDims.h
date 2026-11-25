#ifndef NCOMPILER_FLEX_DIMS_H_
#define NCOMPILER_FLEX_DIMS_H_

// The "flex" rule for matching the extents of an object to a target number of dimensions.
// This one rule is used by flex_ assignment (checkAndSetupDims in tensorFlex.h),
// by ETaccessor maps (set_output_dims in ETaccessor_post_Rcpp.h), and by
// StridedTensorMap views (createSubTensorInfoGeneral in StridedTensorMapInfo.h).
//
// The rule: extents other than 1 are always used, in order. Nothing is flattened or reshaped.
// If there are fewer extents other than 1 than target dimensions, the spare target dimensions
// keep the leftmost 1s that come before the last extent other than 1 (leading or interior 1s),
// and any still remaining are padded as trailing 1s.
// E.g. for 3 target dimensions, extents (5, 1, 6, 1) give (5, 1, 6), and (1, 5) gives (1, 5, 1).
// It fails if there are more extents other than 1 than target dimensions.

// flexDimsMap sets outIndex_2_inIndex[j], for each target dimension j < outNdim, to the index of the input
// dimension that it uses, or to -1 for a padded trailing 1. The used input dimensions are in
// increasing order, so they come first in outIndex_2_inIndex, followed by any -1s.
// input_extents(i) gives the extent of input dimension i (for i < inNdim). A negative value means that
// input dimension i is not a dimension of the object and is never used (e.g. a dimension
// indexed by a scalar in a StridedTensorMap).
// outIndex_2_inIndex can be any array-like type with outNdim elements.
// Returns false (and leaves outIndex_2_inIndex incomplete) if there are more extents other than 1 than outNdim.
template<typename ExtentFunType, typename IndexMapType>
bool flexDimsMap(const ExtentFunType &input_extents, int inNdim, int outNdim, IndexMapType &outIndex_2_inIndex) {
  // First pass: count the extents other than 1 and find the last one.
  int nNon1 = 0;
  int iLastNon1 = -1;
  for(int iIn = 0; iIn < inNdim; ++iIn) {
    auto e = input_extents(iIn);
    if(e >= 0 && e != 1) {
      ++nNon1;
      iLastNon1 = iIn;
    }
  }
  if(nNon1 > outNdim) return false; // Not enough target dimensions: fail
  int nSpare = outNdim - nNon1; // number of 1s that can be kept
  // Second pass: use the extents other than 1, and 1s while there are spare target dimensions.
  int iOut = 0;
  for(int iIn = 0; iIn <= iLastNon1; ++iIn) {
    auto e = input_extents(iIn);
    if(e < 0) continue;
    if(e != 1) {
      outIndex_2_inIndex[iOut++] = iIn;
    } else if(nSpare > 0) {
      outIndex_2_inIndex[iOut++] = iIn;
      --nSpare;
    }
  }
  // pad any remaining target dimensions with trailing 1s
  for( ; iOut < outNdim; ++iOut) outIndex_2_inIndex[iOut] = -1;
  return true;
}

// flexDimsSetup sets outDims (outNdim elements) from inDims (inNdim elements) by the flex rule.
// inDims and outDims can be any array-like types (e.g. Eigen::DSizes, Eigen::array, std::vector).
// Returns false (and leaves outDims incomplete) if that is not possible.
template<typename InDimsType, typename OutDimsType>
bool flexDimsSetup(const InDimsType &inDims, int inNdim, OutDimsType &outDims, int outNdim) {
  // outDims first holds the input indices from flexDimsMap, which are then replaced by extents.
  if(!flexDimsMap([&inDims](int i) { return inDims[i]; }, inNdim, outNdim, outDims)) return false;
  for(int iOut = 0; iOut < outNdim; ++iOut) {
    long iIn = static_cast<long>(outDims[iOut]);
    outDims[iOut] = iIn < 0 ? 1 : inDims[iIn];
  }
  return true;
}

#endif // NCOMPILER_FLEX_DIMS_H_
