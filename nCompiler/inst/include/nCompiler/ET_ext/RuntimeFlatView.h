// Flattened get/set access to an arbitrary-rank tensor (rank known only at
// run time) and/or a strided subview of it, without ever instantiating an
// Eigen::Tensor/StridedTensorMap templated on that rank.
//
// StridedTensorMap solves a different problem: an arbitrary *subview* whose
// compile-time output rank equals the number of kept (non-single) input
// dimensions, with each output dimension still corresponding to exactly one
// input dimension. It deliberately does not merge independently-strided
// dimensions into fewer output dimensions, and its setup (createSubTensorInfo*)
// is templated on that output rank. Flattening to 1D when 2+ kept dimensions
// have real sub-ranges requires unflattening a linear index against their
// sizes, which is exactly what is reimplemented here -- over std::vector
// instead of Eigen::array<_, N>, so the input rank never needs to be a
// template parameter or a runtime-to-compile-time dispatch.
//
// Intended use: build a RuntimeSubviewInfo/RuntimeFlatView once (this does
// the per-dimension fold/offset work) and hold onto it for the lifetime of
// the underlying object's stable storage; every subsequent get/set/copy is
// then just additions, no division, no rebuilding strides.

#ifndef NCOMPILER_RUNTIME_FLAT_VIEW_H_
#define NCOMPILER_RUNTIME_FLAT_VIEW_H_

#include <vector>
#include <cstddef>
#include "index_block.h"

// Per-dimension (size, native stride) for the dimensions kept after applying
// a selection, plus a single combined offset absorbing every fixed-index
// ("single") dimension's contribution and every kept dimension's start.
// Column-major: sizes[0]/strides[0] is the fastest-varying kept dimension.
struct RuntimeSubviewInfo {
  std::vector<long> sizes;
  std::vector<long> strides;
  long baseOffset = 0;
  long totalSize = 1;

  RuntimeSubviewInfo() = default;

  // input_sizes: native size of every raw dimension (e.g. ETaccessorBase::intDims()).
  // ss: one block per raw dimension. A block beyond ss.size(), or a
  // default-constructed (empty) block, selects the whole dimension (kept,
  // not dropped). No bounds checking is performed.
  template<typename SizesT>
  RuntimeSubviewInfo(const SizesT &input_sizes, const std::vector<b__> &ss) {
    const size_t rawN = input_sizes.size();
    long nativeStride = 1;
    for (size_t k = 0; k < rawN; ++k) {
      const b__ &block = (k < ss.size()) ? ss[k] : b__();
      if (block.isSingle()) {
        baseOffset += block.start() * nativeStride;
      } else {
        const long extent = block.isEmpty() ? static_cast<long>(input_sizes[k]) : block.extent();
        const long start = block.isEmpty() ? 0 : block.start();
        baseOffset += start * nativeStride;
        sizes.push_back(extent);
        strides.push_back(nativeStride);
        totalSize *= extent;
      }
      nativeStride *= input_sizes[k];
    }
  }

  size_t nDim() const { return sizes.size(); }
};

// Visits every multi-index combination over `sizes` exactly once (column-
// major: dimension 0 fastest), maintaining one native offset via pure
// addition/subtraction -- no division anywhere, regardless of rank. Calls
// fn(offset) per element. Used when the other side of a copy is a plain
// contiguous buffer, so it just needs its own pointer incremented once per
// call rather than tracked through this same carry logic.
template<typename Fn>
void walkStrided1(const std::vector<long> &sizes,
                   const std::vector<long> &strides, long base,
                   Fn fn) {
  const size_t n = sizes.size();
  if (n == 0) {
    fn(base);
    return;
  }
  std::vector<long> idx(n, 0);
  long off = base;
  for (;;) {
    fn(off);
    size_t k = 0;
    for (; k < n; ++k) {
      off += strides[k];
      if (++idx[k] < sizes[k]) break;
      off -= strides[k] * sizes[k];
      idx[k] = 0;
    }
    if (k == n) break; // every dimension carried: done
  }
}

// Two-sided version of walkStrided1: visits every multi-index combination
// over `sizes` exactly once, maintaining two native offsets (one per side)
// in lockstep via pure addition/subtraction. Used for view-to-view copies
// where both sides have the same logical shape (`sizes`) but independent
// strides/base/data -- e.g. two different objects' subviews of the same
// declared shape. Requires both sides to agree on `sizes`; callers are
// responsible for checking that before calling (see RuntimeFlatView::copyTo).
template<typename Fn>
void walkStrided2(const std::vector<long> &sizes,
                   const std::vector<long> &stridesA, long baseA,
                   const std::vector<long> &stridesB, long baseB,
                   Fn fn) {
  const size_t n = sizes.size();
  if (n == 0) {
    fn(baseA, baseB);
    return;
  }
  std::vector<long> idx(n, 0);
  long offA = baseA;
  long offB = baseB;
  for (;;) {
    fn(offA, offB);
    size_t k = 0;
    for (; k < n; ++k) {
      offA += stridesA[k];
      offB += stridesB[k];
      if (++idx[k] < sizes[k]) break;
      offA -= stridesA[k] * sizes[k];
      offB -= stridesB[k] * sizes[k];
      idx[k] = 0;
    }
    if (k == n) break; // every dimension carried: done
  }
}

// Scalar* + RuntimeSubviewInfo. Cheap to copy (a pointer and a couple of
// small vectors); meant to be built once and cached for repeated get/set,
// e.g. across the millions of iterations of a Monte Carlo loop, as long as
// the underlying storage is stable (does not move/reallocate).
template<typename Scalar>
class RuntimeFlatView {
public:
  RuntimeFlatView() : data_(nullptr) {}
  RuntimeFlatView(Scalar *data, RuntimeSubviewInfo info)
    : data_(data), info_(std::move(info)) {}
  RuntimeFlatView(Scalar *data, const std::vector<int> &intDims, const std::vector<b__> &ss)
    : data_(data), info_(RuntimeSubviewInfo(intDims, ss)) {}
  template<typename vecT>
  RuntimeFlatView(Scalar *data, const std::vector<int> &intDims, const std::vector<vecT> &ss)
    : data_(data), info_(RuntimeSubviewInfo(intDims, vec_2_vecB__(ss))) {}
  long size() const { return info_.totalSize; }
  size_t nDim() const { return info_.nDim(); }
  const RuntimeSubviewInfo &info() const { return info_; }
  Scalar *data() const { return data_; }

  // Rebinds this view to new backing storage, keeping info_ (sizes, strides,
  // baseOffset) exactly as built. Caller's responsibility that the new
  // pointer's layout matches what info_ was built against (e.g. it's the
  // same field of a different, same-shaped object).
  void setData(Scalar *data) { data_ = data; }

  // Random access by flat (linear) index: unflattens against info_.sizes
  // each call (one division per kept dimension). Prefer copyInto/copyFrom
  // for sequential access -- those avoid the per-call unflattening entirely.
  Scalar &at(long flatIndex) const {
    long offset = info_.baseOffset;
    for (size_t k = 0; k < info_.sizes.size(); ++k) {
      const long idx = flatIndex % info_.sizes[k];
      flatIndex /= info_.sizes[k];
      offset += idx * info_.strides[k];
    }
    return data_[offset];
  }

  // Random access by a full multi-index, one 0-based entry per kept
  // dimension (indsT any container supporting operator[], e.g.
  // std::vector<int> or Eigen::Tensor<int, 1>). Unlike at(flatIndex), this
  // is a direct dot-product against info_.strides -- no unflattening.
  // No bounds checking; inds must have at least info_.sizes.size() entries.
  template<typename IndsT>
  Scalar &at(const IndsT &inds) const {
    long offset = info_.baseOffset;
    for (size_t k = 0; k < info_.sizes.size(); ++k)
      offset += static_cast<long>(inds[k]) * info_.strides[k];
    return data_[offset];
  }

  // dest[0..size()-1] = this view, in canonical (column-major over kept
  // dims) order, via incremental offset updates only.
  void copyIntoVector(Scalar *dest) const {
    walkStrided1(info_.sizes, info_.strides, info_.baseOffset,
                 [&](long off) { *dest++ = data_[off]; });
  }

  Eigen::Tensor<Scalar, 1> copyIntoVector() const {
    Eigen::Tensor<Scalar, 1> out(size());
    copyIntoVector(out.data());
    return out;
  }

  void copyIntoVector(Eigen::Tensor<Scalar, 1> &dest) const {
    dest.resize(size());
    copyIntoVector(dest.data());
  }

  // this view = src[0..size()-1], inverse of copyInto.
  void copyFromVector(const Scalar *src) {
    walkStrided1(info_.sizes, info_.strides, info_.baseOffset,
                 [&](long off) { data_[off] = *src++; });
  }

  void copyFromVector(Eigen::Tensor<Scalar, 1> &dest) const {
    copyFromVector(dest.data());
  }

  // Element-wise copy to/from another object with the exact same raw shape
  // and the exact same selection applied (e.g. the same subview of two
  // 10x10x10 objects) -- so info_ itself (sizes, strides, baseOffset) is
  // guaranteed identical on both sides; only the data pointer differs. No
  // flattening, no synthetic linear index, no second stride descriptor.
  void copyToSameShape(Scalar *dstData) const {
    walkStrided1(info_.sizes, info_.strides, info_.baseOffset,
                 [&](long off) { dstData[off] = data_[off]; });
  }

  void copyFromSameShape(const Scalar *srcData) {
    walkStrided1(info_.sizes, info_.strides, info_.baseOffset,
                 [&](long off) { data_[off] = srcData[off]; });
  }

  // Element-wise copy to another view, independently strided (unlike
  // copyToSameShape, dst need not share info_ at all -- only sizes must
  // match, e.g. dst is the "same" field of a different object with its own
  // layout). No intermediate buffer: both offsets are carried incrementally
  // by walkStrided2. Throws if dst's kept-dimension shape disagrees.
  void copyTo(RuntimeFlatView &dst) const {
    if (dst.info_.sizes != info_.sizes)
      throw std::runtime_error("RuntimeFlatView::copyTo: shape mismatch");
    walkStrided2(info_.sizes, info_.strides, info_.baseOffset,
                 dst.info_.strides, dst.info_.baseOffset,
                 [&](long offSrc, long offDst) { dst.data_[offDst] = data_[offSrc]; });
  }

  // this = src, inverse of copyTo.
  void copyFrom(const RuntimeFlatView &src) { src.copyTo(*this); }

private:
  Scalar *data_;
  RuntimeSubviewInfo info_;
};

// Holds several RuntimeFlatViews and gathers/scatters them contiguously
// into/out of one shared buffer (e.g. a single parameter vector spanning
// several distinct, independently-shaped objects). Built once outside the
// hot loop, same as RuntimeFlatView itself.
template<typename Scalar>
class RuntimeFlatViewGroup {
public:
  void add(RuntimeFlatView<Scalar> view) {
    offsets_.push_back(totalSize_);
    totalSize_ += view.size();
    views_.push_back(std::move(view));
  }

  void clear() {
    views_.clear();
    offsets_.clear();
    totalSize_ = 0;
  }

  long totalSize() const { return totalSize_; }
  size_t numViews() const { return views_.size(); }
  const RuntimeFlatView<Scalar> &view(size_t i) const { return views_[i]; }
  long offset(size_t i) const { return offsets_[i]; }

  // Rebinds a single view's data pointer in place (see RuntimeFlatView::setData).
  void setData(size_t i, Scalar *data) { views_[i].setData(data); }

  // Rebinds every view's data pointer, one call per view, in the same order
  // they were add()-ed -- mirrors add() itself, so the caller just walks its
  // own objects in that order without packing the new pointers into a
  // vector first. prepare_for_setData() resets the internal cursor; each
  // setNextData() rebinds the next view and advances it.
  void prepare_for_setData() { setDataCursor_ = 0; }

  void setNextData(Scalar *data) {
    if (setDataCursor_ >= views_.size())
      throw std::runtime_error("RuntimeFlatViewGroup::setNextData: more calls than views "
                                "(did you forget prepare_for_setData()?)");
    views_[setDataCursor_++].setData(data);
  }

  Eigen::Tensor<Scalar, 1> copyIntoVector() const {
    Eigen::Tensor<Scalar, 1> out(totalSize());
    copyIntoVector(out.data());
    return out;
  }

  void copyIntoVector(Eigen::Tensor<Scalar, 1> &dest) const {
    dest.resize(totalSize());
    copyIntoVector(dest.data());
  }

  void copyIntoVector(Scalar *dest) const {
    for (size_t i = 0; i < views_.size(); ++i)
      views_[i].copyIntoVector(dest + offsets_[i]);
  }

  void copyFromVector(const Scalar *src) {
    for (size_t i = 0; i < views_.size(); ++i)
      views_[i].copyFromVector(src + offsets_[i]);
  }

  void copyFromVector(const Eigen::Tensor<Scalar, 1> &src) {
    if(src.size() != totalSize())
      throw std::runtime_error("RuntimeFlatViewGroup::copyFromVector: src.size() != totalSize()");
    copyFromVector(src.data());
  }

  // Proxy returned by setValues() so `group.setValues() = input;` reads as an
  // assignment while dispatching to copyFromVector. Overloads mirror
  // copyFromVector's own parameter types.
  class SetValuesProxy {
  public:
    explicit SetValuesProxy(RuntimeFlatViewGroup &group) : group_(group) {}

    void operator=(const Scalar *src) { group_.copyFromVector(src); }
    void operator=(const Eigen::Tensor<Scalar, 1> &src) { group_.copyFromVector(src); }

  private:
    RuntimeFlatViewGroup &group_;
  };

  SetValuesProxy setValues_() { return SetValuesProxy(*this); }

  // Element-wise copy to/from another group, view by view, with no
  // intermediate buffer -- each pair delegates to RuntimeFlatView::copyTo,
  // which checks that the pair's sizes agree. Requires both groups to have
  // the same number of views, added in corresponding order; a per-view
  // shape mismatch is caught here and rethrown with its index attached, so
  // the error names which view/field disagrees instead of leaving that to
  // the caller to work out from an unqualified "shape mismatch".
  void copyTo(RuntimeFlatViewGroup &dst) const {
    if (dst.views_.size() != views_.size())
      throw std::runtime_error("RuntimeFlatViewGroup::copyTo: numViews() mismatch");
    for (size_t i = 0; i < views_.size(); ++i) {
      try {
        views_[i].copyTo(dst.views_[i]);
      } catch (const std::runtime_error &e) {
        throw std::runtime_error("RuntimeFlatViewGroup::copyTo: view " +
                                  std::to_string(i) + ": " + e.what());
      }
    }
  }

  // this = src, inverse of copyTo.
  void copyFrom(const RuntimeFlatViewGroup &src) { src.copyTo(*this); }

private:
  std::vector<RuntimeFlatView<Scalar>> views_;
  std::vector<long> offsets_;
  long totalSize_ = 0;
  size_t setDataCursor_ = 0;
};

#endif // NCOMPILER_RUNTIME_FLAT_VIEW_H_
