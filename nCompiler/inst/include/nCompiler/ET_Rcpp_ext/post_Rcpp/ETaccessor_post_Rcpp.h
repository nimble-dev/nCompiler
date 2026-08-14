#ifndef NCOMPILER_ETACCESSOR_POST_RCPP_H_
#define NCOMPILER_ETACCESSOR_POST_RCPP_H_

#include <unsupported/Eigen/CXX11/Tensor>
#include <memory>
#include <type_traits>
#include <nCompiler/ET_ext/StridedTensorMap.h>
#include <nCompiler/ET_ext/RuntimeFlatView.h>
#include <nCompiler/ET_ext/post_Rcpp/tensorUtils.h>
#include <nCompiler/ET_ext/post_Rcpp/tensorFlex.h>

template<typename Scalar>
class ETaccessorTyped;

template<typename inDimsT, typename outDimsT>
void set_output_dims(const inDimsT &inDim, outDimsT &outDim,
                    size_t output_nDim) {
  size_t in_nDim = inDim.size();
  if(output_nDim >= in_nDim) {
    for(size_t i = 0; i < in_nDim; ++i) 
      outDim[i] = inDim[i];
    for(size_t i = in_nDim; i < output_nDim; ++i)
      outDim[i] = 1;
  } else {
    size_t i_out = 0;
    for(size_t i_in = 0; i_in < in_nDim; ++i_in) {
      if(inDim[i_in] > 1) {
        if(i_out >= output_nDim)
          Rcpp::stop("Too many non-singleton dimensions for requested morphing to lower dimensional object.");
        outDim[i_out++] = inDim[i_in];
      }
    }
    for(; i_out < output_nDim; ++i_out)
      outDim[i_out] = 1;
  }
}

enum class AsMode { TM, STM, LHS };

// Forward declarations: proxy classes are defined in nC_as.h (included after
// this file). ETaccessorTyped::asTyped() returns them; because asTyped() is a
// template, instantiation is deferred to the call site where they are fully
// defined.
template<typename ViewType> class EmptyProxy;
template<typename Scalar> class EmptyScalarProxy;
template<typename TargetScalar, typename ViewType> class RHSCastProxy;
template<typename TargetScalar, typename ViewType> class CastingProxy;
template<typename TargetScalar, typename Scalar> class CastingScalarProxy;

// Virtual nDim-general methods (e.g. resize, conversions to and from SEXP).
class ETaccessorBase {
  public:
//  virtual void resize(Eigen::Tensor<double, 1> &t)=0;
  // To iron out: set, get, generic ref access.
  virtual void set(SEXP Sinput)=0;
  virtual SEXP get()=0;
  virtual SEXP operator=(SEXP RHS) {set(RHS); return RHS;}

  virtual std::vector<int> &intDims()=0;

  // Virtual element-wise cast/writeback for cross-scalar RuntimeCastingProxy.
  // Only 3 scalar types are supported so virtual templates are avoided.
  virtual void castCopyToDouble(double* dest, size_t n) {
    Rcpp::stop("castCopyToDouble not supported for this ETaccessor type.");
  }
  virtual void castCopyToInt(int* dest, size_t n) {
    Rcpp::stop("castCopyToInt not supported for this ETaccessor type.");
  }
  virtual void castCopyToBool(bool* dest, size_t n) {
    Rcpp::stop("castCopyToBool not supported for this ETaccessor type.");
  }
  virtual void writeBackFromDouble(const double* src, size_t n) {
    Rcpp::stop("writeBackFromDouble not supported for this ETaccessor type.");
  }
  virtual void writeBackFromInt(const int* src, size_t n) {
    Rcpp::stop("writeBackFromInt not supported for this ETaccessor type.");
  }
  virtual void writeBackFromBool(const bool* src, size_t n) {
    Rcpp::stop("writeBackFromBool not supported for this ETaccessor type.");
  }

  template<int nDim, typename Scalar>
  using ETM = Eigen::TensorMap<Eigen::Tensor<Scalar, nDim> >;

  template<int nDim, typename Scalar>
  using ESTM = Eigen::StridedTensorMap<Eigen::Tensor<Scalar, nDim> >;

  template<typename Scalar = double>
  ETaccessorTyped<Scalar> &S() {
    auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
    if(castptr == nullptr) Rcpp::stop("Problem with some form of access()\n.");
    return *castptr;
  }

  template<int nDim, typename Scalar = double>
  ETM<nDim, Scalar> map();

  template<int nDim, typename Scalar = double>
  ESTM<nDim, Scalar> STmap();

  template<int nDim, typename Scalar = double>
  Eigen::Tensor<Scalar, nDim> &ref();

  template<typename Scalar = double>
  Scalar &scalar();

  // Flattened get/set view of the whole object (ss empty, the default) or
  // a strided subview of it (ss: one b__ block per raw dimension), without
  // needing to know the object's rank at compile time. See RuntimeFlatView.h.
  template<typename Scalar = double>
  RuntimeFlatView<Scalar> flatten(const std::vector<b__> &ss = {});

  virtual ~ETaccessorBase(){};
};

template<typename Scalar>
class ETaccessorTyped : public ETaccessorBase {
  public:

  virtual Scalar *data()=0;

  template<int nDim>
  using ETM = Eigen::TensorMap<Eigen::Tensor<Scalar, nDim> >;

  Scalar &scalarTyped() {
    const auto intDims_ = this->intDims();
    for(size_t i = 0; i < intDims_.size(); ++i) {
      if(intDims_[i]!=1)
        Rcpp::stop("Invalid call to scalar() for ETaccessor with dimensions not all equal to 1.");
    }
    return *data();
  }

  // Cast/writeback implementations (element-wise, supports all 3 scalar types).
  void castCopyToDouble(double* dest, size_t n) override {
    Scalar* src = data();
    for(size_t i = 0; i < n; ++i) dest[i] = static_cast<double>(src[i]);
  }
  void castCopyToInt(int* dest, size_t n) override {
    Scalar* src = data();
    for(size_t i = 0; i < n; ++i) dest[i] = static_cast<int>(src[i]);
  }
  void castCopyToBool(bool* dest, size_t n) override {
    Scalar* src = data();
    for(size_t i = 0; i < n; ++i) dest[i] = static_cast<bool>(src[i]);
  }
  void writeBackFromDouble(const double* src, size_t n) override {
    Scalar* dest = data();
    for(size_t i = 0; i < n; ++i) dest[i] = static_cast<Scalar>(src[i]);
  }
  void writeBackFromInt(const int* src, size_t n) override {
    Scalar* dest = data();
    for(size_t i = 0; i < n; ++i) dest[i] = static_cast<Scalar>(src[i]);
  }
  void writeBackFromBool(const bool* src, size_t n) override {
    Scalar* dest = data();
    for(size_t i = 0; i < n; ++i) dest[i] = static_cast<Scalar>(src[i]);
  }

  template<int output_nDim>
  using ESTM = Eigen::StridedTensorMap<Eigen::Tensor<Scalar, output_nDim> >;

  // StridedTensorMap variant of mapTyped — same singleton-drop/pad logic.
  template<int output_nDim>
  ESTM<output_nDim> STmapTyped() {
    return Eigen::MakeStridedTensorMap<output_nDim>::make(mapTyped<output_nDim>());
  }

  // Central dispatch for as() operations. Returns a proxy wrapping the
  // appropriate view. All proxy types expose operator()() uniformly.
  template<typename TargetType, AsMode mode = AsMode::TM, 
           std::enable_if_t<std::is_same_v<type_category_t<TargetType>, eigenTensor>, int> = 0>
  auto asTyped() {
    typedef typename Eigen::nDimTraits2<TargetType>::Scalar TargetScalar;
    constexpr int nDim = Eigen::nDimTraits2<TargetType>::NumDimensions;
    if constexpr (std::is_same_v<TargetScalar, Scalar>) {
      if constexpr (mode == AsMode::TM)
        return EmptyProxy<ETM<nDim>>(mapTyped<nDim>());
      else
        return EmptyProxy<ESTM<nDim>>(STmapTyped<nDim>());
    } else {
      if constexpr (mode == AsMode::LHS) {
        auto view = STmapTyped<nDim>();
        return CastingProxy<TargetScalar, decltype(view)>(view);
      } else if constexpr (mode == AsMode::STM) {
        // Indexed RHS: use STM so that non-contiguous sources (e.g. blockRef)
        // have correct strides in the lazy cast expression.
        auto view = STmapTyped<nDim>();
        return RHSCastProxy<TargetScalar, decltype(view)>(view);
      } else {
        auto view = mapTyped<nDim>();
        return RHSCastProxy<TargetScalar, decltype(view)>(view);
      }
    }
  }

  template<typename TargetType, AsMode mode = AsMode::TM, 
           std::enable_if_t<std::is_same_v<type_category_t<TargetType>, trueScalar>, int> = 0>
  auto asTyped() {
    typedef TargetType TargetScalar;
    if constexpr (std::is_same_v<TargetScalar, Scalar>) {
      return EmptyScalarProxy<TargetScalar>(scalarTyped());
    } else {
      return CastingScalarProxy<TargetScalar, Scalar>(scalarTyped());
    }
  }

  template<int output_nDim>
  ETM<output_nDim> mapTyped() {
    //innate_nDim is the nDim of the object.
    //output_nDim is the requested nDim of the output map.
    //If the output_nDim > innate_nDim, then set the output dims to innate dims padded with 1s.
    //If the output_nDim < innate_nDim, then drop singleton dimensions in the innate dims.
    //This is very similar to checkAndSetupDims in tensorFlex.h
    //but there both the LHS and RHS nDims are known at compile time.
    //Here only the output_nDim is known at compile time.
    //Also it looks like in checkAndSetupDims, RHS singletons are always dropped
    // typedef typename Eigen::internal::traits<ETM<output_nDim> >::Index Index;
    typedef typename ETM<output_nDim>::Dimensions output_Dimensions;
    output_Dimensions outDim;
    set_output_dims(this->intDims(), outDim, output_nDim);
    return ETM<output_nDim>(data(), outDim);
  }

  RuntimeFlatView<Scalar> flattenTyped(const std::vector<b__> &ss = {}) {
    return RuntimeFlatView<Scalar>(data(), RuntimeSubviewInfo(this->intDims(), ss));
  }

  ~ETaccessorTyped(){};
};

template<int nDim, typename Scalar>
Eigen::TensorMap<Eigen::Tensor<Scalar, nDim> > ETaccessorBase::map() {
  auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
  if(castptr == nullptr) Rcpp::stop("Problem creating a map() from some form of access().\n");
  return castptr->template mapTyped<nDim>();
}

template<int nDim, typename Scalar>
Eigen::StridedTensorMap<Eigen::Tensor<Scalar, nDim> > ETaccessorBase::STmap() {
  auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
  if(castptr == nullptr) Rcpp::stop("Problem creating an STmap() from some form of access().\n");
  return castptr->template STmapTyped<nDim>();
}

template<typename Scalar>
Scalar& ETaccessorBase::scalar() {
  auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
  if(castptr == nullptr) Rcpp::stop("Problem using scalar() from some form of access().\n");
  return castptr->scalarTyped();
}

template<typename Scalar>
RuntimeFlatView<Scalar> ETaccessorBase::flatten(const std::vector<b__> &ss) {
  auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
  if(castptr == nullptr) Rcpp::stop("Problem creating a flatten() view from some form of access().\n");
  return castptr->flattenTyped(ss);
}

// default to throwing an error
// then specialize to allow valid types (Eigen::Tensor's or true scalars)
// These are supported as run-time errors because the genericInterfaceC
// will access them by a name.
template<typename ERROR, bool copy=false>
class ETaccessor : public ETaccessorTyped<double> {
  public:
  // Assignment operators are never inherited: the compiler's implicit
  // copy-assignment operator for this class would otherwise hide the
  // virtual ETaccessorBase::operator=(SEXP), breaking `ETaccess(x) = SEXP`.
  using ETaccessorBase::operator=;
  using ET = Eigen::Tensor<double, 0>;
  // I think to compile this all needs to be valid in terms of types but throw run-time errors everywhere.
  // It should never get past the constructor because that throws an error, but other errors are written
  //   for good measure.
  ETaccessor(ERROR &obj_) { Rcpp::stop("Invalid use of some form of access(). You probably tried to access() a non-numeric object.\n"); }
  ~ETaccessor() {};
  double *data() override {
    Rcpp::stop("Invalid call to data() for invalid ETaccessor.");
    return nullptr;
  }
  std::vector<int> &intDims() override {
    Rcpp::stop("Invalid call to intDims() for invalid ETaccessor.");
    return intDims_;
  }
  void set(SEXP Sinput) override {
    Rcpp::stop("Invalid call to set() for invalid ETaccessor.");
  }
  SEXP get() override {
    Rcpp::stop("Invalid call to get() for invalid ETaccessor.");
    return R_NilValue;
  }
  ET &innerRef() {
    Rcpp::stop("Invalid call to ref() for invalid ETaccessor.");
    return obj;
  }
  double &scalar() {
    Rcpp::stop("Invalid call to scalar() for invalid ETaccessor.");
    return *new double(0.); // would leak memory but will never be reached and may reduce compiler warnings
  }
  ET obj;
  std::vector<int> intDims_;
};


template<typename Scalar, int nDim, bool copy>
class ETaccessor<Eigen::Tensor<Scalar, nDim>, copy> : public ETaccessorTyped<Scalar> {
  public:
  // See note in the ETaccessor<ERROR, copy> primary template above.
  using ETaccessorBase::operator=;
  using ET = Eigen::Tensor<Scalar, nDim>;
  // using Scalar = typename ET::Scalar;
  typedef typename Eigen::internal::traits<ET>::Index Index;
  // NumIndices should match nDim, so this is a bit redundant.
  static const Index NumIndices = ET::NumIndices; // StridedTensorMap: This is output number of dimensions (indices).
  typedef typename ET::Dimensions Dimensions;
  ETaccessor(ET &obj_) : obj(obj_) {};
  ~ETaccessor() {};
  Scalar *data() override {return obj.data();}
  // Sized lazily here rather than in the constructor: a no-op (no
  // reallocation) on every call after the first, but avoids the allocation
  // entirely for an ETaccessor whose intDims() is never called (e.g. only
  // data() is used).
  std::vector<int> &intDims() override {
    intDims_.resize(NumIndices);
    Dimensions dim = obj.dimensions();
    std::copy(dim.begin(), dim.end(), intDims_.begin());
    return intDims_;
  }
  void set(SEXP Sinput) override {
    obj = as<ET>(Sinput);
  }
  SEXP get() override {
    return wrap(obj);
  }
  ET &innerRef() {return obj;}
  ET &obj;
  std::vector<int> intDims_;
};

template<typename ET>
struct ETaccessorCopyHolder {
  ET obj_copy;
  ETaccessorCopyHolder(const ET &src) : obj_copy(src) {}
};

template<typename Scalar, int nDim>
class ETaccessor<Eigen::Tensor<Scalar, nDim>, true> : 
  private ETaccessorCopyHolder<Eigen::Tensor<Scalar, nDim>>,
  public ETaccessor<Eigen::Tensor<Scalar, nDim>, false> {
public:
  // See note in the ETaccessor<ERROR, copy> primary template above.
  using ETaccessorBase::operator=;
  using ET = Eigen::Tensor<Scalar, nDim>;
  using Holder = ETaccessorCopyHolder<ET>;
  ETaccessor(const ET &obj_) : Holder(obj_), ETaccessor<ET, false>(Holder::obj_copy) {};
  ~ETaccessor() {};
};

template<typename Scalar, bool copy=false>
class ETaccessorScalar : public ETaccessorTyped<Scalar> {
  public:
  // See note in the ETaccessor<ERROR, copy> primary template above.
  using ETaccessorBase::operator=;
  ETaccessorScalar(Scalar &obj_) : obj(obj_) {};
  ~ETaccessorScalar() {};
  Scalar *data() override {return &obj;}
  std::vector<int> &intDims() override {return intDims_;}
  void set(SEXP Sinput) override { obj = as<Scalar>(Sinput);}
  SEXP get() override {return wrap(obj);}
  Eigen::Tensor<double, 0> &innerRef() {
    Rcpp::stop("Invalid call to ref() for ETaccessor to scalar.");
    return *new Eigen::Tensor<double, 0>(); // bad memory mgmt (would leak) but will never be called. only to show compiler valid return.
  }
  Scalar &obj;
  std::vector<int> intDims_;
};

template<typename Scalar>
class ETaccessorScalar<Scalar, true> : 
  private ETaccessorCopyHolder<Scalar>,
  public ETaccessorScalar<Scalar, false> {
public:
  // See note in the ETaccessor<ERROR, copy> primary template above.
  using ETaccessorBase::operator=;
  using ET = ETaccessorScalar<Scalar, false>;
  using Holder = ETaccessorCopyHolder<Scalar>;
  ETaccessorScalar(const Scalar &obj_) : Holder(obj_), ET(Holder::obj_copy) {};
  ~ETaccessorScalar() {};
};

template<bool copy>
class ETaccessor<double, copy> : public ETaccessorScalar<double, copy> {
  using Ref = std::conditional_t<copy, const double&, double&>;
  public:
  // See note in the ETaccessor<ERROR, copy> primary template above.
  using ETaccessorBase::operator=;
  ETaccessor(Ref obj_) : ETaccessorScalar<double, copy>(obj_) {};
  ~ETaccessor() {};
};

template<bool copy>
class ETaccessor<int, copy> : public ETaccessorScalar<int, copy> {
  using Ref = std::conditional_t<copy, const int&, int&>;
  public:
  // See note in the ETaccessor<ERROR, copy> primary template above.
  using ETaccessorBase::operator=;
  ETaccessor(Ref obj_) : ETaccessorScalar<int, copy>(obj_) {};
  ~ETaccessor() {};
};

template<bool copy>
class ETaccessor<bool, copy> : public ETaccessorScalar<bool, copy> {
  using Ref = std::conditional_t<copy, const bool&, bool&>;
  public:
  // See note in the ETaccessor<ERROR, copy> primary template above.
  using ETaccessorBase::operator=;
  ETaccessor(Ref obj_) : ETaccessorScalar<bool, copy>(obj_) {};
  ~ETaccessor() {};
};

template<int nDim, typename Scalar>
Eigen::Tensor<Scalar, nDim> &ETaccessorBase::ref() {
  auto castptr = dynamic_cast<ETaccessor<Eigen::Tensor<Scalar, nDim> >* >(this);
  if(castptr == nullptr) Rcpp::stop("Problem creating a ref() from some form of access().\n");
  return castptr->innerRef();
}

template<bool copy=false, typename T>
std::enable_if_t<!copy, ETaccessor<T, false>>
ETaccess(T &x) {
  return ETaccessor<T, false>(x);
}

template<bool copy, typename T>
std::enable_if_t<copy, ETaccessor<T, true>>
ETaccess(const T &x) {
  return ETaccessor<T, true>(x);
}

template<bool copy=false, typename T>
std::enable_if_t<!copy, std::unique_ptr<ETaccessorBase>>
ETaccessPtr(T &x) {
  return std::make_unique<ETaccessor<T, false>>(x);
}

template<bool copy, typename T>
std::enable_if_t<copy, std::unique_ptr<ETaccessorBase>>
ETaccessPtr(const T &x) {
  return std::make_unique<ETaccessor<T, true>>(x);
}

// end ETaccess

template<typename Scalar, typename SStype>
RuntimeFlatView<Scalar> makeRuntimeFlatView(std::unique_ptr<ETaccessorBase> &acc, 
                                            const SStype &ss) {
  return RuntimeFlatView<Scalar>(acc->template S<Scalar>().data(), acc->intDims(), ss);
}

#endif // NCOMPILER_ETACCESSOR_POST_RCPP_H_
