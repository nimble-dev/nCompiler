#ifndef NCOMPILER_ETACCESSOR_POST_RCPP_H_
#define NCOMPILER_ETACCESSOR_POST_RCPP_H_

#include <unsupported/Eigen/CXX11/Tensor>
#include <type_traits>

template<typename Scalar>
class ETaccessorTyped;

using Eigen::StridedTensorMap;

enum class AsMode { TM, STM, LHS };

// Forward declaration: CastingProxy is defined in nC_as.h (included after
// this file). ETaccessorTyped::asTyped() returns it; because asTyped() is a
// template, instantiation is deferred to the call site where CastingProxy is
// fully defined.
template<typename TargetScalar, typename ViewType> class CastingProxy;

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
  using ESTM = StridedTensorMap<Eigen::Tensor<Scalar, nDim> >;

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
  using ESTM = StridedTensorMap<Eigen::Tensor<Scalar, output_nDim> >;

  // StridedTensorMap variant of mapTyped — same singleton-drop/pad logic.
  template<int output_nDim>
  ESTM<output_nDim> STmapTyped() {
    return Eigen::MakeStridedTensorMap<output_nDim>::make(mapTyped<output_nDim>());
  }

  // Central dispatch for as() operations. Selects view type and write-back
  // behaviour at compile time based on scalar match and AsMode.
  template<typename TargetScalar, int nDim, AsMode mode = AsMode::TM>
  auto asTyped() {
    if constexpr (std::is_same_v<TargetScalar, Scalar>) {
      if constexpr (mode == AsMode::TM)
        return mapTyped<nDim>();
      else
        return STmapTyped<nDim>(); // mode == STM or LHS: STM for full element access
    } else {
      if constexpr (mode == AsMode::LHS) {
        auto view = STmapTyped<nDim>(); // STM for write-back correctness (handles non-contiguous sources)
        return CastingProxy<TargetScalar, decltype(view)>(view);
      } else {
        return mapTyped<nDim>().template cast<TargetScalar>(); // lazy Eigen expression, RHS only
      }
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
    typedef typename Eigen::internal::traits<ETM<output_nDim> >::Index Index;
    typedef typename ETM<output_nDim>::Dimensions output_Dimensions;
    output_Dimensions outDim;
    const auto intDims_ = this->intDims();
    size_t innate_nDim = intDims_.size();
    if(output_nDim >= innate_nDim) {
      for(size_t i = 0; i < innate_nDim; ++i)
        outDim[i] = intDims_[i];
      if(output_nDim > innate_nDim) {
        for(size_t i = innate_nDim; i < output_nDim; ++i)
          outDim[i] = 1;
      }
    } else {
      size_t i_out = 0;
      for(size_t i_innate = 0 ; i_innate < innate_nDim; ++i_innate) {
        if(intDims_[i_innate] > 1) {
          if(i_out >= output_nDim) {
            Rcpp::stop("Problem making a TensorMap from some form of access(): Too many non-singleton dimensions for the requested map dimensions.\n");
            break;
          } else {
            outDim[i_out++] = intDims_[i_innate];
          }
        }
      }
      for( ; i_out < output_nDim; ++i_out ) outDim[i_out]=1;
    }
    return ETM<output_nDim>(data(), outDim);
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
StridedTensorMap<Eigen::Tensor<Scalar, nDim> > ETaccessorBase::STmap() {
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

// default to throwing an error
// then specialize to allow valid types (Eigen::Tensor's or true scalars)
// These are supported as run-time errors because the genericInterfaceC
// will access them by a name.
template<typename ERROR>
class ETaccessor : public ETaccessorTyped<double> {
  public:
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


template<typename Scalar, int nDim>
class ETaccessor<Eigen::Tensor<Scalar, nDim> > : public ETaccessorTyped<Scalar> {
  public:
  using ET = Eigen::Tensor<Scalar, nDim>;
  // using Scalar = typename ET::Scalar;
  typedef typename Eigen::internal::traits<ET>::Index Index;
  // NumIndices should match nDim, so this is a bit redundant.
  static const Index NumIndices = ET::NumIndices; // StridedTensorMap: This is output number of dimensions (indices).
  typedef typename ET::Dimensions Dimensions;
  ETaccessor(ET &obj_) : obj(obj_), intDims_(NumIndices) {};
  ~ETaccessor() {};
  Scalar *data() override {return obj.data();}
  std::vector<int> &intDims() override {
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
  // Scalar &scalar() {
  //   Dimensions dim = obj.dimensions();
  //   for(int i = 0; i < nDim; ++i) {
  //     if(dim[i]!=1)
  //       Rcpp::stop("Invalid call to scalar() for ETaccessor with dimensions not all equal to 1.");
  //   }
  //   return *obj.data(); // would leak memory but will never be reached and may reduce compiler warnings
  // }
  ET &obj;
  std::vector<int> intDims_;
};

template<typename Scalar>
class ETaccessorScalar : public ETaccessorTyped<Scalar> {
  public:
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
  //Scalar &scalar() {return obj;}
  Scalar &obj;
  std::vector<int> intDims_;
};

template<>
class ETaccessor<double> : public ETaccessorScalar<double> {
  public:
  ETaccessor(double &obj_) : ETaccessorScalar(obj_) {};
  ~ETaccessor() {};
};

// // CppAD header is not read by here, so this needs attention.
// template<>
// class ETaccessor<CppAD::AD<double> > : public ETaccessorScalar<CppAD::AD<double> > {
//   public:
//   ETaccessor(CppAD::AD<double> &obj_) : ETaccessorScalar(obj_) {};
//   ~ETaccessor() {};
// };

template<>
class ETaccessor<int> : public ETaccessorScalar<int> {
  public:
  ETaccessor(int &obj_) : ETaccessorScalar(obj_) {};
  ~ETaccessor() {};
};

template<>
class ETaccessor<bool> : public ETaccessorScalar<bool> {
  public:
  ETaccessor(bool &obj_) : ETaccessorScalar(obj_) {};
  ~ETaccessor() {};
};

// template<>
// class ETaccessor<double> : public ETaccessorTyped<double> {
//   public:
//   using Scalar = double;

//   ETaccessor(Scalar &obj_) : obj(obj_) {};
//   ~ETaccessor() {};
//   Scalar *data() override {return &obj;}
//   std::vector<int> &intDims() override {return intDims_;}
//   void set(SEXP Sinput) override { obj = as<Scalar>(Sinput);}
//   SEXP get() override {return wrap(obj);}
//   Eigen::Tensor<double, 0> &ref() {
//     Rcpp::stop("Invalid call to ref() for ETaccessor to scalar.");
//     return *new Eigen::Tensor<double, 0>(); // bad memory mgmt (would leak) but will never be called. only to show compiler valid return.
//   }
//   Scalar &scalar() {return obj;}
//   Scalar &obj;
//   std::vector<int> intDims_;
// };

template<int nDim, typename Scalar>
Eigen::Tensor<Scalar, nDim> &ETaccessorBase::ref() {
  auto castptr = dynamic_cast<ETaccessor<Eigen::Tensor<Scalar, nDim> >* >(this);
  if(castptr == nullptr) Rcpp::stop("Problem creating a ref() from some form of access().\n");
  return castptr->innerRef();
}

// template<typename Scalar>
// Scalar &ETaccessorBase::scalar() {
//   auto castptr = dynamic_cast<ETaccessor<Scalar>* >(this);
//   if(castptr == nullptr) Rcpp::stop("Problem creating a scalar() from some form of access().\n");
//   return castptr->scalar();
// }

// template<typename Scalar, int nDim>
// auto access(Eigen::Tensor<Scalar, nDim> &x) -> ETaccessor<Eigen::Tensor<Scalar, nDim> >{
//   return ETaccessor<Eigen::Tensor<Scalar, nDim> >(x);
// }

template<typename T>
auto ETaccess(T &x) -> ETaccessor<T>{
  return ETaccessor<T>(x);
}

// end ETaccess

#endif // NCOMPILER_ETACCESSOR_POST_RCPP_H_
