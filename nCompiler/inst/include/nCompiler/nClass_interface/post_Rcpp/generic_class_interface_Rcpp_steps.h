#ifndef GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_
#define GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_

#include<nCompiler/EigenTensor_Rcpp_extensions/post_Rcpp/ETaccessor_post_Rcpp.h>

// begin ETaccess scheme
// If this works, I'll move it to its own header file.

// template<typename Scalar>
// class ETaccessorTyped;

// // Virtual nDim-general methods (e.g. resize, conversions to and from SEXP).
// class ETaccessorBase {
//   public:
// //  virtual void resize(Eigen::Tensor<double, 1> &t)=0;
//   // To iron out: set, get, generic ref access.
//   virtual void set(SEXP Sinput)=0;
//   virtual SEXP get()=0;
//   virtual SEXP operator=(SEXP RHS) {set(RHS); return RHS;}

//   virtual std::vector<int> &intDims()=0;

//   template<int nDim, typename Scalar>
//   using ETM = Eigen::TensorMap<Eigen::Tensor<Scalar, nDim> >;

//   template<typename Scalar = double>
//   ETaccessorTyped<Scalar> &S() {
//     auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
//     if(castptr == nullptr) Rcpp::stop("Problem with some form of access()\n.");
//     return *castptr;
//   }

//   template<int nDim, typename Scalar = double>
//   ETM<nDim, Scalar> map();

//   template<int nDim, typename Scalar = double>
//   Eigen::Tensor<Scalar, nDim> &ref();

//   template<typename Scalar = double>
//   Scalar &scalar();

//   virtual ~ETaccessorBase(){};
// };

// template<typename Scalar>
// class ETaccessorTyped : public ETaccessorBase {
//   public:

//   virtual Scalar *data()=0;

//   template<int nDim>
//   using ETM = Eigen::TensorMap<Eigen::Tensor<Scalar, nDim> >;

//   Scalar &scalarTyped() {
//     const auto intDims_ = this->intDims();
//     for(size_t i = 0; i < intDims_.size(); ++i) {
//       if(intDims_[i]!=1)
//         Rcpp::stop("Invalid call to scalar() for ETaccessor with dimensions not all equal to 1.");
//     }
//     return *data();
//   }

//   template<int output_nDim>
//   ETM<output_nDim> mapTyped() {
//     //innate_nDim is the nDim of the object.
//     //output_nDim is the requested nDim of the output map.
//     //If the output_nDim > innate_nDim, then set the output dims to innate dims padded with 1s.
//     //If the output_nDim < innate_nDim, then drop singleton dimensions in the innate dims.
//     //This is very similar to checkAndSetupDims in tensorFlex.h
//     //but there both the LHS and RHS nDims are known at compile time.
//     //Here only the output_nDim is known at compile time.
//     //Also it looks like in checkAndSetupDims, RHS singletons are always dropped
//     typedef typename Eigen::internal::traits<ETM<output_nDim> >::Index Index;
//     typedef typename ETM<output_nDim>::Dimensions output_Dimensions;
//     output_Dimensions outDim;
//     const auto intDims_ = this->intDims();
//     size_t innate_nDim = intDims_.size();
//     if(output_nDim >= innate_nDim) {
//       for(size_t i = 0; i < innate_nDim; ++i)
//         outDim[i] = intDims_[i];
//       if(output_nDim > innate_nDim) {
//         for(size_t i = innate_nDim; i < output_nDim; ++i)
//           outDim[i] = 1;
//       }
//     } else {
//       size_t i_out = 0;
//       for(size_t i_innate = 0 ; i_innate < innate_nDim; ++i_innate) {
//         if(intDims_[i_innate] > 1) {
//           if(i_out >= output_nDim) {
//             Rcpp::stop("Problem making a TensorMap from some form of access(): Too many non-singleton dimensions for the requested map dimensions.\n");
//             break;
//           } else {
//             outDim[i_out++] = intDims_[i_innate];
//           }
//         }
//       }
//       for( ; i_out < output_nDim; ++i_out ) outDim[i_out]=1;
//     }
//     return ETM<output_nDim>(data(), outDim);
//   }
//   ~ETaccessorTyped(){};
// };

// template<int nDim, typename Scalar>
// Eigen::TensorMap<Eigen::Tensor<Scalar, nDim> > ETaccessorBase::map() {
//   auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
//   if(castptr == nullptr) Rcpp::stop("Problem creating a map() from some form of access().\n");
//   return castptr->template mapTyped<nDim>();
// }

// template<typename Scalar>
// Scalar& ETaccessorBase::scalar() {
//   auto castptr = dynamic_cast<ETaccessorTyped<Scalar>* >(this);
//   if(castptr == nullptr) Rcpp::stop("Problem using scalar() from some form of access().\n");
//   return castptr->scalarTyped();
// }

// // default to throwing an error
// // then specialize to allow valid types (Eigen::Tensor's or true scalars)
// // These are supported as run-time errors because the genericInterfaceC
// // will access them by a name.
// template<typename ERROR>
// class ETaccessor : public ETaccessorTyped<double> {
//   public:
//   using ET = Eigen::Tensor<double, 0>;
//   // I think to compile this all needs to be valid in terms of types but throw run-time errors everywhere.
//   // It should never get past the constructor because that throws an error, but other errors are written
//   //   for good measure.
//   ETaccessor(ERROR &obj_) { Rcpp::stop("Invalid use of some form of access(). You probably tried to access() a non-numeric object.\n"); }
//   ~ETaccessor() {};
//   double *data() override {
//     Rcpp::stop("Invalid call to data() for invalid ETaccessor.");
//     return nullptr;
//   }
//   std::vector<int> &intDims() override {
//     Rcpp::stop("Invalid call to intDims() for invalid ETaccessor.");
//     return intDims_;
//   }
//   void set(SEXP Sinput) override {
//     Rcpp::stop("Invalid call to set() for invalid ETaccessor.");
//   }
//   SEXP get() override {
//     Rcpp::stop("Invalid call to get() for invalid ETaccessor.");
//     return R_NilValue;
//   }
//   ET &innerRef() {
//     Rcpp::stop("Invalid call to ref() for invalid ETaccessor.");
//     return obj;
//   }
//   double &scalar() {
//     Rcpp::stop("Invalid call to scalar() for invalid ETaccessor.");
//     return *new double(0.); // would leak memory but will never be reached and may reduce compiler warnings
//   }
//   ET obj;
//   std::vector<int> intDims_;
// };


// template<typename Scalar, int nDim>
// class ETaccessor<Eigen::Tensor<Scalar, nDim> > : public ETaccessorTyped<Scalar> {
//   public:
//   using ET = Eigen::Tensor<Scalar, nDim>;
//   // using Scalar = typename ET::Scalar;
//   typedef typename Eigen::internal::traits<ET>::Index Index;
//   // NumIndices should match nDim, so this is a bit redundant.
//   static const Index NumIndices = ET::NumIndices; // StridedTensorMap: This is output number of dimensions (indices).
//   typedef typename ET::Dimensions Dimensions;
//   ETaccessor(ET &obj_) : obj(obj_), intDims_(NumIndices) {};
//   ~ETaccessor() {};
//   Scalar *data() override {return obj.data();}
//   std::vector<int> &intDims() override {
//     Dimensions dim = obj.dimensions();
//     std::copy(dim.begin(), dim.end(), intDims_.begin());
//     return intDims_;
//   }
//   void set(SEXP Sinput) override {
//     obj = as<ET>(Sinput);
//   }
//   SEXP get() override {
//     return wrap(obj);
//   }
//   ET &innerRef() {return obj;}
//   // Scalar &scalar() {
//   //   Dimensions dim = obj.dimensions();
//   //   for(int i = 0; i < nDim; ++i) {
//   //     if(dim[i]!=1)
//   //       Rcpp::stop("Invalid call to scalar() for ETaccessor with dimensions not all equal to 1.");
//   //   }
//   //   return *obj.data(); // would leak memory but will never be reached and may reduce compiler warnings
//   // }
//   ET &obj;
//   std::vector<int> intDims_;
// };

// template<typename Scalar>
// class ETaccessorScalar : public ETaccessorTyped<Scalar> {
//   public:
//   ETaccessorScalar(Scalar &obj_) : obj(obj_) {};
//   ~ETaccessorScalar() {};
//   Scalar *data() override {return &obj;}
//   std::vector<int> &intDims() override {return intDims_;}
//   void set(SEXP Sinput) override { obj = as<Scalar>(Sinput);}
//   SEXP get() override {return wrap(obj);}
//   Eigen::Tensor<double, 0> &innerRef() {
//     Rcpp::stop("Invalid call to ref() for ETaccessor to scalar.");
//     return *new Eigen::Tensor<double, 0>(); // bad memory mgmt (would leak) but will never be called. only to show compiler valid return.
//   }
//   //Scalar &scalar() {return obj;}
//   Scalar &obj;
//   std::vector<int> intDims_;
// };

// template<>
// class ETaccessor<double> : public ETaccessorScalar<double> {
//   public:
//   ETaccessor(double &obj_) : ETaccessorScalar(obj_) {};
//   ~ETaccessor() {};
// };

// // // CppAD header is not read by here, so this needs attention.
// // template<>
// // class ETaccessor<CppAD::AD<double> > : public ETaccessorScalar<CppAD::AD<double> > {
// //   public:
// //   ETaccessor(CppAD::AD<double> &obj_) : ETaccessorScalar(obj_) {};
// //   ~ETaccessor() {};
// // };

// template<>
// class ETaccessor<int> : public ETaccessorScalar<int> {
//   public:
//   ETaccessor(int &obj_) : ETaccessorScalar(obj_) {};
//   ~ETaccessor() {};
// };

// template<>
// class ETaccessor<bool> : public ETaccessorScalar<bool> {
//   public:
//   ETaccessor(bool &obj_) : ETaccessorScalar(obj_) {};
//   ~ETaccessor() {};
// };

// // template<>
// // class ETaccessor<double> : public ETaccessorTyped<double> {
// //   public:
// //   using Scalar = double;

// //   ETaccessor(Scalar &obj_) : obj(obj_) {};
// //   ~ETaccessor() {};
// //   Scalar *data() override {return &obj;}
// //   std::vector<int> &intDims() override {return intDims_;}
// //   void set(SEXP Sinput) override { obj = as<Scalar>(Sinput);}
// //   SEXP get() override {return wrap(obj);}
// //   Eigen::Tensor<double, 0> &ref() {
// //     Rcpp::stop("Invalid call to ref() for ETaccessor to scalar.");
// //     return *new Eigen::Tensor<double, 0>(); // bad memory mgmt (would leak) but will never be called. only to show compiler valid return.
// //   }
// //   Scalar &scalar() {return obj;}
// //   Scalar &obj;
// //   std::vector<int> intDims_;
// // };

// template<int nDim, typename Scalar>
// Eigen::Tensor<Scalar, nDim> &ETaccessorBase::ref() {
//   auto castptr = dynamic_cast<ETaccessor<Eigen::Tensor<Scalar, nDim> >* >(this);
//   if(castptr == nullptr) Rcpp::stop("Problem creating a ref() from some form of access().\n");
//   return castptr->innerRef();
// }

// // template<typename Scalar>
// // Scalar &ETaccessorBase::scalar() {
// //   auto castptr = dynamic_cast<ETaccessor<Scalar>* >(this);
// //   if(castptr == nullptr) Rcpp::stop("Problem creating a scalar() from some form of access().\n");
// //   return castptr->scalar();
// // }

// // template<typename Scalar, int nDim>
// // auto access(Eigen::Tensor<Scalar, nDim> &x) -> ETaccessor<Eigen::Tensor<Scalar, nDim> >{
// //   return ETaccessor<Eigen::Tensor<Scalar, nDim> >(x);
// // }

// template<typename T>
// auto ETaccess(T &x) -> ETaccessor<T>{
//   return ETaccessor<T>(x);
// }

// maybe put these inside the class or namespace.
template<typename T>
struct is_shared_ptr : std::false_type {};

template<typename U>
struct is_shared_ptr<std::shared_ptr<U>> : std::true_type {};
// // end ETaccess

template<typename T>
struct shared_ptr_element_type {using type = void;};

template<typename U>
struct shared_ptr_element_type<std::shared_ptr<U>> {using type = U;};

// Interface to class T.
template<class T>
class genericInterfaceC : virtual public genericInterfaceBaseC {
 public:
  ~genericInterfaceC() {
#ifdef SHOW_DESTRUCTORS
  std::cout<<"In derived genericInterfaceC destructor"<<std::endl;
#endif
  }
  // interface to a member of type P in class T
  template<typename P, typename T2>
    class accessor_class : public accessor_base {
  public:
   typedef P T2::*ptrtype; // T2 will only be T or a base class of T.
    ptrtype ptr;
    static constexpr bool P_is_shared_ptr = is_shared_ptr<P>::value;
    using shared_ptr_element = typename shared_ptr_element_type<P>::type;
    static constexpr bool shared_ptr_element_is_polymorphic = std::is_polymorphic_v<shared_ptr_element>;

    accessor_class(ptrtype ptr) : ptr(ptr) {};
    SEXP get(const genericInterfaceBaseC *intBasePtr) const {
#ifdef SHOW_FIELDS
      std::cout<<"in derived get"<<std::endl;
#endif
      return Rcpp::wrap(dynamic_cast<const T*>(intBasePtr)->*ptr);
    }
    void set(genericInterfaceBaseC *intBasePtr, SEXP Svalue) {
#ifdef SHOW_FIELDS
      std::cout<<"in derived set"<<std::endl;
#endif
      //      dynamic_cast<T*>(intBasePtr)->*ptr = Rcpp::as<P>(Svalue);
      // Originally we defined an Rcpp::Exporter specialization as needed,
      // which is called via as<>.  However, we gain more flexibility in
      // argument passing by defining new Rcpp::traits::input_parameter specializations.
      // As a result, it is simpler her to create a new P object via this pathway.
      dynamic_cast<T*>(intBasePtr)->*ptr = P(typename Rcpp::traits::input_parameter<P>::type(Svalue));
    }
    std::unique_ptr<ETaccessorBase> ETaccess(genericInterfaceBaseC *intBasePtr) {
      std::unique_ptr<ETaccessorBase> ans( new ETaccessor<P>( dynamic_cast<T*>(intBasePtr)->*ptr ) );
      return ans;
    }
    std::shared_ptr<genericInterfaceBaseC> getInterfacePtr(genericInterfaceBaseC *intBasePtr) {
      if constexpr(P_is_shared_ptr) {
        if constexpr (shared_ptr_element_is_polymorphic) {
          return std::dynamic_pointer_cast<genericInterfaceBaseC>(dynamic_cast<T*>(intBasePtr)->*ptr);
        } else {
          return std::static_pointer_cast<genericInterfaceBaseC>(dynamic_cast<T*>(intBasePtr)->*ptr);
        }
      }
      return nullptr;
    }
  };

 // static maps from character names
 static int name_count;
// typedef std::map<std::string,int> name2index_type;
 static name2index_type name2index;

  // typedef std::map<std::string, std::shared_ptr<accessor_base> > name2access_type;
  // typedef std::pair<std::string, std::shared_ptr<accessor_base> > name_access_pair;
  static name2access_type name2access;

  const name2access_type& get_name2access() const{
    return name2access;
  }

  // Enter a new (name, member ptr) pair to static maps.
  template<typename P, typename T2>
    static name_access_pair field(std::string name, P T2::*ptr) {
#ifdef SHOW_FIELDS
    std::cout<<"adding "<<name<<std::endl;
#endif
    name2index[name] = name_count++;
    return name_access_pair(
                            name,
                            std::shared_ptr<accessor_base>(new accessor_class<P, T2>(ptr))
                            );
      }

  // hello world to see if static maps were populated.
  void hw() {
    std::cout<<"HW "<<name_count <<std::endl;
  }

  // return a member as a SEXP, chosen by name.
  // This is called via
  // "SEXP get_value(SEXP Xptr, const std::string &name)"
  // In turn this calls derived get in accessor_class above
  SEXP get_value(const std::string &name) const {
#ifdef SHOW_FIELDS
    std::cout<<"in derived get_value"<<std::endl;
#endif
    name2access_type::const_iterator access = name2access.find(name);
    if(access == name2access.end())
      return R_NilValue;
    return (access->second->get(this));
  }

  void set_value(const std::string &name, SEXP Svalue ) {
#ifdef SHOW_FIELDS
    std::cout<<"in derived set_value"<<std::endl;
#endif
    name2access_type::iterator access = name2access.find(name);
    if(access == name2access.end()) {
      std::cout<<"Problem: \""<<name<<"\" is not a field in this nClass."<<std::endl;
      return;
    }
    access->second->set(this, Svalue);
  }

  std::unique_ptr<ETaccessorBase> access(const std::string &name) {
    name2access_type::iterator access = name2access.find(name);
    if(access == name2access.end()) {
      std::cout<<"Problem: \""<<name<<"\" is not a field in this nClass."<<std::endl;
    }
    return (access->second->ETaccess(this));
  }

  // SEXP& value(const std::string &name) {
  //   name2access_type::iterator access = name2access.find(name);
  //   access->second;
  // }

  /****** METHODS ******/
  struct method_info {
    // explicit saves the compiler from giving ambiguous
    // constructor error from implicit copy and move constructors.
    // I am not sure if this is the right way to resolve the issue.
    method_info(const std::shared_ptr<method_base>& method_ptr_,
                const args &args_) :
      my_args(args_),
      method_ptr(method_ptr_){};
    args my_args;
    std::shared_ptr<method_base> method_ptr;
  };
  // method_info needs a template argument, so this idea breaks.
  typedef std::map<std::string, method_info > name2method_type;
  typedef std::pair<std::string, method_info > name_method_pair;


  SEXP call_method(const std::string &name, SEXP Sargs) {
#ifdef SHOW_METHODS
    std::cout<<"in derived call_method"<<std::endl;
#endif
    typename name2method_type::iterator method = name2method.find(name);
    if(method == name2method.end()) {
      std::cout<<"Problem: \""<<name<<"\" is not a method in this nClass."<<std::endl;
      return R_NilValue;
    }
 //   if(TYPEOF(Sargs) != ENVSXP)
 //     Rcpp::stop("nCompiler call_method should pass the calling environment.\n");
 //   SEXP SinnerArgs = PROTECT(process_call_args(method->second.my_args.argVector, Sargs));
 //   SEXP Sans = PROTECT(method->second.method_ptr->call(this, SinnerArgs));
 //   UNPROTECT(2);
    SEXP Sans = PROTECT(method->second.method_ptr->call(this, Sargs));
    UNPROTECT(1);
return Sans;
  }

  template<typename P, typename T2, bool use_const=false, typename ...ARGS>
    class method_class : public method_base {
  public:
    using ptrtype = typename std::conditional<use_const, P (T2::*)(ARGS...) const, P (T2::*)(ARGS...)>::type;
    ptrtype ptr;
  method_class(ptrtype ptr) : ptr(ptr) {};

    SEXP call(genericInterfaceBaseC *intBasePtr, SEXP Sargs) {
#ifdef SHOW_METHODS
      std::cout<<"in derived call"<<std::endl;
#endif
      if(LENGTH(Sargs) != sizeof...(ARGS)) {
        std::cout<<"Incorrect number of arguments"<<std::endl;
        return R_NilValue;
      }
      return Rcpp::wrap(
                        expand_call_method_narg<P, T>::template call<ptrtype, ARGS...>(dynamic_cast<T*>(intBasePtr), ptr, Sargs)
                        );
    }
  };

  /* Partial specialization on void return type avoids Rcpp::wrap<void>, which doesn't work. */
  /* There might be a slightly more compact way to refactor just the Rcpp::wrap step, but */
  /* this is a quick and simple solution:*/
  template<bool use_const, typename T2, typename ...ARGS>
    class method_class<void, T2, use_const, ARGS...> : public method_base {
  public:
    typedef void (T2::*ptrtype)(ARGS...);
    ptrtype ptr;
  method_class(ptrtype ptr) : ptr(ptr) {};

    SEXP call(genericInterfaceBaseC *intBasePtr, SEXP Sargs) {
#ifdef SHOW_METHODS
      std::cout<<"in derived call"<<std::endl;
#endif
      if(LENGTH(Sargs) != sizeof...(ARGS)) {
        std::cout<<"Incorrect number of arguments"<<std::endl;
        return R_NilValue;
      }
      expand_call_method_narg<void, T>::template call<ptrtype, ARGS...>(dynamic_cast<T*>(intBasePtr), ptr, Sargs);
      return R_NilValue;
    }
  };

//  typedef std::map<std::string, std::shared_ptr<method_base> > name2method_type;
//  typedef std::pair<std::string, std::shared_ptr<method_base> > name_method_pair;

  static name2method_type name2method;
  // name_method_pair for non-const method
//   template<typename P,  typename ...ARGS>
//     static name_method_pair method(std::string name,
//                                    P (T::*fun)(ARGS... args),
//                                    const args& args_) {
// #ifdef SHOW_METHODS
//     std::cout<<"adding method "<<name<<std::endl;
// #endif
//     return
//       name_method_pair(name,
//                        method_info(std::shared_ptr<method_base>(new method_class<P, T, false, ARGS...>(fun)), args_)
//                        );
//   }

  template<typename P, typename T2, typename ...ARGS>
    static name_method_pair method(std::string name,
                                   P (T2::*fun)(ARGS... args),
                                   const args& args_) {
#ifdef SHOW_METHODS
    std::cout<<"adding method "<<name<<std::endl;
#endif
    return
      name_method_pair(name,
                       method_info(std::shared_ptr<method_base>(new method_class<P, T2, false, ARGS...>(fun)), args_)
                       );
  }


  // overload name_method_pair for const method
  template<typename P, typename T2, typename ...ARGS>
    static name_method_pair method(std::string name,
                                   P (T2::*fun)(ARGS... args) const,
                                   const args& args_) {
#ifdef SHOW_METHODS
    std::cout<<"adding (const) method "<<name<<std::endl;
#endif
    return
      name_method_pair(name,
                       method_info(std::shared_ptr<method_base>(new method_class<P, T2, true, ARGS...>(fun)), args_)
                       );
  }
#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {
    archive(cereal::base_class<genericInterfaceBaseC>(this));
  }
#endif
};


#endif // GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_
