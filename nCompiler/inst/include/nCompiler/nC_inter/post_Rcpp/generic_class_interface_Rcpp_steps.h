#ifndef GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_
#define GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_

#include<nCompiler/ET_Rcpp_ext/post_Rcpp/ETaccessor_post_Rcpp.h>

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
      // As a result, it is simpler here to create a new P object via this pathway.
      if constexpr(P_is_shared_ptr) {
        Rcpp::RObject Rextptr = get_extptr_from_SEXP(Svalue);
        SEXP Sextptr = Rextptr;
        if(Sextptr != R_NilValue) {
          // Use the regular Exporter pathway for non-shared_ptr types
          dynamic_cast<T*>(intBasePtr)->*ptr = P(typename Rcpp::traits::input_parameter<P>::type(Sextptr));
          return;
        }
        // If Svalue is not an external pointer, try to set values from list or environment
        //         Rprintf("trying to use set all values\n");
        auto casted_T = dynamic_cast<T*>(intBasePtr);
        auto& ptr2 = casted_T->*ptr;
        if(ptr2 != nullptr) {
          //           Rprintf("its not null\n");
          ptr2->set_all_values(Svalue);
        } else {
          if constexpr(std::is_default_constructible_v<typename P::element_type>) {
            casted_T->*ptr =  std::make_shared<typename P::element_type>();
            //   auto& ptr3 = casted_T->*ptr;
            (casted_T->*ptr)->set_all_values(Svalue);
          } else {
            Rcpp::stop("Trying to set values of an uninitialized compiled nClass (with no default constructor!) from a list or environment.");
          }
        }
      } else {
        dynamic_cast<T*>(intBasePtr)->*ptr = P(typename Rcpp::traits::input_parameter<P>::type(Svalue));
      }
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

  // For a list input, checking names in the list is costly
  // so we iterate through the list and check names against name2access.
  void set_all_values_impl_list(const Rcpp::List Robj) {
    // Cache names once to avoid repeatedly constructing the names vector
    if(!Robj.length()) return;
    Rcpp::Nullable<Rcpp::CharacterVector> nmsN = Robj.names();
    if(nmsN.isNull()) {
      Rcpp::stop("Setting multiple values of an nClass from a list requires that the list have names.\n");
    }
    Rcpp::CharacterVector nms(nmsN.get());
    for(int i = 0; i < Robj.length(); ++i) {
      // Safely extract the i-th name from the cached names vector
      std::string name = Rcpp::as<std::string>(nms[i]);
      name2access_type::iterator access = name2access.find(name);
      if(access == name2access.end()) continue;
      SEXP Svalue = Robj[i];
      access->second->set(this, Svalue);
    }
  }

  // For an environment input, checking names is less costly
  // so we iterate through name2access and check for each name
  // whether it exists in the environment.
  void set_all_values_impl_environment(const Rcpp::Environment Robj) {
    size_t n = name2access.size();
    auto i_n2a = name2access.begin();
    auto end_n2a = name2access.end();
    for(; i_n2a != end_n2a; ++i_n2a) {
      if(Robj.exists(i_n2a->first)) {
        SEXP Svalue = Robj.get(i_n2a->first);
        i_n2a->second->set(this, Svalue);
      }
    }
  }

  void set_all_values(SEXP Robj) {
    if(Rcpp::is<Rcpp::Environment>(Robj)) {
      set_all_values_impl_environment(Robj);
    } else if(Rcpp::is<Rcpp::List>(Robj)) {
      set_all_values_impl_list(Robj);
    } else {
      Rcpp::stop("Setting all values of an nClass only works from environment (including nClass or R6) or list objects.\n");
    }    
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
      return nullptr;
    }
    return (access->second->ETaccess(this));
  }

  std::shared_ptr<genericInterfaceBaseC> get_interface_ptr(const std::string &name) {
    name2access_type::iterator access = name2access.find(name);
    if(access == name2access.end()) {
      std::cout<<"Problem: \""<<name<<"\" is not a field in this nClass."<<std::endl;
      return nullptr;
    }
    return (access->second->getInterfacePtr(this));
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

// Pointer to a single element of a named field of obj, addressed by a
// multi-index (one entry per raw dimension of the field, e.g.
// Eigen::Tensor<int, 1>), 0-based unless subtract_ones is set (R callers
// pass 1-based indices; subtract_ones folds the -1 into the same pass that
// already walks inds for bounds-checking, rather than copying/mutating inds
// or teaching RuntimeFlatView about 1-based indexing).
// obj->access(var) is only used to locate the field's data pointer and
// shape; the returned pointer is into the field's own storage and stays
// valid for as long as obj does (the accessor itself is a temporary, not
// the owner of that storage).
template<typename Scalar = double, typename IndsT>
Scalar* make_scalarNodePtr(const std::shared_ptr<genericInterfaceBaseC> &obj,
                           const std::string &var,
                           const IndsT &inds,
                           bool subtract_ones = false) {
  auto acc = obj->access(var);
  if (!acc)
    Rcpp::stop("make_scalarNodePtr: field \"" + var + "\" not found.");
  auto view = acc->flatten<Scalar>();
  const RuntimeSubviewInfo &info = view.info();
  const std::vector<long> &sizes = info.sizes;
  if (static_cast<size_t>(inds.size()) != sizes.size())
    Rcpp::stop("make_scalarNodePtr: inds has " + std::to_string(inds.size()) +
               " entries but field \"" + var + "\" has " + std::to_string(sizes.size()) +
               " dimensions.");
  const long origin = subtract_ones ? 1 : 0;
  long offset = info.baseOffset;
  for (size_t k = 0; k < sizes.size(); ++k) {
    const long idx = static_cast<long>(inds[k]) - origin;
    if (idx < 0 || idx >= sizes[k])
      Rcpp::stop("make_scalarNodePtr: index " + std::to_string(inds[k]) +
                 " out of range for dimension " + std::to_string(k) +
                 " of field \"" + var + "\" (size " + std::to_string(sizes[k]) + ").");
    offset += idx * info.strides[k];
  }
  return view.data() + offset;
}

// Shared by make_nodeSTM and rebind_nodeSTM: resolves obj's named field and
// inds selection into the (data pointer, native dims, per-dimension b__
// blocks) a StridedTensorMap needs, either to construct one (make_nodeSTM)
// or to rebind an existing one in place (rebind_nodeSTM). intDims is a real
// copy (not a reference into the accessor), since acc -- and its own
// intDims() storage -- goes out of scope when this function returns; data
// remains valid regardless, because it points into the field's own storage
// in obj, not into the accessor.
//
// inds is a column-major (nDim x 2) matrix-like container (any type
// supporting operator()(row, col), e.g. Eigen::Tensor<int, 2>), one row per
// raw dimension of the field, giving [start, stop] for that dimension:
//   - both columns missing (R's NA or negative)  -> whole dimension (kept)
//   - only the stop column missing                -> single index at start
//                                                     (drops this dimension)
//   - both given (may be equal)                   -> range [start, stop]
//                                                     (kept; extent is 1
//                                                     when start == stop,
//                                                     the dimension is not
//                                                     dropped)
// subtract_ones converts from R's 1-based indices (subtracted only from
// non-missing values, after the missing/negative check).
//
// The number of kept (non-dropped) dimensions must equal output_nDim: this
// is checked explicitly before construction, because createSubTensorInfoGeneral
// fills a fixed-size Eigen::array<long, output_nDim> by counting kept
// dimensions as it walks ss, and silently leaves slots uninitialized (rather
// than erroring) if that count doesn't match output_nDim.
template<typename Scalar>
struct nodeSTM_spec {
  Scalar *data;
  std::vector<int> intDims;
  std::vector<b__> ss;
};

template<int output_nDim, typename Scalar, typename IndsT>
nodeSTM_spec<Scalar>
resolve_nodeSTM_spec(const std::shared_ptr<genericInterfaceBaseC> &obj,
                      const std::string &var,
                      const IndsT &inds,
                      bool subtract_ones) {
  auto acc = obj->access(var);
  if (!acc)
    Rcpp::stop("make_nodeSTM: field \"" + var + "\" not found.");
  nodeSTM_spec<Scalar> spec;
  spec.data = acc->template S<Scalar>().data();
  spec.intDims = acc->intDims(); // copy: acc (and its intDims() storage) won't outlive this function
  const size_t nDim = spec.intDims.size();
  const long origin = subtract_ones ? 1 : 0;

  spec.ss.reserve(nDim);
  int nKept = 0;
  for (size_t k = 0; k < nDim; ++k) {
    const long rawStart = static_cast<long>(inds(k, 0));
    const long rawStop  = static_cast<long>(inds(k, 1));
    const bool startMissing = (rawStart == NA_INTEGER || rawStart < 0);
    const bool stopMissing  = (rawStop  == NA_INTEGER || rawStop  < 0);
    if (startMissing && stopMissing) {
      spec.ss.emplace_back(); // whole dimension
      ++nKept;
    } else if (startMissing) {
      Rcpp::stop("make_nodeSTM: start is missing but stop is given, in dimension " +
                 std::to_string(k) + " of field \"" + var + "\".");
    } else if (stopMissing) {
      const long idx = rawStart - origin;
      if (idx < 0 || idx >= spec.intDims[k])
        Rcpp::stop("make_nodeSTM: single index " + std::to_string(rawStart) +
                   " out of range in dimension " + std::to_string(k) +
                   " of field \"" + var + "\" (size " + std::to_string(spec.intDims[k]) + ").");
      spec.ss.emplace_back(idx); // single index: drops this dimension
    } else {
      const long start = rawStart - origin;
      const long stop  = rawStop - origin;
      if (start < 0 || stop >= spec.intDims[k] || start > stop)
        Rcpp::stop("make_nodeSTM: range [" + std::to_string(rawStart) + ", " +
                   std::to_string(rawStop) + "] out of range in dimension " +
                   std::to_string(k) + " of field \"" + var + "\" (size " +
                   std::to_string(spec.intDims[k]) + ").");
      spec.ss.emplace_back(start, stop); // range, kept even if start == stop
      ++nKept;
    }
  }
  if (nKept != output_nDim)
    Rcpp::stop("make_nodeSTM: selection keeps " + std::to_string(nKept) +
               " dimension(s) but output_nDim is " + std::to_string(output_nDim) +
               " for field \"" + var + "\".");
  return spec;
}

// StridedTensorMap view over a (possibly strided, possibly rank-reducing)
// subview of a named field of obj, with the output rank output_nDim fixed
// at compile time. Sibling to make_scalarNodePtr for the multi-element case.
// See resolve_nodeSTM_spec above for the meaning of inds and subtract_ones.
template<int output_nDim, typename Scalar = double, typename IndsT>
Eigen::StridedTensorMap<Eigen::Tensor<Scalar, output_nDim>>
make_nodeSTM(const std::shared_ptr<genericInterfaceBaseC> &obj,
             const std::string &var,
             const IndsT &inds,
             bool subtract_ones = false) {
  auto spec = resolve_nodeSTM_spec<output_nDim, Scalar>(obj, var, inds, subtract_ones);
  return Eigen::StridedTensorMap<Eigen::Tensor<Scalar, output_nDim>>(spec.data, spec.intDims, spec.ss);
}

// Rebinds an existing (persistent) StridedTensorMap member in place, e.g. one
// built once via a default-constructed, empty StridedTensorMap and bound here
// before millions of repeated accesses. See resolve_nodeSTM_spec above for
// the meaning of inds and subtract_ones.
template<int output_nDim, typename Scalar = double, typename IndsT>
void rebind_nodeSTM(Eigen::StridedTensorMap<Eigen::Tensor<Scalar, output_nDim>> &target,
                    const std::shared_ptr<genericInterfaceBaseC> &obj,
                    const std::string &var,
                    const IndsT &inds,
                    bool subtract_ones = false) {
  auto spec = resolve_nodeSTM_spec<output_nDim, Scalar>(obj, var, inds, subtract_ones);
  target.rebind(spec.data, spec.intDims, spec.ss);
}

#endif // GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_
