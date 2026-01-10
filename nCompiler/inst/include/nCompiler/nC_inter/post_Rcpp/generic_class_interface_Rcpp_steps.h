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
