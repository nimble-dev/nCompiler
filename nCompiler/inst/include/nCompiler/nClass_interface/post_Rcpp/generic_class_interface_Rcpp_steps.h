#ifndef GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_
#define GENERIC_CLASS_INTERFACE_RCPP_STEPS_H_

// Interface to class T.
template<class T>
class genericInterfaceC : public genericInterfaceBaseC {
 public:
  ~genericInterfaceC() {
#ifdef SHOW_DESTRUCTORS
  std::cout<<"In derived genericInterfaceC destructor"<<std::endl;
#endif
  }
  // interface to a member of type P in class T
  template<typename P>
    class accessor_class : public accessor_base {
  public:
   typedef P T::*ptrtype;
    ptrtype ptr;
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
  };

 // static maps from character names
 static int name_count;
 typedef std::map<std::string,int> name2index_type;
 static name2index_type name2index;

  typedef std::map<std::string, std::shared_ptr<accessor_base> > name2access_type;
  typedef std::pair<std::string, std::shared_ptr<accessor_base> > name_access_pair;
  static name2access_type name2access;

  // Enter a new (name, member ptr) pair to static maps.
  template<typename P>
    static name_access_pair field(std::string name, P T::*ptr) {
#ifdef SHOW_FIELDS
    std::cout<<"adding "<<name<<std::endl;
#endif
    name2index[name] = name_count++;
    return name_access_pair(
                            name,
                            std::shared_ptr<accessor_base>(new accessor_class<P>(ptr))
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
    if(TYPEOF(Sargs) != ENVSXP)
      Rcpp::stop("nCompiler call_method should pass the calling environment.\n");
    SEXP SinnerArgs = PROTECT(process_call_args(method->second.my_args.argVector, Sargs));
    SEXP Sans = PROTECT(method->second.method_ptr->call(this, SinnerArgs));
    UNPROTECT(2);
    return Sans;
  }

  template<typename P, bool use_const=false, typename ...ARGS>
    class method_class : public method_base {
  public:
    using ptrtype = typename std::conditional<use_const, P (T::*)(ARGS...) const, P (T::*)(ARGS...)>::type;
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
  template<bool use_const, typename ...ARGS>
    class method_class<void, use_const, ARGS...> : public method_base {
  public:
    typedef void (T::*ptrtype)(ARGS...);
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
  template<typename P,  typename ...ARGS>
    static name_method_pair method(std::string name,
                                   P (T::*fun)(ARGS... args),
                                   const args& args_) {
#ifdef SHOW_METHODS
    std::cout<<"adding method "<<name<<std::endl;
#endif
    return
      name_method_pair(name,
                       method_info(std::shared_ptr<method_base>(new method_class<P, false, ARGS...>(fun)), args_)
                       );
  }
  // overload name_method_pair for const method
  template<typename P,  typename ...ARGS>
    static name_method_pair method(std::string name,
                                   P (T::*fun)(ARGS... args) const,
                                   const args& args_) {
#ifdef SHOW_METHODS
    std::cout<<"adding (const) method "<<name<<std::endl;
#endif
    return
      name_method_pair(name,
                       method_info(std::shared_ptr<method_base>(new method_class<P, true, ARGS...>(fun)), args_)
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
