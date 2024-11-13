#ifndef __NCOMPILER_CLASS_INTERFACE
#define __NCOMPILER_CLASS_INTERFACE

//#include<Rcpp.h>
#include<map>
#include<string>
#include<utility>
#include<Rinternals.h>
#include<iostream>
#include<memory>
#ifdef NCOMPILER_USES_CEREAL
#include<nCompiler/nClass_cereal/archives.h>
#endif
#include<nCompiler/nClass_interface/expand_call_method.h>
// We must be able to assume that any Rcpp extension
// forward declarations have been made by now.
//#include<nCompiler/shared_ptr_as_wrap.h>

// #include<nCompiler/loadedObjectEnv.h>

// These avoid the problem that a macro argument like "fields" below
// can't contain comma-separated elements.
#define NCOMPILER_FIELDS(...) { __VA_ARGS__ }
#define NCOMPILER_METHODS(...) { __VA_ARGS__ }

// Some options for verbose output:
// #define SHOW_DESTRUCTORS
// #define SHOW_FIELDS
// #define SHOW_METHODS

#define NCOMPILER_INTERFACE(name, fields, methods)\
  template <>\
  int genericInterfaceC<name>::name_count = 0;	\
  template<>\
  genericInterfaceC<name>::name2index_type genericInterfaceC<name>::name2index {};\
  template<>\
  genericInterfaceC<name>::name2access_type genericInterfaceC<name>::name2access \
  fields\
  ;\
  template<>\
  genericInterfaceC<name>::name2method_type genericInterfaceC<name>::name2method \
    methods\
 ;

// Base class for interfaces to nimble classes
class genericInterfaceBaseC {
 public:
  // return a named member converted to a SEXP.
  // Derived classes should provide valid implementations.
  virtual SEXP get_value(const std::string &name) const {
    std::cout<<"Error: you should be in a derived genericInterfaceC class for get_value"<<std::endl;
    return R_NilValue;
  }
  virtual void set_value(const std::string &name, SEXP Svalue) {
    std::cout<<"Error: you should be in a derived genericInterfaceC class for set_value"<<std::endl;
  }
  virtual SEXP call_method(const std::string &name, SEXP Sargs) {
    std::cout<<"Error: you should be in a derived genericInterfaceC class for call_method"<<std::endl;
    return R_NilValue;
  }
  virtual SEXP make_deserialized_return_SEXP() {
    std::cout<<"In  base class make_deserialized_return_SEXP"<<std::endl;
    return R_NilValue;
  }

#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {}
#endif
  virtual ~genericInterfaceBaseC() {
#ifdef SHOW_DESTRUCTORS
    std::cout<<"In genericInterfaceBaseC destructor"<<std::endl;
#endif
  }

  enum argPassingType{copy, ref, refBlock};
  struct arg {
    const std::string name;
    const argPassingType APtype;
    arg(const std::string &name_, argPassingType type_=copy) :
    name(name_),
      APtype(type_) {}
  };
  struct args {
    typedef std::vector<const arg> argVectorT;
    // explicit saves the compiler from giving ambiguous
    // constructor error from implicit copy and move constructors.
    // I am not sure if this is the right way to resolve the issue.
    const argVectorT argVector;
    explicit args(const argVectorT &argVector_ ) :
    argVector(argVector_) {};
  };
};

// A forward declaration.
SEXP process_call_args(const genericInterfaceBaseC::args::argVectorT &argVector,
                       SEXP Sargs);

// Base class for accessing a single member from a nimble class,
// converted to SEXP.
//
// Derived classes have pointers to members of derived interface classes.
class accessor_base {
 public:
  // return the member from the interface object, converted to SEXP.
  // Derived classes should provide valid implementation.
  virtual SEXP get(const genericInterfaceBaseC *) const {
    std::cout<<"Error: you should be in get for a derived accessor class"<<std::endl;
    return R_NilValue;
  };
  virtual void set(genericInterfaceBaseC *, SEXP Svalue) {
    std::cout<<"Error: you should be in set for a derived accessor class"<<std::endl;
  };
  virtual ~accessor_base(){}
};

// Base class for calling a single method from a nimble class,
// with answer converted to SEXP.
class method_base {
 public:
  virtual SEXP call(genericInterfaceBaseC *, SEXP Sargs) {
    std::cout<<"Error: you should be in a derived method class"<<std::endl;
    return R_NilValue;
  };
  virtual ~method_base(){}
};


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
      return Rcpp::wrap(reinterpret_cast<const T*>(intBasePtr)->*ptr);
    }
    void set(genericInterfaceBaseC *intBasePtr, SEXP Svalue) {
#ifdef SHOW_FIELDS
      std::cout<<"in derived set"<<std::endl;
#endif
      //      reinterpret_cast<T*>(intBasePtr)->*ptr = Rcpp::as<P>(Svalue);
      // Originally we defined an Rcpp::Exporter specialization as needed,
      // which is called via as<>.  However, we gain more flexibility in
      // argument passing by defining new Rcpp::traits::input_parameter specializations.
      // As a result, it is simpler her to create a new P object via this pathway.
      reinterpret_cast<T*>(intBasePtr)->*ptr = P(typename Rcpp::traits::input_parameter<P>::type(Svalue));
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

  template<typename P, typename ...ARGS>
    class method_class : public method_base {
  public:
    typedef P (T::*ptrtype)(ARGS...);
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
                        expand_call_method_narg<P, T>::template call<ptrtype, ARGS...>(reinterpret_cast<T*>(intBasePtr), ptr, Sargs)
                        );
    }
  };

  /* Partial specialization on void return type avoids Rcpp::wrap<void>, which doesn't work. */
  /* There might be a slightly more compact way to refactor just the Rcpp::wrap step, but */
  /* this is a quick and simple solution:*/
  template<typename ...ARGS>
    class method_class<void, ARGS...> : public method_base {
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
      expand_call_method_narg<void, T>::template call<ptrtype, ARGS...>(reinterpret_cast<T*>(intBasePtr), ptr, Sargs);
      return R_NilValue;
    }
  };

//  typedef std::map<std::string, std::shared_ptr<method_base> > name2method_type;
//  typedef std::pair<std::string, std::shared_ptr<method_base> > name_method_pair;

  static name2method_type name2method;
  template<typename P,  typename ...ARGS>
    static name_method_pair method(std::string name,
                                   P (T::*fun)(ARGS... args),
                                   const args& args_) {
#ifdef SHOW_METHODS
    std::cout<<"adding method "<<name<<std::endl;
#endif
    return
      name_method_pair(name,
                       method_info(std::shared_ptr<method_base>(new method_class<P, ARGS...>(fun)), args_)
                       );
  }
#ifdef NCOMPILER_USES_CEREAL
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {
    archive(cereal::base_class<genericInterfaceBaseC>(this));
  }
#endif
};


/* // From here down has been turned into macros above. */
/* // This example uses the input name fooC */
/* // followed by "field" and "method" entries. */
/* template<> */
/* int genericInterfaceC<fooC>::name_count = 0; */

/* template<> */
/* genericInterfaceC<fooC>::name2index_type genericInterfaceC<fooC>::name2index {}; */

/* template<> */
/* genericInterfaceC<fooC>::name2access_type genericInterfaceC<fooC>::name2access { */
/*   field("x", &fooC::x), */
/*     field("y", &fooC::y) */
/* }; */


/* template<> */
/* genericInterfaceC<fooC>::name2method_type genericInterfaceC<fooC>::name2method { */
/*     method("print_val", &fooC::print_val) */
/* }; */

#endif
