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
#include<nCompiler/nC_cereal/archives.h>
#endif
#include<nCompiler/nC_inter/expand_call_method.h>
#include<nCompiler/ET_Rcpp_ext/ETaccessor.h>

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

class accessor_base;

// Base class for interfaces to nimble classes
class genericInterfaceBaseC {
 public:
  typedef std::map<std::string,int> name2index_type;
  typedef std::map<std::string, std::shared_ptr<accessor_base> > name2access_type;
  typedef std::pair<std::string, std::shared_ptr<accessor_base> > name_access_pair;

  virtual const name2access_type& get_name2access() const{
    std::cout<<"Error: you should be in a derived genericInterfaceC class for get_name2access"<<std::endl;
    // return something to avoid compiler warning
    static name2access_type dummy;
    return dummy;
  };

  virtual std::unique_ptr<ETaccessorBase> access(const std::string &name) {
    std::cout<<"Error: you should be in a derived genericInterfaceC class for access"<<std::endl;
    return std::unique_ptr<ETaccessorBase>(nullptr);
  }

  // return a named member that is itself a genericInterfaceBaseC-derived
  // (nClass) object, as a shared_ptr to its generic interface base.
  // Derived classes should provide valid implementations.
  virtual std::shared_ptr<genericInterfaceBaseC> get_interface_ptr(const std::string &name) {
    std::cout<<"Error: you should be in a derived genericInterfaceC class for get_interface_ptr"<<std::endl;
    return nullptr;
  }

  // return a named member converted to a SEXP.
  // Derived classes should provide valid implementations.
  virtual SEXP get_value(const std::string &name) const {
    std::cout<<"Error: you should be in a derived genericInterfaceC class for get_value"<<std::endl;
    return R_NilValue;
  }
  virtual void set_all_values(SEXP Robj) {
    std::cout<<"Error: you should be in a derived genericInterfaceC class for set_all_values"<<std::endl;
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
    typedef std::vector<arg> argVectorT;
    // explicit saves the compiler from giving ambiguous
    // constructor error from implicit copy and move constructors.
    // I am not sure if this is the right way to resolve the issue.
    const argVectorT argVector;
    explicit args(const argVectorT &argVector_ ) :
    argVector(argVector_) {};
  };
};

// Forward declaration needed for class_from_interface below
template<class T> class genericInterfaceC;

// Extracts ClassName from genericInterfaceC<ClassName>, used by interface_resolver
// to determine the owned type for nC_shared_from_this().
template<typename T> struct class_from_interface { using type = void; };
template<typename T> struct class_from_interface<genericInterfaceC<T>> { using type = T; };

// FirstDerived and interface_resolver<> designed with help from Google Gemini
// Helper template to find the first type that inherits from Base
template <typename T, typename... Rest>
struct FirstGenericDerived {
  using type = std::conditional_t<
      std::is_base_of_v<genericInterfaceBaseC, T>,
      T,
      typename FirstGenericDerived<Rest...>::type
  >;
};

// Base case for the recursive helper template
template <typename T>
struct FirstGenericDerived<T> {
  using type = std::conditional_t<
      std::is_base_of_v<genericInterfaceBaseC, T>,
      T,
      genericInterfaceBaseC
  >;
};

// General case (2+ template args): derived nClass.
// nC_shared_from_this() casts the inherited shared_from_this() result to
// shared_ptr<OwnedType>, where OwnedType is the most-derived class
// (extracted from the first template arg, which is always genericInterfaceC<ClassName>).
// The enable_shared_from_this lives only in the single-arg (root) specialization below;
// it is inherited through the base-class chain and initialized correctly by
// shared_ptr<DerivedClass> because DerivedClass is derived from enable_shared_from_this<RootClass>.
template <typename... Bases>
class interface_resolver : public Bases..., virtual public genericInterfaceBaseC
{
private:
  using FirstFound = typename FirstGenericDerived<Bases...>::type;
  using OwnedType  = typename class_from_interface<FirstFound>::type;

public:
  std::shared_ptr<OwnedType> nC_shared_from_this() {
    return std::static_pointer_cast<OwnedType>(this->shared_from_this());
  }
  const name2access_type& get_name2access() const override {
      return FirstFound::get_name2access();
  }
  std::unique_ptr<ETaccessorBase> access(const std::string &name) override {
      return FirstFound::access(name);
  }
  std::shared_ptr<genericInterfaceBaseC> get_interface_ptr(const std::string &name) override {
      return FirstFound::get_interface_ptr(name);
  }
  SEXP get_value(const std::string &name) const override {
      return FirstFound::get_value(name);
  }
  void set_all_values(SEXP Robj) override {
      FirstFound::set_all_values(Robj);
  }
  void set_value(const std::string &name, SEXP Svalue) override {
      FirstFound::set_value(name, Svalue);
  }
  SEXP call_method(const std::string &name, SEXP Sargs) override {
      return FirstFound::call_method(name, Sargs);
  }
  SEXP make_deserialized_return_SEXP() override {
      return FirstFound::make_deserialized_return_SEXP();
  }
};

// Single-arg specialization: root nClass (no nClass parent).
// Adds enable_shared_from_this<OwnedType> so that shared_ptr<ClassName>
// correctly initialises the weak_ptr used by shared_from_this().
// nC_shared_from_this() is a trivial cast here (same type).
template<typename First>
class interface_resolver<First> :
    public First,
    virtual public genericInterfaceBaseC,
    public std::enable_shared_from_this<typename class_from_interface<First>::type>
{
private:
  using FirstFound = First;
  using OwnedType  = typename class_from_interface<First>::type;

public:
  std::shared_ptr<OwnedType> nC_shared_from_this() {
    return std::enable_shared_from_this<OwnedType>::shared_from_this();
  }
  const name2access_type& get_name2access() const override {
      return FirstFound::get_name2access();
  }
  std::unique_ptr<ETaccessorBase> access(const std::string &name) override {
      return FirstFound::access(name);
  }
  std::shared_ptr<genericInterfaceBaseC> get_interface_ptr(const std::string &name) override {
      return FirstFound::get_interface_ptr(name);
  }
  SEXP get_value(const std::string &name) const override {
      return FirstFound::get_value(name);
  }
  void set_all_values(SEXP Robj) override {
      FirstFound::set_all_values(Robj);
  }
  void set_value(const std::string &name, SEXP Svalue) override {
      FirstFound::set_value(name, Svalue);
  }
  SEXP call_method(const std::string &name, SEXP Sargs) override {
      return FirstFound::call_method(name, Sargs);
  }
  SEXP make_deserialized_return_SEXP() override {
      return FirstFound::make_deserialized_return_SEXP();
  }
};

// Empty specialization: nClass with no generic interface (no enable_shared_from_this).
template<>
class interface_resolver<> : virtual public genericInterfaceBaseC
{
private:
  using FirstFound = genericInterfaceBaseC;

public:
  const name2access_type& get_name2access() const override {
      return FirstFound::get_name2access();
  }
  SEXP get_value(const std::string &name) const override {
      return FirstFound::get_value(name);
  }
  void set_all_values(SEXP Robj) override {
      FirstFound::set_all_values(Robj);
  }
  void set_value(const std::string &name, SEXP Svalue) override {
      FirstFound::set_value(name, Svalue);
  }
  SEXP call_method(const std::string &name, SEXP Sargs) override {
      return FirstFound::call_method(name, Sargs);
  }
  SEXP make_deserialized_return_SEXP() override {
      return FirstFound::make_deserialized_return_SEXP();
  }
};

// A forward declaration. (This is being disabled and a new approach is being used.)
//SEXP process_call_args(const genericInterfaceBaseC::args::argVectorT &argVector,
//                       SEXP Sargs);
// Base class for accessing a single member from a nimble class,
// converted to SEXP.
//
// Derived classes have pointers to members of derived interface classes.
// class ETaccessorBase;

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
  virtual std::unique_ptr<ETaccessorBase> ETaccess(genericInterfaceBaseC *) {
    std::cout<<"Error: you should be in access for a derived accessor class"<<std::endl;
  }
  virtual std::shared_ptr<genericInterfaceBaseC> getInterfacePtr(genericInterfaceBaseC *intBasePtr) {
    return nullptr;
  }
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


template<class T>
class genericInterfaceC;

#endif
