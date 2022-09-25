#ifndef __NCOMPILER_CLASS_INTERFACE
#define __NCOMPILER_CLASS_INTERFACE

#include<Rcpp.h>
#include<map>
#include<string>
#include<utility>
#include<Rinternals.h>
#include<iostream>
#include<memory>
#include<nCompiler/nCompiler_cereal.h>
#include<nCompiler/expand_call_method.h>
#include<nCompiler/shared_ptr_as_wrap.h>

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
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {}
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
    const args::argVectorT &argVector(method->second.my_args.argVector);
    args::argVectorT::size_type numArgsRequired(argVector.size());
    std::vector<std::string> namesProvided(numArgsRequired);
    std::vector<SEXP> promisesProvided(numArgsRequired);
    Rcpp::List RinnerArgs(numArgsRequired);

    // We implement R's rules for argument matching:
    // 1. exact matces; 2. partial matches; 3. positional
    // https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Argument-matching
    // In addition, we do not (cannot easily) support missingness.
    if(numArgsRequired) { // continue if > 0 args
      // Get '...' from the calling environment
      bool using_dotdotdot(true);
      std::vector<int> arg_match_indices(numArgsRequired, -1);
      SEXP Sdotdotdot = Rf_findVarInFrame(Sargs, R_DotsSymbol);
      std::cout<<TYPEOF(Sdotdotdot)<<std::endl;
      if(TYPEOF(Sdotdotdot) != DOTSXP) {
        //Rcpp::Environment Renv(Sargs);
        //SEXP Sparam = Renv["param"];
        using_dotdotdot = false;
        for(int iReq = 0; iReq < argVector.size(); ++iReq) {
          const std::string expected_name(argVector[iReq].name);
          promisesProvided[iReq] = Rf_findVarInFrame(Sargs, Rf_install(expected_name.c_str()));
          std::cout<<expected_name<<": "<<TYPEOF(promisesProvided[iReq])<<std::endl;
                                    if(TYPEOF(promisesProvided[iReq])!=PROMSXP) {
          Rcpp::stop("nCompiler call_method's environment does not have '...' so all arguments must be provided.\n");
        }
        }
        } else {
            // partial_matches "rows" will be required(aka expected), "columns" will be provided
            std::vector<bool> partial_matches(numArgsRequired*numArgsRequired, false);
            std::vector<bool> provided_arg_used(numArgsRequired, false);
            bool done(false);
            // We navigate through the ... pairlist using Rinternals.
            // Use of Rcpp for this was not straightforward.
            SEXP SthisArg = CAR(Sdotdotdot);
            SEXP SnextArg = CDR(Sdotdotdot);
            SEXP SargSym = TAG(Sdotdotdot);
            // Iterate over provided arguments
            size_t numArgsProvided;
            size_t iProv = 0; // index of provided argument
            while(!done) {
              if(TYPEOF(SthisArg)!=PROMSXP) { // PROMSXP=5
                //          std::cout<<"finishing on type "<<TYPEOF(SthisArg)<<std::endl;
                done = true;
              } else {
                promisesProvided[iProv] = SthisArg;
                if(TYPEOF(SargSym)==SYMSXP) { // (SYMSXP=1); so the provided arg is named
                  SEXP SprovName = PRINTNAME(SargSym);
                  // CHARSXP's have internal details already figured out
                  // by Rcpp, so use Rcpp to obtain regular std::string
                  const std::string provName(Rcpp::String(SprovName).get_cstring());
                  namesProvided[iProv] = provName; // used later only for error reporting.
                  // Iterate over require arguments
                  bool this_exact_matches(false);
                  int iReq = 0;
                  while((!this_exact_matches) && iReq < numArgsRequired) {
                    // C++ STL tools for partial matching
                    // are not the right fit, and this is so simple,
                    // so we'll cook it right here: Determine if
                    // the name match is exact, partial, or neither

                    const std::string expected_name(argVector[iReq].name);
                    std::string::const_iterator iExpected(expected_name.begin());
                    const std::string::const_iterator iExpEnd(expected_name.end());
                    std::string::const_iterator iProvided(provName.begin());
                    const std::string::const_iterator iProvEnd(provName.end());
                    bool done_this_match((iExpected == iExpEnd) || (iProvided == iProvEnd));
                    if(!done_this_match)
                      done_this_match = (*iExpected) != (*iProvided);
                    while(!done_this_match) {
                      ++iExpected;
                      ++iProvided;
                      done_this_match = (iExpected == iExpEnd) || (iProvided == iProvEnd);
                      if(!done_this_match) {
                        done_this_match = (*iExpected) != (*iProvided);
                      }
                    }
                    std::cout<<*iExpected<<" "<<*iProvided<<std::endl;
                    this_exact_matches = (iProvided == iProvEnd) &&
                      (*iExpected) == (*iProvided);
                    bool this_partial_matches = iProvided == iProvEnd;

                    //              std::cout<<"match result for input "<<provName<<" to "<<expected_name<<" "<<this_exact_matches<<" "<<this_partial_matches<<std::endl;

                    if(this_exact_matches) { // exact match
                      if(arg_match_indices[iReq]==-1) {
                        //                 std::cout<<"exact match for arg "<<provName<<std::endl;
                        arg_match_indices[iReq] = iProv;
                        provided_arg_used[iProv] = true;
                      } else {
                        Rcpp::stop("Multiple exact matches to name " + provName);
                      }
                    } else if(this_partial_matches) {
                      partial_matches[iReq + numArgsRequired*iProv] = true;
                    }
                    ++iReq;
                  } // finish iterating over required args
                } // finish handling case that an arg name was input
                //          int l = LENGTH(STRING_ELT(SargName, 0));
                //         std::string ans(CHAR(STRING_ELT(SargName 0)));
                //         std::cout<<"name is "<<ans<<std::endl;
              } // finish handling that this provided arg
              ++iProv;
              done = TYPEOF(SnextArg) == NILSXP;
              //        std::cout<<"next type "<<TYPEOF(SnextArg)<<std::endl;
              if(iProv >= numArgsRequired && !done) {
                Rcpp::stop("Too many arguments provided.");
                done = true;
              }
              if(!done) {
                SthisArg = CAR(SnextArg);
                SargSym = TAG(SnextArg);
                SnextArg = CDR(SnextArg);
              }
            } //finish iterating over provided args
            numArgsProvided = iProv;// We will have already errored out if too many were provided
            if(numArgsProvided != numArgsRequired) {
              Rcpp::stop("Too few arguments provided.");
            }
            // At this point, we have arg_match_indices set for exact matches
            // and partial_matches matrix set for partial matches
            // Next we choose partial matches for args that lack exact matches
            for(int iReq = 0; iReq < numArgsRequired; ++iReq) {
              if(arg_match_indices[iReq]==-1) {
                int new_match(-1);
                for(int iP = 0; iP < numArgsRequired; ++iP) {
                  if(partial_matches[iReq + numArgsRequired*iP]) {
                    if(new_match != -1) {
                      Rcpp::stop("Multiple partial matches to " + namesProvided[iP]);
                    } else {
                      new_match = iP;
                    }
                  }
                }
                if(new_match != -1) {
                  arg_match_indices[iReq] = new_match;
                  provided_arg_used[new_match] = true;
                }
              }
            }
            // At this point, we have arg_match_indices for both exact and partial matches.
            // Now we check if any unused arguments were named, which is an error.
            for(int iP = 0; iP < numArgsRequired; ++iP) {
              if(!provided_arg_used[iP]) {
                if(namesProvided[iP].size() != 0) {
                  Rcpp::stop("Named argument " + namesProvided[iP] + " does not match any formal arguments.");
                }
              }
            }

            // And then we place remaining arguments in order.
            bool done_unnamed_matching(false);
            int i_unmatched_req(0);
            int i_unused_prov(0);
            while(!done_unnamed_matching) {
              while(arg_match_indices[i_unmatched_req] != -1 &&
                    i_unmatched_req < numArgsRequired-1) {
                ++i_unmatched_req;
              }
              while(provided_arg_used[i_unused_prov] &&
                    i_unused_prov < numArgsRequired-1) {
                ++i_unused_prov;
              }
              //        std::cout<<"i_unmatched_req " <<i_unmatched_req<<" i_unused_prov "<< i_unused_prov<<std::endl;
              // At this point we have already trapped mismatched numbers of
              // arguments and multiple partial matches, so
              // the unnamed matches should work perfectly.
              if(arg_match_indices[i_unmatched_req] == -1 &&
                 (!provided_arg_used[i_unused_prov])) {
                //          std::cout<<"unnamed placement of provided "<<i_unused_prov<<" for formal "<<i_unmatched_req<<std::endl;
                arg_match_indices[i_unmatched_req] = i_unused_prov;
                provided_arg_used[i_unused_prov] = true;
              } else {
                done_unnamed_matching = true;
              }
            }
            // std::cout<<"arg_match_indices ";
            // for(int iii = 0; iii < numArgsRequired; ++iii) {
            //   std::cout<<arg_match_indices[iii]<<" ";
            // }
            // std::cout<<std::endl;
            // std::cout<<"ready to construct arg list for inner call"<<std::endl;
            //
            // We use Rcpp tools at this step to get PROTECTion behavior.
            // We are not sure how direct use of PROTECT/UNPROTECT works
            // when inside of the called function there could be an error trap
            // that does not return control here for UNPROTECT.
          }
          for(int iReq = 0; iReq < numArgsRequired; ++iReq) {
            int this_iP = using_dotdotdot ? arg_match_indices[iReq] : iReq;
            switch(argVector[iReq].APtype) {
            case copy:
              {
                RinnerArgs[iReq] = Rf_eval(R_PromiseExpr(promisesProvided[this_iP]),
                                           PRENV(promisesProvided[this_iP]));
                break;
              };
            case ref:
              {
                // In the argument place, put a list of the argument name and its environment
                // This imitates R function createRefInfoIntoC.  Any changes here or there must
                // be kept up to date with each other.
                std::cout<<"handling ref"<<std::endl;
                RinnerArgs[iReq] = Rcpp::List::create(PRCODE(promisesProvided[this_iP]),
                                                      PRENV(promisesProvided[this_iP]));
                break;
              };
            case refBlock:
              {
                std::cout<<"handling blockRef"<<std::endl;
                RinnerArgs[iReq] = Rcpp::List::create(PRCODE(promisesProvided[this_iP]),
                                                      PRENV(promisesProvided[this_iP]));
                break;
              };
            default:
              Rcpp::stop("Invalid argPassingType"); // should never ever be reached
            }
          }

          // Can I UNPROTECT(1) here, in case there is any error-trap inside the
          // method that prevents returning to this code?
          // In the subsequent calls, there is a LENGTH() and VECTOR_ELT() calls on SinnerArgs.
          // Are those safe without PROTECTion?
          // Alternatively, should I make SinnerArgs an Rcpp::List?
        }         // end of if(numArgsRequired)
        SEXP Sans = PROTECT(method->second.method_ptr->call(this, RinnerArgs));
        UNPROTECT(1);
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
  template<class Archive>
    void _SERIALIZE_(Archive &archive) {
    archive(cereal::base_class<genericInterfaceBaseC>(this));
  }
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
