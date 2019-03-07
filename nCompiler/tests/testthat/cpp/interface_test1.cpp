// get nimble/include into -I flags
// [[Rcpp::depends(nimbleCompiler)]]
#include <nimble/nimbleClassInterface.h>

class fooC : public genericInterfaceC<fooC> {
 public:
  // example member data
  int x;
  double y;
  // silly constructor that calls hello world.
 fooC() : x(2), y(3.5) {
    hw();
  }
  // example method
  double print_val(double a1, int a2) {
    std::cout<<a1<<" "<<a2<<std::endl;
    return(a1 + a2);
  }
};

// Next section could be turned into a macro with input name fooC
// followed by "field" and "method" entries.
template<>
int genericInterfaceC<fooC>::name_count = 0;

template<>
genericInterfaceC<fooC>::name2index_type genericInterfaceC<fooC>::name2index {};

template<>
genericInterfaceC<fooC>::name2access_type genericInterfaceC<fooC>::name2access {
  field("x", &fooC::x),
    field("y", &fooC::y)
};

template<>
genericInterfaceC<fooC>::name2method_type genericInterfaceC<fooC>::name2method {
    method("print_val", &fooC::print_val)
};

// // see inst/include/nimbleClassInterface.h
// for a fully developed NIMBLE_INTERFACE template
// to accomplish what is needed

  




// "main.cpp":

extern "C" {
  SEXP new_foo() {
    fooC *test = new fooC;
    return(R_MakeExternalPtr(static_cast<void*>(test),
			     R_NilValue,
			     R_NilValue));
  }
}
// This is completely generic, good for all derived classes
// [[Rcpp::export]]
SEXP get_value(SEXP Xptr, const std::string &name) {
  genericInterfaceBaseC *obj =
    reinterpret_cast<genericInterfaceBaseC*>(R_ExternalPtrAddr(Xptr));
  std::cout << name << std::endl;
  return(obj->get_value( name ));
}

extern "C" {
  // See how to use .External
  SEXP count_args(SEXP args) {
    std::cout<<Rf_length(args)<<std::endl;
    return(R_NilValue);
  }
}

// This is completely generic, good for all derived classes
// [[Rcpp::export]]
SEXP call_method(SEXP Xptr, const std::string &name, SEXP Sargs) {
  genericInterfaceBaseC *obj =
    reinterpret_cast<genericInterfaceBaseC*>(R_ExternalPtrAddr(Xptr));
  std::cout << name << std::endl;
  return(obj->call_method( name, Sargs ));
}


