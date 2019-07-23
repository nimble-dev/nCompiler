#ifndef __EXPAND_CALL_METHOD
#define __EXPAND_CALL_METHOD

// Possible issues:
// Istead of a direct call to Rcpp:as, it should generally be like:
// typename Rcpp::traits::input_parameter<U1>::type x1(args[1]);
// based on Rcpp's Module_generated_CppMethod.h and elsewhere.
// This should allow an extra layer of type decision.
// Also it may be that creating local arguments is important
// so we have lvalues.  In C++11 std::forward<> might be the thing to do.

template<class P, typename T>
  struct expand_call_method_narg {
  public:
    template<typename ptrtype>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)();
    }

    template<typename ptrtype, typename A1>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(Rcpp::as<A1>(VECTOR_ELT(Sargs, 0)));
    }
    
    template<typename ptrtype, typename A1, typename A2>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(Rcpp::as<A1>(VECTOR_ELT(Sargs, 0)),
			 Rcpp::as<A2>(VECTOR_ELT(Sargs, 1))
			 );
    }

    template<typename ptrtype, typename A1, typename A2, typename A3>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(Rcpp::as<A1>(VECTOR_ELT(Sargs, 0)),
			 Rcpp::as<A2>(VECTOR_ELT(Sargs, 1)),
			 Rcpp::as<A3>(VECTOR_ELT(Sargs, 2)));
    }

    template<typename ptrtype, typename A1, typename A2, typename A3, typename A4>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(Rcpp::as<A1>(VECTOR_ELT(Sargs, 0)),
			 Rcpp::as<A2>(VECTOR_ELT(Sargs, 1)),
			 Rcpp::as<A3>(VECTOR_ELT(Sargs, 2)),
			 Rcpp::as<A4>(VECTOR_ELT(Sargs, 3)));
    }

    template<typename ptrtype, typename A1, typename A2, typename A3, typename A4, typename A5>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(Rcpp::as<A1>(VECTOR_ELT(Sargs, 0)),
			 Rcpp::as<A2>(VECTOR_ELT(Sargs, 1)),
			 Rcpp::as<A3>(VECTOR_ELT(Sargs, 2)),
			 Rcpp::as<A4>(VECTOR_ELT(Sargs, 3)),
			 Rcpp::as<A5>(VECTOR_ELT(Sargs, 4)));
    }

    template<typename ptrtype, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(Rcpp::as<A1>(VECTOR_ELT(Sargs, 0)),
			 Rcpp::as<A2>(VECTOR_ELT(Sargs, 1)),
			 Rcpp::as<A3>(VECTOR_ELT(Sargs, 2)),
			 Rcpp::as<A4>(VECTOR_ELT(Sargs, 3)),
			 Rcpp::as<A5>(VECTOR_ELT(Sargs, 4)),
			 Rcpp::as<A6>(VECTOR_ELT(Sargs, 5)));
    }

  };

#endif
