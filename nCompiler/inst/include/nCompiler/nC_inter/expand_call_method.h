#ifndef __EXPAND_CALL_METHOD
#define __EXPAND_CALL_METHOD

// Class conversion issues:
// Instead of a direct call to Rcpp:as, it generally needs to be like:
// typename Rcpp::traits::input_parameter<U1>::type x1(args[1]);
// based on Rcpp's Module_generated_CppMethod.h and elsewhere.
// This allows an extra layer of type decision. In particular, if type U1
// is a reference type, such as Eigen::Tensor<double, 1>&, then
// Rcpp::traits::input_parameter<U1>::type x1(args[1]) will forward args[1] to
// Rcpp::as<Eigen::Tensor<double, 1>>, automatically stripping the reference.
// Stripping the reference is important because using Rcpp::as with reference
// types will generally not compile when used as template arguments for
// expand_call_method_narg.
//
// Possible issues:
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
      return (obj->*ptr)(
        typename Rcpp::traits::input_parameter<A1>::type(VECTOR_ELT(Sargs, 0))
      );
    }
    
    template<typename ptrtype, typename A1, typename A2>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(
        typename Rcpp::traits::input_parameter<A1>::type(VECTOR_ELT(Sargs, 0)),
        typename Rcpp::traits::input_parameter<A2>::type(VECTOR_ELT(Sargs, 1))
      );
    }

    template<typename ptrtype, typename A1, typename A2, typename A3>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(
        typename Rcpp::traits::input_parameter<A1>::type(VECTOR_ELT(Sargs, 0)),
        typename Rcpp::traits::input_parameter<A2>::type(VECTOR_ELT(Sargs, 1)),
        typename Rcpp::traits::input_parameter<A3>::type(VECTOR_ELT(Sargs, 2)));
    }

    template<typename ptrtype, typename A1, typename A2, typename A3, typename A4>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(
        typename Rcpp::traits::input_parameter<A1>::type(VECTOR_ELT(Sargs, 0)),
        typename Rcpp::traits::input_parameter<A2>::type(VECTOR_ELT(Sargs, 1)),
        typename Rcpp::traits::input_parameter<A3>::type(VECTOR_ELT(Sargs, 2)),
        typename Rcpp::traits::input_parameter<A4>::type(VECTOR_ELT(Sargs, 3)));
    }

    template<typename ptrtype, typename A1, typename A2, typename A3, typename A4, typename A5>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(
        typename Rcpp::traits::input_parameter<A1>::type(VECTOR_ELT(Sargs, 0)),
        typename Rcpp::traits::input_parameter<A2>::type(VECTOR_ELT(Sargs, 1)),
        typename Rcpp::traits::input_parameter<A3>::type(VECTOR_ELT(Sargs, 2)),
        typename Rcpp::traits::input_parameter<A4>::type(VECTOR_ELT(Sargs, 3)),
        typename Rcpp::traits::input_parameter<A5>::type(VECTOR_ELT(Sargs, 4)));
    }

    template<typename ptrtype, typename A1, typename A2, typename A3, typename A4, typename A5, typename A6>
    static P call(T *obj, ptrtype ptr, SEXP Sargs) {
      return (obj->*ptr)(
        typename Rcpp::traits::input_parameter<A1>::type(VECTOR_ELT(Sargs, 0)),
        typename Rcpp::traits::input_parameter<A2>::type(VECTOR_ELT(Sargs, 1)),
        typename Rcpp::traits::input_parameter<A3>::type(VECTOR_ELT(Sargs, 2)),
        typename Rcpp::traits::input_parameter<A4>::type(VECTOR_ELT(Sargs, 3)),
        typename Rcpp::traits::input_parameter<A5>::type(VECTOR_ELT(Sargs, 4)),
        typename Rcpp::traits::input_parameter<A6>::type(VECTOR_ELT(Sargs, 5)));
    }

  };

#endif
