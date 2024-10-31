#ifndef DERIVSMGR_H_
#define DERIVSMGR_H_

#include <functional>
#include <tuple>
#include <utility>
#include <vector>

template<typename... ARGS>
struct pack {
  using tupleType = std::tuple<ARGS...>;
  static const size_t size = std::tuple_size<tupleType>::value;
};

///////////////////////////
// type processing to get scalar types
template<typename S>
struct scalarType {
  using type = S;
};

template<typename S>
struct scalarType<CppAD::AD<S> > {
  using type = typename scalarType<S>::type;
};

template<typename S>
struct scalarType<std::vector<S> > {
  using type = typename scalarType<S>::type;
};

///////////////////////////
// type processing to get non-AD types from AD types
template<typename S>
struct oneNonAD {
  using type = S;
};

template<typename S>
struct oneNonAD<CppAD::AD<S> > {
  using type = S;
};

template<typename S>
struct oneNonAD< std::vector<S> > {
  using type = std::vector< typename oneNonAD<S>::type >;
};

///////////////////////////
// hold types for a an AD method
template<typename C, typename RT, typename... ARGS>
struct ADmethodInfo {
  using classType = C;
  using ADreturnType = RT;
  using returnType = typename oneNonAD<RT>::type;
  using ADargsPackType = pack<ARGS...>;
  using fnType = std::function<RT(C*, ARGS...)>;
  using argsPackType = pack< typename oneNonAD<ARGS>::type ...  >;
};

///////////////////////////
// Extract types from an AD method
template<typename C, typename RT, typename... ARGS>
auto METHOD_TYPES_DUMMY(RT (C::*method)(ARGS...)) -> ADmethodInfo<C, RT, ARGS... >;

#define METHOD_TYPES( M  ) decltype( METHOD_TYPES_DUMMY(M) )

///////////////////////////
// How to handle argument conversions to AD methods
enum ADargRole {WRT, NON_WRT, CONST, NONE, RETURN, WRT_RETURN, NON_WRT_RETURN, CONST_RETURN};

template<ADargRole RT, ADargRole... ADARGROLES>
struct ADhandling {
  static const ADargRole ADreturnArgRole = RT;
  static constexpr std::array<ADargRole, sizeof...(ADARGROLES)> ADargRoleArray = {ADARGROLES...};
  static const size_t size = sizeof...(ADARGROLES);
};

///////////////////////////
// Type info for one AD argument
template<typename oneADargType, ADargRole oneADargRole, bool isReturnValue=false>
struct ADargTypeInfo {
  using ADargType = oneADargType;
  static constexpr bool is_double = std::is_same<typename scalarType< oneADargType >::type, double >::value;
  static constexpr bool require_double = ((oneADargRole != CONST) &
                                          (oneADargRole != NON_WRT) &
                                          (oneADargRole != NONE));
  static_assert(is_double || (!require_double), "WRT, NON_WRT, and RETURN conversion types require doubles.");
  static constexpr bool is_ADvar = ((oneADargRole == WRT) |
                                    (oneADargRole == WRT_RETURN) |
                                    (oneADargRole == RETURN));
  static constexpr bool is_ADdyn = ((oneADargRole == NON_WRT) |
                                    (oneADargRole == NON_WRT_RETURN));
  static constexpr bool is_ADconst = ((oneADargRole == CONST) |
                                      (oneADargRole == CONST_RETURN));
  static constexpr bool is_none = oneADargRole == NONE;
  static constexpr bool is_returnValue_valid = ((!isReturnValue) |
                                                (oneADargRole == RETURN) |
                                                (oneADargRole == NONE));
  static_assert(is_returnValue_valid, "The return value conversion type must be RETURN or NONE.");
  using easteregg = int;
};

///////////////////////////
// Type info for return and arguments of AD function
template<typename RT, typename T, typename ADRETURNARGROLE>
struct ADfunTypeInfo {};

template<typename RT, typename... ARGS, ADargRole ADRETURNARGROLE, ADargRole... ADARGROLES>
struct ADfunTypeInfo< RT, pack<ARGS...>, ADhandling<ADRETURNARGROLE, ADARGROLES...> > {
 // using tupleType = std::tuple<ARGS...>;
  using ADreturnTypeInfo = ADargTypeInfo< RT, ADRETURNARGROLE, true >;
  ADreturnTypeInfo rm;
  using ADargTypeInfoPack = pack< ADargTypeInfo<ARGS, ADARGROLES> ...  >;
  ADargTypeInfoPack am;
};

///////////////////////////
// All info (types and roles for return and arguments) of AD function
template<typename ADfunTypes, typename ADhandlingTypes, typename funTypes=void >
struct ADfunAllInfo {
  static const size_t size = ADfunTypes::ADargsPackType::size;
  static const size_t N2 = ADhandlingTypes::size;
  static_assert(size==N2, "Length of ADhandlingTypes does not match number of arguments.");
  using indices = std::index_sequence<size>;
  static const ADargRole ADreturnArgRole = ADhandlingTypes::ADreturnArgRole;
  using maps = ADfunTypeInfo< typename ADfunTypes::ADreturnType,
                              typename ADfunTypes::ADargsPackType,
                              ADhandlingTypes >;
  maps m;
};


/////////////////////////
// get lengths of arbitrary classes
template<typename T>
struct LENGTHGETTER2 {
  static size_t length(T &t) {return 1;}
};

template<typename S>
struct LENGTHGETTER2<std::vector<S> > {
  using T = std::vector<S>;
  static size_t length(T &t) {return t.size();}
};

template<size_t I, typename ADTM, typename T, bool CppADvar >
size_t GETLENGTH2(T &t) {
  static constexpr bool is_ADcategory = CppADvar ? ADTM::is_ADvar : ADTM::is_ADdyn;
  std::cout<< I <<" "<<is_ADcategory<< std::endl;
  return is_ADcategory ? LENGTHGETTER2<T>::length(t) : 0;
}

/////////////////////////
// Flatten inputs, by one CppAD role at a time (AD variables or AD dynamics).
// I.e. pack the inputs into the values vector.
template <typename T>
struct ARG_FLATTENER {
  static void flatten(T &t, std::vector<double> &values, size_t istart, size_t iend) {
    values[istart] = t;
  }
};

template <typename S>
struct ARG_FLATTENER<std::vector<S> > {
  static void flatten(std::vector<S> &t, std::vector<double> &values, size_t istart, size_t iend) {
    for(size_t i = istart; i < iend; ++i)
      values[i] = t[i-istart];
  }
};

template<size_t I, typename ADTM, typename T, typename start_end_array, bool CppADvar >
size_t ARG_FLATTENER2(T &t, std::vector<double> &values, const start_end_array &start_ends) {
  static constexpr bool is_ADcategory = CppADvar ? ADTM::is_ADvar : ADTM::is_ADdyn;
  std::cout<< I <<" "<<is_ADcategory<< std::endl;
  if constexpr(is_ADcategory) // if constexpr is C++17
    ARG_FLATTENER<T>::flatten(t, values, start_ends[I], start_ends[I+1]);
  return 0;
}

/////////////////////////
// Unflatten inputs, by one CppAD role at a time (AD variables or AD dynamics).
// I.e. unpack from the values vector to the inputs.
template <typename T>
struct ARG_UNFLATTENER {
  static T unflatten(std::vector<double> &values, size_t istart, size_t iend) {
    return static_cast<T>(values[istart]);
  }
};

template<typename S>
struct ARG_UNFLATTENER<std::vector<S> > {
  static std::vector<S> unflatten(std::vector<double> &values, size_t istart, size_t iend) {
    std::vector<S> result(iend - istart);
    for(size_t i = istart; i < iend; ++i)
      result[i-istart]=values[i];
    return result;
  }
};

template <typename T, typename S>
struct ARG_UNFLATTENER2 {
  static T unflatten(std::vector<S> &values, size_t istart, size_t iend) {
    return static_cast<T>(values[istart]);
  }
};

template <typename T, typename S>
struct ARG_UNFLATTENER2<CppAD::AD<T>, S> {
  static CppAD::AD<T> unflatten(std::vector<S> &values, size_t istart, size_t iend) {
    CppAD::AD<T> ans = values[istart];
    return ans;
  }
};

template<typename Tscalar, typename S>
struct ARG_UNFLATTENER2<std::vector<Tscalar>, S> {
  static std::vector<Tscalar> unflatten(std::vector<S> &values, size_t istart, size_t iend) {
    std::vector<Tscalar> result(iend - istart);
    for(size_t i = istart; i < iend; ++i)
      // may need some casting here.
      result[i-istart]=values[i];
    return result;
  }
};

template<size_t I, typename ADargTypeInfo, typename NONADARG, typename ADargTupleType, typename start_end_array, typename S >
auto ARGS_UNFLATTENER2(NONADARG &t, ADargTupleType &ADargTuple,
                      std::vector<S> &var_values, const start_end_array &var_start_ends,
                      std::vector<S> &dyn_values, const start_end_array &dyn_start_ends) -> typename ADargTypeInfo::ADargType {
  static constexpr bool is_ADvar = ADargTypeInfo::is_ADvar;
  static constexpr bool is_ADdyn = ADargTypeInfo::is_ADdyn;
  static constexpr bool use_original = !(is_ADvar || is_ADdyn);
  if constexpr(use_original) {
    return t;
  } else {
    if constexpr(is_ADvar) {
      std::get<I>(ADargTuple) = ARG_UNFLATTENER2<typename ADargTypeInfo::ADargType, S>::unflatten(var_values, var_start_ends[I], var_start_ends[I+1]);
    } else {
      std::get<I>(ADargTuple) = ARG_UNFLATTENER2<typename ADargTypeInfo::ADargType, S>::unflatten(dyn_values, dyn_start_ends[I], dyn_start_ends[I+1]);
    }
    return std::get<I>(ADargTuple);
  }
}

/////////////
// some checks
template<typename ADARGTYPEINFO>
struct CHECK1 {
  static const bool value = std::conditional< std::is_same<typename ADARGTYPEINFO::ADargType, double>::value, std::true_type, std::false_type >::type::value;
};

template<typename ADFUNTYPES>
struct CHECK2 {
  static const bool value = std::conditional< std::is_same< typename ADFUNTYPES::ADreturnType, CppAD::AD<double> >::value, std::true_type, std::false_type >::type::value;
};

template<typename ADARGTYPE>
struct CHECK3 {
  static const bool value = std::conditional< std::is_same< ADARGTYPE, CppAD::AD<double> >::value, std::true_type, std::false_type >::type::value;
};

///////////////////////////
// utility for recursive peeling of parameter packs
template< typename F, typename... REST >
struct SPLIT {
  using firstType = F;
  using restPackType = pack<REST...>;
};

///////////////////////////
// error-trapping tool to see whether template arguments caught from fn
// match template arguments with which nDerivs is called.
template< typename ARGS1, typename ARGS2 >
struct ARGS_MATCH;

template< typename... ARGS1, typename... ARGS2  >
struct ARGS_MATCH< pack<ARGS1...>, pack<ARGS2...> > {
  static constexpr bool testbool = std::true_type::value;
  using ARGS1split = SPLIT<ARGS1...>;
  using ARGS2split = SPLIT<ARGS2...>;
  static constexpr bool lengthsMatch = sizeof...(ARGS1) == sizeof...(ARGS2);
  static constexpr bool firstMatch = std::is_same<typename std::remove_reference< typename ARGS1split::firstType>::type,
                                                  typename std::remove_reference< typename ARGS1split::firstType>::type>::value;
  static constexpr bool restMatch = std::conditional<(ARGS1split::restPackType::size >= 1),
    ARGS_MATCH<typename ARGS1split::restPackType, typename ARGS2split::restPackType>,
    std::true_type>::type::value;
  static constexpr bool value =  firstMatch && restMatch;
};

class AD_tape_mgr {
public:
  CppAD::ADFun<double> *ADtape_;
  CppAD::ADFun<double>* &ADtape() {return ADtape_;};

  // we'll move starts_ends and total_length here.
};

class AD_base_class {
public:
};

template<typename ADfunTypes, typename ADhandlingTypes, typename funTypes=void >
class deriv_mgr2 {
  // example: ADfunTypes will be METHOD_TYPES( &CppAD::AD<double>(NC, CppAD::AD<double>) ), aka ADmethodInfo(NC, CppAD::AD<double>, CppAD::AD<double>)
  // example AD handlingTypes will be ADhandling<RETURN, WRT>
  using thisType = deriv_mgr2<ADfunTypes, ADhandlingTypes, funTypes>;
  public:
  using allInfo = ADfunAllInfo<ADfunTypes, ADhandlingTypes, funTypes>;
  allInfo tm; // make static assertions run.

  using classType = typename ADfunTypes::classType; // e.g. NC
  using ADreturnType = typename ADfunTypes::ADreturnType; // e.g. CppAD::AD<double>
  using fnType = typename ADfunTypes::fnType;
  fnType fn;
  classType* const obj;
  deriv_mgr2( fnType f, classType *obj_ ) : fn(f), obj(obj_) {};

  using ADargTupleType = typename ADfunTypes::ADargsPackType::tupleType; // e.g. std::tuple< CppAD::AD<double> >
  ADargTupleType ADargTuple;

  static const size_t NUMARGS = allInfo::size;
  using indices = std::make_index_sequence<NUMARGS>;
  using length_array = std::array<size_t, NUMARGS>;
  length_array ADvar_lengths;
  length_array ADdyn_lengths;
  length_array dummy;
  using start_end_array = std::array<size_t, NUMARGS+1>;
  start_end_array ADvar_start_ends;
  start_end_array ADdyn_start_ends;
  size_t ADvar_total_length;
  size_t ADdyn_total_length;
  std::vector<double> var_values;
  std::vector<double> dyn_values;
  std::vector<CppAD::AD<double> > ADvar_values;
  std::vector<CppAD::AD<double> > ADdyn_values;

  template<size_t... INDS, typename... NONADARGS, typename... ADARGTYPEINFOS >
  void set_lengths(std::index_sequence<INDS...>, pack<ADARGTYPEINFOS...>, NONADARGS... nonadargs  ) {
    ADvar_lengths = {GETLENGTH2<INDS, ADARGTYPEINFOS, NONADARGS, true>(nonadargs)...};
    ADvar_total_length = std::accumulate(ADvar_lengths.begin(), ADvar_lengths.end(), 0);
    var_values.resize(ADvar_total_length);
    ADvar_values.resize(ADvar_total_length);
    ADvar_start_ends[0] = 0;
    std::partial_sum(ADvar_lengths.begin(), ADvar_lengths.end(), ADvar_start_ends.begin()+1);

    ADdyn_lengths = {GETLENGTH2<INDS, ADARGTYPEINFOS, NONADARGS, false>(nonadargs)...};
    ADdyn_total_length = std::accumulate(ADdyn_lengths.begin(), ADdyn_lengths.end(), 0);
    dyn_values.resize(ADdyn_total_length);
    ADdyn_values.resize(ADdyn_total_length);
    ADdyn_start_ends[0] = 0;
    std::partial_sum(ADdyn_lengths.begin(), ADdyn_lengths.end(), ADdyn_start_ends.begin()+1);
  }
  // template<int i, typename F>
  // void flatten_args_inner(F &f) {
  //   std::cout<< i <<" (last) "<<std::endl;
  //   ARG_FLATTENER<F>::flatten(f, values, start_ends[i], start_ends[i+1]);
  // }
  // template<size_t i, typename F, typename... INNERARGS>
  // void flatten_args_inner(F &f, INNERARGS... args) {
  //   std::cout<< i <<std::endl;
  //   ARG_FLATTENER<F>::flatten(f, values, start_ends[i], start_ends[i+1]);
  //   flatten_args_inner<i+1, INNERARGS...>(args...);
  // }

  template<size_t... INDS, typename... NONADARGS, typename... ADARGTYPEINFOS >
  void flatten_args(std::index_sequence<INDS...>, pack<ADARGTYPEINFOS...>, NONADARGS... nonadargs  ) {
    dummy = {ARG_FLATTENER2<INDS, ADARGTYPEINFOS, NONADARGS, start_end_array, true>
             (nonadargs, var_values, ADvar_start_ends)...};
    dummy = {ARG_FLATTENER2<INDS, ADARGTYPEINFOS, NONADARGS, start_end_array, false>
             (nonadargs, dyn_values, ADdyn_start_ends)...};
  }

  using maps = typename allInfo::maps;
  using ADargTypeInfoPack = typename maps::ADargTypeInfoPack;

  // Here INDS will be an index_sequence
  // ADARGTYPEINFOS is deduced from ADargTypeInfoPack(), which has type pack<>
  template<size_t... INDS, typename... ADARGTYPEINFOS, typename... NONADARGS >
  ADreturnType runfun_internal(std::index_sequence<INDS...>, pack<ADARGTYPEINFOS...>, NONADARGS... nonadargs  ) {
    return fn(obj, ARGS_UNFLATTENER2<INDS, ADARGTYPEINFOS, NONADARGS, ADargTupleType, start_end_array, CppAD::AD<double> >
              (nonadargs, ADargTuple, ADvar_values, ADvar_start_ends, ADdyn_values, ADdyn_start_ends ) ...);
  }

   template<typename... ADARGTYPEINFOS>
   void check1(pack<ADARGTYPEINFOS...>) {
  std::array<bool, 1> ans = {CHECK1<ADARGTYPEINFOS>::value ...};
  std::cout<<"check1 "<<ans[0]<<std::endl;
  }

    template<typename... ADARGTYPEINFOS>
   void check3(pack<ADARGTYPEINFOS...>) {
  std::array<bool, 1> ans = {CHECK3<ADARGTYPEINFOS>::value ...};
  std::cout<<"check3 "<<ans[0]<<std::endl;
  }

  template<typename... NONADARGS>
  void record_tape(AD_tape_mgr &ADTM, NONADARGS... nonadargs) {
    set_lengths(indices(), ADargTypeInfoPack(), nonadargs...);
    flatten_args(indices(), ADargTypeInfoPack(), nonadargs...);
    std::transform(var_values.begin(), var_values.end(), ADvar_values.begin(), [](double x)->CppAD::AD<double>{return CppAD::AD<double>(x);});
    ADreturnType res;
    check1(ADargTypeInfoPack());
    std::cout<< "check 2 "<< CHECK2<ADfunTypes>::value  <<std::endl;
    using ADargsPackType = typename ADfunTypes::ADargsPackType;
    check3(ADargsPackType());
    res = runfun_internal(indices(), ADargTypeInfoPack(), nonadargs...);
                std::cout<<"success"<<std::endl;
  }

  template<typename... NONADARGS>
  void nDerivs(bool do_update, bool reset, AD_tape_mgr &ADTM, NONADARGS... nonadargs ) {
    static_assert( ARGS_MATCH< pack<NONADARGS...>,
                   typename ADfunTypes::argsPackType >::value,
                   "nDerivs was coded with arguments that don't match its function.");
    if(true) { // if(reset)
      record_tape(ADTM, nonadargs...);
      // run_for_taping;
    }
  }
};


#endif // DERIVSMGR_H_
