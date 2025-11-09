#ifndef NLIST_H_
#define NLIST_H_

// I think nList could be done as a pre_Rcpp if we determine we
// won't use genericInterfaceC.
template<typename T>
class nList { // : public genericInterfaceC<nList<T> > {
private:
  std::vector< T  > contents;
public:
  std::vector< T > &get() {return contents;}
  typename std::vector< T >::iterator begin() {return contents.begin();}
  typename std::vector< T >::const_iterator cbegin() {return contents.cbegin();}
  typename std::vector< T >::iterator end() {return contents.end();}
  typename std::vector< T >::const_iterator cend() {return contents.cend();}
  T get(size_t i) const {return contents[i];}
  T set(size_t i, const T& value) {return contents[i] = value;}
  void resize(size_t m) {contents.resize(m);}
  size_t size() const {return contents.size();}
  T& operator[](size_t i) {return contents[i];}
  const T& operator[](size_t i) const {return contents[i];}
  nList() {};
  nList(size_t len) : contents(len) {};
};

// These chunks of commented code record the thought process and experiments
// with making nLists<>s inherit from genericInterfaceC.
// I don't think it's really natural to do so but it could become handy in some situaitons.
//
// I am not sure we really need nList<>s to be genericInterfaces
//  That would mean we want to get an extptr and manually change them.
//  We can do it and include the following, but we'd need to make sure
//  this only appears once (as it defines static template members)
// We'll make a specialized macro for nList interface
// This did compile from here to....
// #define NCOMPILER_INTERFACE_NLIST(T)\
//   template <>\
//    int genericInterfaceC<nList<T> >::name_count = 0;	\
//    template<>\
//    genericInterfaceC<nList<T> >::name2index_type genericInterfaceC<nList<T> >::name2index {};\
//    template<>\
//    genericInterfaceC<nList<T> >::name2access_type genericInterfaceC<nList<T> >::name2access \
//    {}\
//    ;\
//    template<>\
//    genericInterfaceC<nList<T> >::name2method_type genericInterfaceC<nList<T> >::name2method \
//    {\
//  method("resize", &nList<T>::resize, args({{arg("i",copy)}}))\
//    }\
//  ;
//NCOMPILER_INTERFACE_NLIST(double)
// ...here

// Following is an idea of how we'd need another calling interface since there would be no shared_ptr.
// This was sketched out but not checked or tested.
/* inline genericInterfaceBaseC *get_genericInterfaceBaseC_nonptr(SEXP Xptr) { */
/*   return reinterpret_cast<genericInterfaceBaseC*>(R_ExternalPtrAddr(Xptr)); */
/* } */
/* // A different version of accessing generic interface calls. */
/* // This is for the case without the share_ptr_holder layer. */
/* // [[Rcpp::export]] */
/* SEXP call_method_nonptr(SEXP Xptr, const std::string &name, SEXP Sargs) { */
/*   genericInterfaceBaseC *obj = */
/*     get_genericInterfaceBaseC_nonptr(Xptr); */
/*   return(obj->call_method( name, Sargs )); */
/* } */

// Here I am starting
// from code on nCompiler/shared_ptr_as_wrap and modifying
// These will work by copy.
namespace Rcpp {
  namespace traits {
    template <typename T>
    class Exporter< nList< T > > {
    public:
      Exporter(SEXP Sx) :
        Sinput(Sx) { }
      inline nList< T > get(){
        Rcpp::List Rlist(Sinput);
        nList<T> ans(Rlist.size());
        for(size_t i = 0; i < ans.size(); ++i) {
          ans[i] = Rcpp::as<T>(Rlist[i]); // explicit template argument required here
        }
        return ans;
      }
    private:
      SEXP Sinput;
    };
  }
}

namespace Rcpp {
  template<typename T>
  SEXP wrap(const nList< T > & obj ) {
    Rcpp::List Sans = Rcpp::List( obj.size());
    std::cout<<"wrap "<<obj.size()<<std::endl;
    for(size_t i = 0; i < obj.size(); ++i)
      Sans[i] = obj[i];
    return Sans;
  }
}


#endif // NLIST_H_
