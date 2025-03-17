#ifndef NCOMPILER_MODEL_BASE_DEVEL_H_
#define NCOMPILER_MODEL_BASE_DEVEL_H_

// The content here should eventually live in a nimbleModel package
// but the quickest way to get into the guts of development is to
// put it here.

template<class Derived>
class modelBaseClass {
  public:
  double v;
  void base_hw() {
    Rprintf("base hw\n");
  }
  void set_from_list(Rcpp::List Rlist) {
    Rcpp::CharacterVector Rnames = Rlist.names();
    size_t len = Rnames.length();
    for(size_t i = 0; i < len; ++i) {
      // explicit cast is needed because even though Rnames[i] can cast to a string,
      // set_value takes a const string& so we need an object in place here.
      static_cast<Derived*>(this)->set_value(std::string(Rnames[i]), Rlist[i]);
    }
  }
  void resize_from_list(Rcpp::List Rlist) {
    Rcpp::CharacterVector Rnames = Rlist.names();
    size_t len = Rnames.length();
    size_t vec_len;
    Rcpp::IntegerVector vs;
    for(size_t i = 0; i < len; ++i) {
      // explicit cast is needed because even though Rnames[i] can cast to a string,
      // set_value takes a const string& so we need an object in place here.
      vs = Rlist[i];
      vec_len = vs.length();
      std::unique_ptr<ETaccessorBase> ETA = static_cast<Derived*>(this)->access(std::string(Rnames[i]));
      switch(vec_len) {
      case 0 :
        break;
      case 1 :
        ETA->template ref<1>().resize(vs[0]);
        break;
      case 2 :
        ETA->template ref<2>().resize(vs[0], vs[1]);
        break;
      case 3 :
        ETA->template ref<3>().resize(vs[0], vs[1], vs[2]);
        break;
      case 4 :
        ETA->template ref<4>().resize(vs[0], vs[1], vs[2], vs[3]);
        break;
      case 5 :
        ETA->template ref<5>().resize(vs[0], vs[1], vs[2], vs[3], vs[4]);
        break;
      case 6 :
        ETA->template ref<6>().resize(vs[0], vs[1], vs[2], vs[3], vs[4], vs[5]);
        break;
      }
    }
  }
};

template<class Derived>
class nodeFunctionBase {
  public:
  double v;

};

#endif // NCOMPILER_MODEL_BASE_DEVEL_H_
