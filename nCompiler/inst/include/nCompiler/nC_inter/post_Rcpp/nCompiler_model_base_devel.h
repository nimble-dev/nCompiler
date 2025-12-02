#ifndef NCOMPILER_MODEL_BASE_DEVEL_H_
#define NCOMPILER_MODEL_BASE_DEVEL_H_

// The content here should eventually live in a nimbleModel package
// but the quickest way to get into the guts of development is to
// put it here.

class modelBaseClass_ {
  public:
  double v;
  virtual ~modelBaseClass_() {};
};

class nodeFunctionClassBase_ {
  public:
  double v;
  virtual ~nodeFunctionClassBase_() {};
};

template<class Derived>
class modelClass_ : public modelBaseClass_ {
  public:
  double v;
  void base_hw() {
    Rprintf("base hw\n");
  }
  std::vector< std::shared_ptr<nodeFunctionClassBase_> > nodeFunctionPtrs;
  // NEXT STEPS: record the shared_ptrs and indices for future use.
  // build up calculate at level of node and then model.
  void setup_node_mgmt() {
      Derived *self = static_cast<Derived*>(this);
      const auto& name2access = self->get_name2access();
      size_t n = name2access.size();
      Rprintf("There are %d member variables indexed:\n", (int)n);
      auto i_n2a = name2access.begin();
      auto end_n2a = name2access.end();
      for(; i_n2a != end_n2a; ++i_n2a) {
        // This compiles and runs but does not successfully identify any genericInterfaceBaseC members.
        std::shared_ptr<genericInterfaceBaseC> ptr = i_n2a->second->getInterfacePtr(dynamic_cast<genericInterfaceBaseC*>(self));
        bool got_one = (ptr != nullptr);
        if(got_one)
          Rprintf("HOORAY: field %s is genericInterfaceBaseC\n", i_n2a->first.c_str());
        else
          Rprintf("field %s is NOT a genericInterfaceBaseC\n", i_n2a->first.c_str()); 
      }
  }
  /*
    mv name2access typedefs to the base class.
    create virtual accessor function for name2access.
    check on what "Derived" is here.
  */
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
class nodeFunctionClass_ : public nodeFunctionClassBase_ {
  public:
  double v;
  virtual ~nodeFunctionClass_() {};
};

#endif // NCOMPILER_MODEL_BASE_DEVEL_H_
