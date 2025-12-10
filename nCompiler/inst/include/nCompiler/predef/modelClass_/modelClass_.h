// to be included from the predefined modelBase_nClass.
// Add "#include <nCompiler/predef/modelClass_/modelClass_.h>" to that file,
// after the declaration of modelBase_nClass.


template<class Derived>
class modelClass_ : public modelBase_nClass {
public:
    modelClass_() {};
    std::vector< std::shared_ptr<nodeFxnBase_nClass> > nodeFxnPtrs;
    std::map<std::string, size_t> name2index_map;
    double calculate(std::shared_ptr<calcInstrList_nClass> calcInstrList) override {
        double logProb(0.0);
        const auto& calcInstrVec = calcInstrList->calcInstrList.get();
        auto calcInstr = calcInstrVec.cbegin();
        auto calcInstrEnd = calcInstrVec.cend();
        for( ; calcInstr != calcInstrEnd; ++calcInstr) {
            auto nodeFxnPtr = nodeFxnPtrs[(*calcInstr)->nodeIndex-1];
            const auto& nodeInstrVec = (*calcInstr)->nodeInstrVec.get();
            auto nodeInstr = nodeInstrVec.cbegin();
            auto nodeInstrEnd = nodeInstrVec.cend();
            for( ; nodeInstr != nodeInstrEnd; ) {
                logProb += nodeFxnPtr->calculate(*nodeInstr++);
            }
        }
        return(logProb);
    }
    
    // This version takes a character vector of names from R so that
    // the ordering of nodeFxns matches that in R, which is important for
    // the calculation instructions.
    void do_setup_node_mgmt_from_names(Rcpp::CharacterVector names)   {
        Rprintf("Attempting setup_node_mgmt_from_names with %d names\n", (int)names.length());
        Derived *self = static_cast<Derived*>(this);
        const auto& name2access = self->get_name2access();
        nodeFxnPtrs.clear();
        name2index_map.clear();
        size_t n = names.length();
        for(size_t i = 0; i < n; ++i) {
            std::string name = Rcpp::as<std::string>(names[i]);
            auto it = name2access.find(name);
            if(it != name2access.end()) {
                std::shared_ptr<genericInterfaceBaseC> ptr = it->second->getInterfacePtr(dynamic_cast<genericInterfaceBaseC*>(self));
                // When looking up this way, we do expect always to find objects (ptr valid) and that they are nodeFxn ptrs (ptr2 valid).
                // So we can turn these messages into errors once things are working.
                bool got_one = (ptr != nullptr);
                if(got_one) {
                    Rprintf("HOORAY: field %s is genericInterfaceBaseC\n", name.c_str());
                    std::shared_ptr<nodeFxnBase_nClass> ptr2 = std::dynamic_pointer_cast<nodeFxnBase_nClass>(ptr);
                    bool step_two = (ptr2 != nullptr);
                    if(step_two) {
                        Rprintf("AND IT IS A NODEFXN PTR!\n");
                        name2index_map.emplace(name, nodeFxnPtrs.size());
                        nodeFxnPtrs.push_back(ptr2);
                    } else {
                        Rprintf("but it is not a nodefxn ptr\n");
                    }
                } else {
                    Rprintf("field %s is NOT a genericInterfaceBaseC\n", name.c_str());
                }
            }
        }
    }

    // This version scans all members to find nodeFxns.
    // The resulting ordering comes from the order of the name2access map,
    // and so may not match R. This was written first but may fall out of common use.
    void setup_node_mgmt() {
        Derived *self = static_cast<Derived*>(this);
        const auto& name2access = self->get_name2access();
        size_t n = name2access.size();
        Rprintf("There are %d member variables indexed:\n", (int)n);
        auto i_n2a = name2access.begin();
        auto end_n2a = name2access.end();
        nodeFxnPtrs.clear();
        name2index_map.clear();
        size_t index = 0;
        for(; i_n2a != end_n2a; ++i_n2a) {
            std::shared_ptr<genericInterfaceBaseC> ptr = i_n2a->second->getInterfacePtr(dynamic_cast<genericInterfaceBaseC*>(self));
            bool got_one = (ptr != nullptr);
            if(got_one) {
                Rprintf("HOORAY: field %s is genericInterfaceBaseC\n", i_n2a->first.c_str());
                std::shared_ptr<nodeFxnBase_nClass> ptr2 = std::dynamic_pointer_cast<nodeFxnBase_nClass>(ptr);
                bool step_two = (ptr2 != nullptr);
                if(step_two) {
                    Rprintf("AND IT IS A NODEFXN PTR!\n");
                    nodeFxnPtrs.push_back(ptr2);
                    name2index_map.emplace(i_n2a->first, index++);
                } else {
                    Rprintf("but it is not a nodefxn ptr\n");
                }
            }
            else
                Rprintf("field %s is NOT a genericInterfaceBaseC\n", i_n2a->first.c_str());
        }
    }
    void c_print_nodes() {
        auto i_n2i = name2index_map.begin();
        auto end_n2i = name2index_map.end();
        Rprintf("0-based index: name\n");
        for(; i_n2i != end_n2i; ++i_n2i) {
            Rprintf("%d: %s\n", i_n2i->first.c_str(), (int)i_n2i->second);
        }
    }
    void set_from_list(Rcpp::List Rlist) {
        Rcpp::CharacterVector Rnames = Rlist.names();
        size_t len = Rnames.length();
        for(size_t i = 0; i < len; ++i) {
            // explicit cast is needed because even though Rnames[i] can cast to a string,
            // set_value takes a const string& so we need an object in place here.
            // set_value fails safely if a name is not found.
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
            // if the name was not found, a "Problem:" message was emitted, and we skip using it here.
            if(ETA) {
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
    }
};