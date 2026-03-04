template<class Derived>
class nList2_ : public nList2Base_nClass {
public:
    nList2_() {};
    std::vector<Derived> contents_;
    std::vector<Derived> &contents() {return contents_;}
    const std::vector<Derived> &contents() const {return contents_;}
    virtual int setLength_(int length) {
        contents_.resize(length);
        return length;
    }
    virtual int getLength_() {
        return contents_.size();
    }
    Derived setOne_(size_t i, const Derived& v) {
        // add error trapping
        return (contents_[i-1] = v);
    }
    Derived getOne_(size_t i) {
        // add error trapping
        return contents_[i-1];
    }
    template<typename INDS>
    std::vector<Derived> getMany_(const INDS &inds) {
        // To-do: we would need to scan over inds if we want to
        // respect omitting zeros. Currently we ignore.
        std::vector<Derived> res;
        res.resize(inds.size());
        for(size_t i = 0; i < inds.size(); ++i) {
            res[i] = contents_[inds[i] - 1];
        }
        return res;
    }
    template<typename INDS>
    Rcpp::List getManyToList_(const INDS &inds) {
        // To-do: we would need to scan over inds if we want to
        // respect omitting zeros. Currently we ignore.
        Rcpp::List res(inds.size());
        for(size_t i = 0; i < inds.size(); ++i) {
            res[i] = contents_[inds[i] - 1];
        }
        return res;
    }
    template<typename BOOLS>
    std::vector<Derived> getManyLogical_(const BOOLS &bools) {
        // oops, Eigen::Tensor<bool> does not having begin() and end(), so we do it ourselves
        // size_t n = std::count(bools.begin(), bools.end(), true);
        size_t n = 0;
        for(size_t i = 0; i < bools.size(); ++i) {
            if(bools[i]) n++;
        }
        std::vector<Derived> res;
        res.resize(n);
        size_t j = 0;
        for(size_t i = 0; i < bools.size(); ++i) {
            if(bools[i]) {
                res[j++] = contents_[i];
            }
        }
        return res;
    }
    template<typename BOOLS>
    Rcpp::List getManyToListLogical_(const BOOLS &bools) {
        size_t n = 0;
        for(size_t i = 0; i < bools.size(); ++i) {
            if(bools[i]) n++;
        }
        Rcpp::List res(n);
        size_t j = 0;
        for(size_t i = 0; i < bools.size(); ++i) {
            if(bools[i]) {
                res[j++] = contents_[i];
            }
        }
        return res;
    }
    // setMany_ with a vector of values
    // To-do: do we want recycling rule behavior?
    template<typename INDS, typename VALS>
    VALS setMany_(const INDS &inds, const VALS &vals) {
        for(size_t i = 0; i < inds.size(); ++i) {
            contents_[inds[i] - 1] = vals[i];
        }
        return vals;
    }
    // setMany_ from a single value
    template<typename INDS>
    Derived setManySingle_(const INDS &inds, const Derived &val) {
        for(size_t i = 0; i < inds.size(); ++i) {
            contents_[inds[i] - 1] = val;
        }
        return val;
    }
    // setManyFromList_ from a list of values
    // To-do: could this use the VALS template above?
    template<typename INDS>
    Rcpp::List setManyFromList_(const INDS &inds, const Rcpp::List &vals) {
        for(size_t i = 0; i < inds.size(); ++i) {
            contents_[inds[i] - 1] = vals[i];
        }
        return vals;
    }
    
    // setManyLogical_ with a vector of values
    template<typename BOOLS, typename VALS>
    VALS setManyLogical_(const BOOLS &bools, const VALS &vals) {
        size_t j = 0;
        for(size_t i = 0; i < bools.size(); ++i) {
            if(bools[i]) {
                contents_[i] = vals[j++];
            }
        }
        return vals;
    }
    // setManyFromListLogical_
    template<typename BOOLS>
    Rcpp::List setManyFromListLogical_(const BOOLS &bools, const Rcpp::List &vals) {
        size_t j = 0;
        for(size_t i = 0; i < bools.size(); ++i) {
            if(bools[i]) {
                contents_[i] = vals[j++];
            }
        }
        return vals;
    }
    // setManyLogical_ from a single value
    template<typename BOOLS>
    Derived setManyLogicalSingle_(const BOOLS &bools, const Derived &val) {
        for(size_t i = 0; i < bools.size(); ++i) {
            if(bools[i]) {
                contents_[i] = val;
            }
        }
        return val;
    }
    auto begin() noexcept {
        return contents_.begin();
    }
    auto begin() const noexcept {
        return contents_.begin();
    }
    auto end() noexcept {
        return contents_.end();
    }
    auto end() const noexcept {
        return contents_.end();
    }
    auto& operator[](size_t i) {
        return contents_[i]; }
    const auto& operator[](size_t i) const {
        return contents_[i]; }

};
