template<class Element>
class nList2_ : public nList2Base_nClass {
public:
    nList2_() {};
    std::vector<Element> contents_;
    std::vector<Element> &contents() {return contents_;}
    const std::vector<Element> &contents() const {return contents_;}
    virtual int setLength_(int length) {
        contents_.resize(length);
        return length;
    }
    virtual int getLength_() {
        return static_cast<int>(contents_.size());
    }
    size_t doubleBracket_inds_2_size_t(const Rcpp::RObject &inds, bool check_upper = true) {
      if (Rf_xlength(inds) != 1) {
        Rcpp::stop("single-bracket getter expects index of length 1");
      }

      int idx1 = 0; // 1-based index from R

      switch (inds.sexp_type()) {
        case INTSXP: {
          Rcpp::IntegerVector v(inds);   // view/wrapper, no coercion copy
          if (v[0] == NA_INTEGER) Rcpp::stop("index is NA");
          idx1 = v[0];
          break;
        }
        case REALSXP: {
          Rcpp::NumericVector v(inds);   // view/wrapper, no coercion copy
          double d = v[0];
          if (Rcpp::NumericVector::is_na(d) || !R_finite(d)) {
            Rcpp::stop("numeric index must be a a number");
          }
          idx1 = static_cast<int>(std::floor(d));
          break;
        }
        case LGLSXP: {
          Rcpp::LogicalVector v(inds);   // view/wrapper
          if (v[0] == NA_LOGICAL) Rcpp::stop("logical index is NA");
          if (!v[0]) Rcpp::stop("logical FALSE is invalid for single index");
          idx1 = 1; // TRUE -> first element
          break;
        }
        default:
          Rcpp::stop("index type must be integer, double, or logical");
      }

      if (idx1 < 1 || (check_upper ? static_cast<size_t>(idx1) > contents_.size() : false)) {
        Rcpp::stop("index out of bounds");
      }
      return static_cast<size_t>(idx1 - 1); // convert to 0-based
    }
    Element doubleBracket_get_(const Rcpp::RObject &inds) {
      size_t idx1 = doubleBracket_inds_2_size_t(inds);
      return contents_[idx1];
    }
    Element doubleBracket_set_(const Rcpp::RObject &inds, const Element &value) {
      size_t idx1 = doubleBracket_inds_2_size_t(inds, false);
      if(idx1 >= static_cast<int>(contents_.size())) contents_.resize(idx1 + 1);
      return (contents_[idx1] = value);
    }
    Element doubleBracket_get_cpp_(
            int i) {
        if(i < 1 || static_cast<size_t>(i) > contents_.size()) {
            throw std::out_of_range("index out of bounds");
        }
        return contents_[i-1];
    }
    Element doubleBracket_set_cpp_(
            int i,
            const Element& value) {
        if(i < 1) {
          throw std::out_of_range("index out of bounds");
        }
        if(i > static_cast<int>(contents_.size())) contents_.resize(i);
        return (contents_[i-1] = value);
    }

    std::vector<Element> singleBracket_get_(const Rcpp::RObject &inds) {
      std::vector<Element> res;
      switch (inds.sexp_type()) {
        case INTSXP: {
          // Integer indexing
          Rcpp::IntegerVector idx = Rcpp::as<Rcpp::IntegerVector>(inds);
          res.resize(idx.size());
          for(size_t i = 0; i < idx.size(); ++i) {
            if(idx[i] == NA_INTEGER) Rcpp::stop("index is NA");
            if(idx[i] < 1 || static_cast<size_t>(idx[i]) > contents_.size()) {
              Rcpp::stop("index out of bounds");
            }
            res[i] = contents_[static_cast<int>(idx[i]) - 1];
          }
          break;
        }
        case REALSXP: {
          // Numeric indexing
          Rcpp::NumericVector idx = Rcpp::as<Rcpp::NumericVector>(inds);
          res.resize(idx.size());
          for(size_t i = 0; i < idx.size(); ++i) {
            if(Rcpp::NumericVector::is_na(idx[i]) || !R_finite(idx[i])) {
              Rcpp::stop("numeric index must be a finite number");
            }
            size_t int_idx = static_cast<size_t>(std::floor(idx[i]));
            if(int_idx < 1 || int_idx > contents_.size()) {
              Rcpp::stop("index out of bounds");
            }
            res[i] = contents_[int_idx - 1];
          }
          break;
        }
        case LGLSXP: {
          Rcpp::LogicalVector bools = Rcpp::as<Rcpp::LogicalVector>(inds);
          size_t bools_n = static_cast<size_t>(bools.size());
          size_t contents_n = contents_.size();
          // Check for NA in the base logical vector (applies wherever it is recycled)
          for(size_t i = 0; i < bools_n; ++i) {
            if(bools[i] == NA_LOGICAL) Rcpp::stop("index is NA");
          }
          // Count TRUE positions, recycling bools over contents_n
          size_t n = 0;
          for(size_t i = 0; i < contents_n; ++i) {
            if(bools[i % bools_n]) ++n;
          }
          res.resize(n);
          size_t j = 0;
          for(size_t i = 0; i < contents_n; ++i) {
            if(bools[i % bools_n]) res[j++] = contents_[i];
          }
          break;
        }
        default:
          Rcpp::stop("index type must be integer, double, or logical");
      }
      return res;
    }

    template<typename VALUES>
    void singleBracket_set_(const Rcpp::RObject &inds, const VALUES &values) {
      size_t n = 0;
      if(values.size() == 0) Rcpp::stop("nList: replacement has length zero");
      size_t vals_n = static_cast<size_t>(values.size());
      switch (inds.sexp_type()) {
        case INTSXP: {
          Rcpp::IntegerVector idx = Rcpp::as<Rcpp::IntegerVector>(inds);
          n = static_cast<size_t>(idx.size());
          if(n == 0) return;
          if(n % vals_n != 0) {
            Rcpp::warning("nList: number of items to replace is not a multiple of replacement length");
          }
          size_t max_idx = 1;
          for(size_t i = 0; i < n; ++i) {
            if(idx[i] == NA_INTEGER) Rcpp::stop("nList: index is NA");
            if(idx[i] < 1) Rcpp::stop("nList: index out of bounds");
            max_idx = std::max(max_idx, static_cast<size_t>(idx[i]));
          }
          if(max_idx > contents_.size()) contents_.resize(max_idx);
          size_t j = 0;
          for(size_t i = 0; i < n; ++i, ++j) {
            if(j >= vals_n) j = 0; // recycling rule
            contents_[static_cast<size_t>(idx[i]) - 1] = values[j];
          }
          break;
        }
        case REALSXP: {
          Rcpp::NumericVector idx = Rcpp::as<Rcpp::NumericVector>(inds);
          n = static_cast<size_t>(idx.size());
          if(n == 0) return;
          if(n % vals_n != 0) {
            Rcpp::warning("nList: number of items to replace is not a multiple of replacement length");
          }
          size_t max_idx = 1;
          for(size_t i = 0; i < n; ++i) {
            if(Rcpp::NumericVector::is_na(idx[i]) || !R_finite(idx[i])) {
              Rcpp::stop("nList: numeric index must be a finite number");
            }
            int int_idx = static_cast<int>(std::floor(idx[i]));
            if(int_idx < 1) Rcpp::stop("nList: index out of bounds");
            max_idx = std::max(max_idx, static_cast<size_t>(int_idx));
          }
          if(max_idx > contents_.size()) contents_.resize(max_idx);
          size_t j = 0;
          for(size_t i = 0; i < n; ++i, ++j) {
            if(j >= vals_n) j = 0; // recycling rule
            contents_[static_cast<size_t>(std::floor(idx[i])) - 1] = values[j];
          }
          break;
        }
        case LGLSXP: {
          Rcpp::LogicalVector bools = Rcpp::as<Rcpp::LogicalVector>(inds);
          size_t bools_n = static_cast<size_t>(bools.size());
          // Extend contents_ if bools is longer (R semantics: x[c(T,F,T)] <- ... can extend x)
          if(bools_n > contents_.size()) contents_.resize(bools_n);
          size_t contents_n = contents_.size();
          // Check for NA in the base logical vector (applies wherever it is recycled)
          for(size_t i = 0; i < bools_n; ++i) {
            if(bools[i] == NA_LOGICAL) Rcpp::stop("nList: logical index is NA");
          }
          // Count TRUE positions, recycling bools over contents_n
          n = 0;
          for(size_t i = 0; i < contents_n; ++i) {
            if(bools[i % bools_n]) ++n;
          }
          if(n == 0) return;
          if(n % vals_n != 0) {
            Rcpp::warning("nList: number of items to replace is not a multiple of replacement length");
          }
          // Assign, recycling both bools (over contents_n) and values (over TRUE positions)
          size_t val_j = 0;
          for(size_t i = 0; i < contents_n; ++i) {
            if(bools[i % bools_n]) {
              contents_[i] = values[val_j % vals_n];
              ++val_j;
            }
          }
          break;
        }
        default:
          Rcpp::stop("nList: index type must be integer, double, or logical");
      }
    }
    void singleBracket_set_(const Rcpp::RObject &inds, const Rcpp::List &values) {
      singleBracket_set_<Rcpp::List>(inds, values);
    }
    void singleBracket_set_(const Rcpp::RObject &inds, const std::vector<Element> &values) {
      singleBracket_set_<std::vector<Element>>(inds, values);
    }
    void singleBracket_set_single_(const Rcpp::RObject &inds, const Element &value) {
      singleBracket_set_(inds, std::vector<Element>{value});
    }

    // C++-side single-bracket setter: accepts any 1-D Eigen tensor or unevaluated
    // Eigen op for inds, with Scalar == int, double, or bool.
    // VALUES must support .size() and operator[].
    // bool  → logical indexing (recycles over contents_, extends if bools longer)
    // int / double → 1-based positional indexing (doubles are floor()ed, extends contents_)
    // values are recycled over the selected positions, matching R semantics.
    template<typename EigenElement, int AccessLevel, typename VALUES>
    void singleBracket_set_cpp_(
            const Eigen::TensorBase<EigenElement, AccessLevel>& inds,
            const VALUES& values) {
        using Scalar = typename EigenElement::Scalar;
        if (values.size() == 0) Rcpp::stop("nList: replacement has length zero");
        size_t vals_n = static_cast<size_t>(values.size());
        Eigen::Tensor<Scalar, 1> evaluated = inds;
        size_t n = static_cast<size_t>(evaluated.size());

        if (std::is_same<Scalar, bool>::value) {
            // Logical indexing: extend contents_ if bools is longer, recycle over contents_n
            if (n > contents_.size()) contents_.resize(n);
            size_t contents_n = contents_.size();
            size_t count = 0;
            for (size_t i = 0; i < contents_n; ++i)
                if (evaluated(static_cast<Eigen::Index>(i % n))) ++count;
            if (count == 0) return;
            if (count % vals_n != 0)
                Rcpp::warning("nList: number of items to replace is not a multiple of replacement length");
            size_t val_j = 0;
            for (size_t i = 0; i < contents_n; ++i) {
                if (evaluated(static_cast<Eigen::Index>(i % n))) {
                    contents_[i] = values[val_j % vals_n];
                    ++val_j;
                }
            }
        } else {
            // Positional indexing (int or double), 1-based
            if (n == 0) return;
            if (n % vals_n != 0)
                Rcpp::warning("nList: number of items to replace is not a multiple of replacement length");
            // Validate and find max index before any assignment
            size_t max_idx = 1;
            for (size_t i = 0; i < n; ++i) {
                size_t idx1 = static_cast<size_t>(
                    std::floor(static_cast<double>(evaluated(static_cast<Eigen::Index>(i)))));
                if (idx1 < 1) Rcpp::stop("nList: index out of bounds");
                max_idx = std::max(max_idx, idx1);
            }
            if (max_idx > contents_.size()) contents_.resize(max_idx);
            for (size_t i = 0; i < n; ++i) {
                size_t idx1 = static_cast<size_t>(
                    std::floor(static_cast<double>(evaluated(static_cast<Eigen::Index>(i)))));
                contents_[idx1 - 1] = values[i % vals_n];
            }
        }
    }
    template<typename EigenElement, int AccessLevel>
    void singleBracket_set_nList_cpp_(
            const Eigen::TensorBase<EigenElement, AccessLevel>& inds,
            std::shared_ptr<nList2_<Element>> values) {
        singleBracket_set_cpp_(inds, values->contents());
    }
    template<typename EigenElement, int AccessLevel>
    void singleBracket_set_single_cpp_(
            const Eigen::TensorBase<EigenElement, AccessLevel>& inds,
            const Element& value) {
        singleBracket_set_cpp_(inds, std::vector<Element>{value});
    }

    // Superseded by doubleBracket_get_ / doubleBracket_set_:
    // Element setOne_(size_t i, const Element& v) {
    //     return (contents_[i-1] = v);
    // }
    // Element getOne_(size_t i) {
    //     return contents_[i-1];
    // }

    // Superseded by singleBracket_get_:
    // template<typename INDS>
    // std::vector<Element> getMany_(const INDS &inds) {
    //     std::vector<Element> res;
    //     res.resize(inds.size());
    //     for(size_t i = 0; i < inds.size(); ++i) {
    //         res[i] = contents_[inds[i] - 1];
    //     }
    //     return res;
    // }
    // template<typename INDS>
    // Rcpp::List getManyToList_(const INDS &inds) {
    //     Rcpp::List res(inds.size());
    //     for(size_t i = 0; i < inds.size(); ++i) {
    //         res[i] = contents_[inds[i] - 1];
    //     }
    //     return res;
    // }

    Rcpp::List as_list_() {
      Rcpp::List res(contents_.size());
      for(size_t i = 0; i < contents_.size(); ++i) {
          res[i] = contents_[i];
      }
      return res;
    }
    // Superseded by singleBracket_get_ (logical case):
    // template<typename BOOLS>
    // std::vector<Element> getManyLogical_(const BOOLS &bools) {
    //     // Eigen::Tensor<bool> lacks begin()/end(), so iterate by index
    //     size_t n = 0;
    //     for(size_t i = 0; i < bools.size(); ++i) if(bools[i]) n++;
    //     std::vector<Element> res(n);
    //     size_t j = 0;
    //     for(size_t i = 0; i < bools.size(); ++i) if(bools[i]) res[j++] = contents_[i];
    //     return res;
    // }
    // template<typename BOOLS>
    // Rcpp::List getManyToListLogical_(const BOOLS &bools) {
    //     size_t n = 0;
    //     for(size_t i = 0; i < bools.size(); ++i) if(bools[i]) n++;
    //     Rcpp::List res(n);
    //     size_t j = 0;
    //     for(size_t i = 0; i < bools.size(); ++i) if(bools[i]) res[j++] = contents_[i];
    //     return res;
    // }

    // Superseded by singleBracket_set_:
    // template<typename INDS, typename VALS>
    // VALS setMany_(const INDS &inds, const VALS &vals) {
    //     for(size_t i = 0; i < inds.size(); ++i) contents_[inds[i] - 1] = vals[i];
    //     return vals;
    // }
    // template<typename INDS>
    // Element setManySingle_(const INDS &inds, const Element &val) {
    //     for(size_t i = 0; i < inds.size(); ++i) contents_[inds[i] - 1] = val;
    //     return val;
    // }
    // template<typename INDS>
    // Rcpp::List setManyFromList_(const INDS &inds, const Rcpp::List &vals) {
    //     for(size_t i = 0; i < inds.size(); ++i) contents_[inds[i] - 1] = vals[i];
    //     return vals;
    // }
    // template<typename BOOLS, typename VALS>
    // VALS setManyLogical_(const BOOLS &bools, const VALS &vals) {
    //     size_t j = 0;
    //     for(size_t i = 0; i < bools.size(); ++i) if(bools[i]) contents_[i] = vals[j++];
    //     return vals;
    // }
    // template<typename BOOLS>
    // Rcpp::List setManyFromListLogical_(const BOOLS &bools, const Rcpp::List &vals) {
    //     size_t j = 0;
    //     for(size_t i = 0; i < bools.size(); ++i) if(bools[i]) contents_[i] = vals[j++];
    //     return vals;
    // }
    // template<typename BOOLS>
    // Element setManyLogicalSingle_(const BOOLS &bools, const Element &val) {
    //     for(size_t i = 0; i < bools.size(); ++i) if(bools[i]) contents_[i] = val;
    //     return val;
    // }

    // C++-side single-bracket getter: accepts any 1-D Eigen tensor or unevaluated
    // Eigen op with Scalar == int, double, or bool.
    // bool  → logical indexing (recycles over contents_, matching R semantics)
    // int / double → 1-based positional indexing (doubles are floor()ed)
    template<typename EigenElement, int AccessLevel>
    std::vector<Element> singleBracket_get_cpp_(
            const Eigen::TensorBase<EigenElement, AccessLevel>& inds) {
        using Scalar = typename EigenElement::Scalar;
        Eigen::Tensor<Scalar, 1> evaluated = inds;
        size_t n = static_cast<size_t>(evaluated.size());

        if (std::is_same<Scalar, bool>::value) {
            // Logical indexing: recycle bool tensor over contents_
            size_t contents_n = contents_.size();
            if (n == 0) return std::vector<Element>{};
            size_t count = 0;
            for (size_t i = 0; i < contents_n; ++i)
                if (evaluated(static_cast<Eigen::Index>(i % n))) ++count;
            std::vector<Element> res;
            res.reserve(count);
            for (size_t i = 0; i < contents_n; ++i)
                if (evaluated(static_cast<Eigen::Index>(i % n)))
                    res.push_back(contents_[i]);
            return res;
        } else {
            // Positional indexing (int or double), 1-based
            std::vector<Element> res;
            res.resize(n);
            for (size_t i = 0; i < n; ++i) {
                size_t idx1 = static_cast<size_t>(
                    std::floor(static_cast<double>(evaluated(static_cast<Eigen::Index>(i)))));
                if (idx1 < 1 || idx1 > contents_.size())
                    Rcpp::stop("nList: index out of bounds");
                res[i] = contents_[idx1 - 1];
            }
            return res;
        }
    }

    auto begin() noexcept       { return contents_.begin(); }
    auto begin() const noexcept { return contents_.begin(); }
    auto end() noexcept         { return contents_.end(); }
    auto end() const noexcept   { return contents_.end(); }
    size_t size() const noexcept { return contents_.size(); }
    auto& operator[](size_t i) { return contents_[i]; }
    const auto& operator[](size_t i) const { return contents_[i]; }
};
