#ifndef _NCOMPILER_N_OPTIM
#define _NCOMPILER_N_OPTIM

// R's API to optimization methods
#include <R_ext/Applic.h>

#define QUOTEME(A) #A
#define PREDEFINED_HEADER(PH) QUOTEME(PH.h)

#ifdef PREDEFINED_OptimControlList
#include PREDEFINED_HEADER(PREDEFINED_OptimControlList)
std::shared_ptr<OptimControlList> make_OptimControlList() {
    return(std::shared_ptr<OptimControlList>(new OptimControlList));
}
#endif

#ifdef PREDEFINED_OptimResultList
#include PREDEFINED_HEADER(PREDEFINED_OptimResultList)
std::shared_ptr<OptimResultList> make_OptimResultList() {
    return(std::shared_ptr<OptimResultList>(new OptimResultList));
}
#endif

#ifdef PREDEFINED_OptimControlList
#include PREDEFINED_HEADER(PREDEFINED_OptimControlList)
#ifdef PREDEFINED_OptimResultList
#include PREDEFINED_HEADER(PREDEFINED_OptimResultList)

namespace nCompiler {

    // ---------------------------------------------------------------------------
    // This class and wrapper are equivalent to std::bind(-,-) in C++11, so that:
    //   object->method(par) == NimBoundMethod<T>(&T::method, object)(par);
    //                       == NimBind(&T::method, object)(par);
    //                       == std::bind(&T::method, object)(par);
    template <class T, typename Arg, typename Result>
            class NBoundMethod {
            public:
                NBoundMethod(Result (T::*method)(Arg), T* object) :
                    method_(method), object_(object) {}
                Result operator()(Arg par) {
                    return (object_->*method_)(par);
                }

            private:
                Result (T::*method_)(Arg);
                T* object_;
            };

    template <class T>
            inline NBoundMethod<T, Eigen::Tensor<double, 1>&, double> nBind(
                double (T::*method)(Eigen::Tensor<double, 1>&), T* object) {
                return NBoundMethod<T, Eigen::Tensor<double, 1>&, double>(
                    method, object
                );
            }

    template <class T>
            inline NBoundMethod<T, Eigen::Tensor<double, 1>, double> nBind(
                    double (T::*method)(Eigen::Tensor<double, 1>), T* object) {
                return NBoundMethod<T, Eigen::Tensor<double, 1>, double>(
                    method, object
                );
            }

    template <class T>
        inline NBoundMethod<T, Eigen::Tensor<double, 1>&,
                            Eigen::Tensor<double, 1>> nBind(
            Eigen::Tensor<double, 1> (T::*method)(Eigen::Tensor<double, 1>&),
                                                  T* object) {
                return NBoundMethod<T,
                                    Eigen::Tensor<double, 1>&,
                                    Eigen::Tensor<double, 1>>(method, object);
            }

    template <class T>
        inline NBoundMethod<T, Eigen::Tensor<double, 1>,
                            Eigen::Tensor<double, 1>> nBind(
            Eigen::Tensor<double, 1> (T::*method)(Eigen::Tensor<double, 1>),
                                                  T* object) {
                return NBoundMethod<T,
                                    Eigen::Tensor<double, 1>,
                                    Eigen::Tensor<double, 1>>(method, object);
            }

    // ---------------------------------------------------------------------------
    // NimOptimProblem class hierarchy

    class OptimProblem {
    public:
        OptimProblem(const std::string& method, Eigen::Tensor<double, 1>& lower,
                     Eigen::Tensor<double, 1>& upper,
                     std::shared_ptr<OptimControlList> control, bool hessian)
                     : method_(method),
                     lower_(lower),
                     upper_(upper),
                     control_(control),
                     hessian_(hessian) {}

                     std::shared_ptr<OptimResultList> solve(Eigen::Tensor<double, 1>& par);

    private:
        // These function and gradient callbacks for R's optim() where `this` is
        // passed in as the final argument `void * ex`.
        static double fn(int, double*, void*);
        static void gr(int, double*, double*, void*);

    protected:
        // These are callbacks used internally by fn() and gr().
        virtual double function() = 0;
        virtual void gradient() = 0;

        // Problem parameters.
        const std::string& method_;
        Eigen::Tensor<double, 1>& lower_;
        Eigen::Tensor<double, 1>& upper_;
        std::shared_ptr<OptimControlList> control_;
        const bool hessian_;

        // Temporaries.
        Eigen::Tensor<double, 1> par_;  // Argument for fn() and gr().
        Eigen::Tensor<double, 1> ans_;  // Result of gradient.
    };

    template <class Fn>
            class OptimProblem_Fun : public OptimProblem {
            public:
                OptimProblem_Fun(Fn fn, const std::string& method,
                                 Eigen::Tensor<double, 1>& lower,
                                 Eigen::Tensor<double, 1>& upper,
                                 std::shared_ptr<OptimControlList> control,
                                 bool hessian)
                                 : OptimProblem(method, lower, upper, control, hessian), fn_(fn) {}

            protected:
                virtual double function() { return fn_(par_); }
                virtual void gradient();  // Uses finite difference approximation.

            private:
                Fn fn_;
            };

    template <class Fn>
            void OptimProblem_Fun<Fn>::gradient() {
                const int n = par_.size();
                // TODO: NIM_ASSERT_SIZE(ans_, n);
                Eigen::Tensor<double, 1>& ndeps = control_->ndeps;
                if (ndeps.size() == 1) {
                    double fill = ndeps[0];
                    ndeps.resize(n);
                    ndeps.setConstant(fill);
                }
                // TODO: NIM_ASSERT_SIZE(ndeps, n);
                Eigen::Tensor<double, 1> par_h = par_;
                if (method_ == "L-BFGS-B") {
                    // Constrained optimization.
                    for (int i = 0; i < n; ++i) {
                        const double h_pos = std::min(ndeps[i], upper_[i] - par_[i]);
                        const double h_neg = std::min(ndeps[i], par_[i] - lower_[i]);
                        par_h[i] = par_[i] + h_pos;
                        const double pos = fn_(par_h);
                        par_h[i] = par_[i] - h_neg;
                        const double neg = fn_(par_h);
                        par_h[i] = par_[i];
                        ans_[i] = (pos - neg) / (h_pos + h_neg);
                    }
                } else {
                    // Unconstrained optimization.
                    for (int i = 0; i < n; ++i) {
                        const double h = ndeps[i];
                        par_h[i] = par_[i] + h;
                        const double pos = fn_(par_h);
                        par_h[i] = par_[i] - h;
                        const double neg = fn_(par_h);
                        par_h[i] = par_[i];
                        ans_[i] = (pos - neg) / (2 * h);
                    }
                }
            }

            template <class Fn, class Gr>
                    class OptimProblem_Fun_Grad : public OptimProblem {
                        public:
                            OptimProblem_Fun_Grad(Fn fn, Gr gr, const std::string& method,
                                                  Eigen::Tensor<double, 1>& lower,
                                                  Eigen::Tensor<double, 1>& upper,
                                                  std::shared_ptr<OptimControlList> control,
                                                  bool hessian)
                                                  : OptimProblem(method, lower, upper, control, hessian),
                                                  fn_(fn),
                                                  gr_(gr) {}

                        protected:
                            virtual double function() { return fn_(par_); }
                            virtual void gradient() { ans_ = gr_(par_); }

                        private:
                            Fn fn_;
                            Gr gr_;
                        };

    double OptimProblem::fn(int n, double* par, void* ex) {
        OptimProblem* problem = static_cast<OptimProblem*>(ex);
        problem->par_.resize(n);
        std::copy(par, par + n, problem->par_.data());
        return problem->function() / problem->control_->fnscale;
    }

    void OptimProblem::gr(int n, double* par, double* ans, void* ex) {
        OptimProblem* problem = static_cast<OptimProblem*>(ex);
        problem->par_.resize(n);
        std::copy(par, par + n, problem->par_.data());
        problem->ans_.resize(n);
        problem->gradient();
        for (int i = 0; i < n; ++i) {
            ans[i] = problem->ans_[i] / problem->control_->fnscale;
        }
    }

    // This attempts to match the behavior of optim() defined in the documentation
    //   https://stat.ethz.ch/R-manual/R-devel/library/stats/html/optim.html
    // and in the reference implementation
    //   https://svn.r-project.org/R/trunk/src/library/stats/R/optim.R
    //   https://svn.r-project.org/R/trunk/src/library/stats/src/optim.c
    //   https://svn.r-project.org/R/trunk/src/include/R_ext/Applic.h
    std::shared_ptr<OptimResultList> OptimProblem::solve(
            Eigen::Tensor<double, 1>& par) {
        // TODO: NIM_ASSERT1(!par.isMap(), "Internal error: failed to handle mapped NimArr");
        const int n = par.size();

        std::shared_ptr<OptimResultList> result = make_OptimResultList();
        result->par = par;
        result->counts.resize(2);
        result->counts.setConstant(NA_INTEGER);
        if (hessian_) {
            result->hessian.resize(n,n);
            result->hessian.setConstant(NA_REAL);
        }

        // Set context-dependent default control_ values.
        if (control_->maxit == NA_INTEGER) {
            if (method_ == "Nelder-Mead") {
                control_->maxit = 500;
            } else {
                control_->maxit = 100;
            }
        }

        // Parameters common to all methods.
        double* dpar = par.data();
        double* X = result->par.data();
        double* Fmin = &(result->value);
        int* fail = &(result->convergence);
        void* ex = this;
        int* fncount = &(result->counts[0]);
        int* grcount = &(result->counts[1]);

        if (method_ == "Nelder-Mead") {
            nmmin(n, dpar, X, Fmin, OptimProblem::fn, fail, control_->abstol,
                  control_->reltol, ex, control_->alpha, control_->beta,
                  control_->gamma, control_->trace, fncount, control_->maxit);
        } else if (method_ == "BFGS") {
            std::vector<int> mask(n, 1);
            vmmin(n, dpar, Fmin, OptimProblem::fn, OptimProblem::gr,
                  control_->maxit, control_->trace, mask.data(), control_->abstol,
                  control_->reltol, control_->REPORT, ex, fncount, grcount, fail);
            result->par = par;
        } else if (method_ == "CG") {
            cgmin(n, dpar, X, Fmin, OptimProblem::fn, OptimProblem::gr, fail,
                  control_->abstol, control_->reltol, ex, control_->type,
                  control_->trace, fncount, grcount, control_->maxit);
        } else if (method_ == "L-BFGS-B") {
            if (lower_.size() == 1) {
                lower_.resize(n);
                lower_.setConstant(lower_[0]);
            }
            if (upper_.size() == 1) {
                upper_.resize(n);
                upper_.setConstant(upper_[0]);
            }
            // TODO: NIM_ASSERT_SIZE(lower_, n);
            // TODO: NIM_ASSERT_SIZE(upper_, n);
            std::vector<int> nbd(n, 0);
            for (int i = 0; i < n; ++i) {
                if (std::isfinite(lower_[i])) nbd[i] |= 1;
                if (std::isfinite(upper_[i])) nbd[i] |= 2;
            }
            char msg[60];
            lbfgsb(n, control_->lmm, X, lower_.data(), upper_.data(),
                   nbd.data(), Fmin, OptimProblem::fn, OptimProblem::gr, fail,
                   ex, control_->factr, control_->pgtol, fncount, grcount,
                   control_->maxit, msg, control_->trace, control_->REPORT);
            result->message = msg;
        } else {
            // TODO: NIMERROR("Unknown method_: %s", method_.c_str());
        }
        result->value *= control_->fnscale;

        // Compute Hessian.
        if (hessian_) {
            Rf_warning("Hessian computation is not implemented");  // TODO
        }

        return result;
    }

    // ---------------------------------------------------------------------------
    // These nimOptim() and nimOptimDefaultControl() will appear in generated code

    template <class Fn, class Gr>
            inline std::shared_ptr<OptimResultList> nOptim(
                    Eigen::Tensor<double, 1>& par, Fn fn, Gr gr, const std::string& method,
                    Eigen::Tensor<double, 1>& lower, Eigen::Tensor<double, 1>& upper,
                    std::shared_ptr<OptimControlList> control, bool hessian) {
        return OptimProblem_Fun_Grad<Fn, Gr>(fn, gr, method, lower, upper,
                                             control, hessian)
                                             .solve(par);
    }

    // This handles the special case where gr is not specified (i.e. is "NULL").
    template <class Fn>
            inline std::shared_ptr<OptimResultList> nOptim(
                    Eigen::Tensor<double, 1>& par, Fn fn, const char* gr,
                    const std::string& method,
                    Eigen::Tensor<double, 1>& lower, Eigen::Tensor<double, 1>& upper,
                    std::shared_ptr<OptimControlList> control, bool hessian) {
        // TODO: NIM_ASSERT1(
        //    strcmp(gr, "NULL") == 0,
        //    "Internal error: failed to handle gradient argument type in optim()");
        return OptimProblem_Fun<Fn>(fn, method, lower, upper, control, hessian)
        .solve(par);
    }

    // TODO Handle double -> vector conversion in R instead of here.
    template <class Fn, class Gr>
            inline std::shared_ptr<OptimResultList> nOptim(
                    Eigen::Tensor<double, 1>& par, Fn fn, Gr gr, const std::string& method,
                    double lower, double upper, std::shared_ptr<OptimControlList> control,
                    bool hessian) {
        Eigen::Tensor<double, 1> lower_vector;
        lower_vector.resize(1);
        lower_vector.setConstant(lower);
        Eigen::Tensor<double, 1> upper_vector;
        upper_vector.resize(1);
        upper_vector.setConstant(upper);
        return OptimProblem_Fun_Grad<Fn, Gr>(fn, gr, method, lower_vector,
                                             upper_vector, control, hessian)
                                             .solve(par);
    }

    // TODO Handle double -> vector conversion in R instead of here.
    // This handles the special case where gr is not specified (i.e. is "NULL").
    template <class Fn>
            inline std::shared_ptr<OptimResultList> nOptim(
                    Eigen::Tensor<double, 1>& par, Fn fn, const char* gr,
                    const std::string& method,
                    double lower, double upper, std::shared_ptr<OptimControlList> control,
                    bool hessian) {
        // TODO: NIM_ASSERT1(
        //    strcmp(gr, "NULL") == 0,
        //    "Internal error: failed to handle gradient argument type in optim()");
        Eigen::Tensor<double, 1> lower_vector;
        lower_vector.resize(1);
        lower_vector.setConstant(lower);
        Eigen::Tensor<double, 1> upper_vector;
        upper_vector.resize(1);
        upper_vector.setConstant(upper);
        return OptimProblem_Fun<Fn>(fn, method, lower_vector, upper_vector,
                                    control, hessian)
                                    .solve(par);
    }

}
#endif
#endif


#endif
