#ifndef SHARED_PTR_AS_WRAP_FORWARD_DECLARATIONS_H_
#define SHARED_PTR_AS_WRAP_FORWARD_DECLARATIONS_H_

#include <memory>
#include <utility>

// Defined here (not just forward-declared) because loadedObjectHookC_impl.h
// (included via the _post_Rcpp path before shared_ptr_as_wrap.h) needs the
// complete type: it accesses the nested nCwrapMode::mode enum and calls
// .get() in a class-template member body, which requires more than an
// incomplete type even though nothing has been instantiated yet. This class
// has no Rcpp dependency, so it's safe to define this early.
class nCwrapMode {
  public:
    enum class mode {full, generic};
    static constexpr mode full = mode::full;
    static constexpr mode generic = mode::generic;
    nCwrapMode(mode m) : m_(m) {}
    nCwrapMode(bool m) : m_(m ? mode::full : mode::generic) {}
    mode get() const { return m_; }
  private:
    mode m_;
};

namespace Rcpp {
namespace traits {
  template <typename T>
    class Exporter< std::shared_ptr< T > >;
  }
}

namespace Rcpp {
  template<typename T>
    SEXP wrap( std::shared_ptr< T > obj );

  // Forward-declared here (definition in shared_ptr_as_wrap.h) for the same
  // reason as the shared_ptr overload above: accessor_class_aux::get(), in
  // generic_class_interface_Rcpp_steps.h, calls this as a *qualified* name
  // (Rcpp::wrap), which disables ADL. Two-phase lookup then fixes the
  // overload set to whatever's visible at accessor_class_aux::get()'s
  // definition point, included well before shared_ptr_as_wrap.h -- without
  // this forward declaration, that call can never find this overload no
  // matter what it's instantiated with, since no rescue lookup happens at
  // instantiation time for qualified calls.
  template<typename T>
    SEXP wrap( std::pair<std::shared_ptr< T >, nCwrapMode> pair_obj );
}

#endif // SHARED_PTR_AS_WRAP_FORWARD_DECLARATIONS_H_
