#ifndef NCOMPILED_ETACCESSOR_PRE_H_
#define NCOMPILED_ETACCESSOR_PRE_H_

#include <memory>

class ETaccessorBase;

// Declared here (before Rcpp.h) so that wrap/as calls inside Rcpp's own
// templates can find them. Definitions are in post_Rcpp/ETaccessor_as_wrap.h.
namespace Rcpp {
  // This Exporter implements Rcpp::as< std::unique_ptr<ETaccessorBase> >
  namespace traits {
    template <>
    class Exporter< std::unique_ptr<ETaccessorBase> >;
  }

  // Implement Rcpp::wrap( std::unique_ptr<ETaccessorBase> ).
  SEXP wrap( const std::unique_ptr<ETaccessorBase> &x );
}

#endif // NCOMPILED_ETACCESSOR_PRE_H_
