#ifndef _NC_UTILS_
#define _NC_UTILS_

#include<iostream>
#include<sstream>


#define PRINTF Rprintf
#define NERROR Rf_error

using std::string;

// Output operators (nCat, nPrint, nStop, nWarning, nMessage, etc.).
// Generated code streams the arguments into a local std::ostringstream and passes it to one of these.
// A local stream (instead of one global stream, as in nimble) needs no clean-up and is thread-safe.
// Output goes through Rcpp's tools.

inline void nCout_(const std::ostringstream &output) {
  Rcpp::Rcout << output.str();
}

inline void nStop_(const std::ostringstream &output) {
  // Rcpp::stop throws a C++ exception, so destructors run and the cppStacktrace error trap can catch it.
  Rcpp::stop(output.str());
}

inline void nWarning_(const std::ostringstream &output) {
  // Pass the message as an argument, not as the format string, so a '%' in it is not interpreted.
  Rcpp::warning("%s", output.str());
}

inline void nMessage_(const std::ostringstream &output, int level) {
  // Call the R function nMessage, which uses the `logger` system and handles an active progress bar.
  // The function is looked up once.
  static Rcpp::Function Rmessage = Rcpp::Environment::namespace_env("nCompiler")["nMessage"];
  Rmessage(output.str(), Rcpp::Named("level") = level);
}

// Progress bar via R `cli` package. The functions are looked up once.
inline void Rprogress_bar(const string &msg, int total) {
  static Rcpp::Function progress_bar = Rcpp::Environment::namespace_env("cli")["cli_progress_bar"];
  progress_bar("", msg, "iterator", total);
}

inline void Rprogress_update() {
  static Rcpp::Function progress_update = Rcpp::Environment::namespace_env("cli")["cli_progress_update"];
  progress_update();
}

inline double nc_mod(double a, double b) {return(fmod(a, b));}

inline double nc_cube(double a) {return a*a*a;}
inline double nc_square(double a) {return a*a;}

inline double cwiseMin(double a, double b) {return (a <= b ? a : b);}
inline double cwiseMax(double a, double b) {return (a >= b ? a : b);}

#endif
