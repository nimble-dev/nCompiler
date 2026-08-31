#include<iostream>
#include<sstream>


#define PRINTF Rprintf
#define NERROR Rf_error

using std::string;

inline std::ostringstream _nCompiler_global_output;

// Values as defined by R `logger` package.
typedef enum {
  FATAL = 100,
  ERROR = 200,
  WARN = 300,
  SUCCESS = 350,
  INFO = 400,
  DEBUG = 500,
  TRACE = 600
} LogLevel;

inline void nCompiler_print_to_R(std::ostringstream &input) {
  PRINTF("%s", input.str().c_str());
  input.str("");
  input.clear();
}

inline void Rmessage_old(LogLevel level, std::ostringstream &input) {
  Rcpp::Environment nc = Rcpp::Environment::namespace_env("nCompiler");
  Rcpp::Function message = nc["nMessage"];
  message(int(level), input.str().c_str());
  input.str("");
  input.clear();  
  return;
}

template<bool add_newline = false>
void nMessage_(std::ostringstream &output) {
    if constexpr (add_newline) Rcpp::Rcout<<output.str()<<"\n";
    else Rcpp::Rcout<<output.str();
}

inline void nWarning_(std::ostringstream &output) {
    Rcpp::warning(output.str());
}

inline void nStop_(std::ostringstream &output) {
  Rcpp::stop(output.str());
}

inline void Rprogress_bar(string msg, int total) {
    Rcpp::Environment cli = Rcpp::Environment::namespace_env("cli");
    Rcpp::Function progress_bar = cli["cli_progress_bar"];
    progress_bar("", msg, "iterator", total);
    return;
}

inline void Rprogress_update() {
    Rcpp::Environment cli = Rcpp::Environment::namespace_env("cli");
    Rcpp::Function progress_update = cli["cli_progress_update"];
    progress_update();
    return;
}

#ifndef _NC_UTILS_
#define _NC_UTILS_

inline double nc_mod(double a, double b) {return(fmod(a, b));}

inline double nc_cube(double a) {return a*a*a;}
inline double nc_square(double a) {return a*a;}

inline double cwiseMin(double a, double b) {return (a <= b ? a : b);}
inline double cwiseMax(double a, double b) {return (a >= b ? a : b);}

#endif
