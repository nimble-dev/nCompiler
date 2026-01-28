#include<iostream>
#include<sstream>

#define PRINTF Rprintf
#define NERROR Rf_error

std::ostringstream _nCompiler_global_output;

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

void nCompiler_print_to_R(std::ostringstream &input) {
  PRINTF("%s", input.str().c_str());
  input.str("");
  input.clear();
}

void Rmessage(LogLevel level, std::ostringstream &input) {
  Rcpp::Environment nc = Rcpp::Environment::namespace_env("nCompiler");
  Rcpp::Function message = nc["nMessage"];
  message(int(level), input.str().c_str());
  input.str("");
  input.clear();  
  return;
}

void Rwarning(std::ostringstream &input) {
  Rcpp::Environment base = Rcpp::Environment::namespace_env("base");
  Rcpp::Function warning = base["warning"];
  warning(input.str().c_str());
  input.str("");
  input.clear();  
  return;
}

void nStop(std::ostringstream &input) {
  NERROR("%s", input.str().c_str());
  input.str("");
  input.clear();
}


#ifndef _NC_UTILS_
#define _NC_UTILS_

inline double nc_mod(double a, double b) {return(fmod(a, b));}

inline double nc_cube(double a) {return a*a*a;}
inline double nc_square(double a) {return a*a;}

inline double cwiseMin(double a, double b) {return (a <= b ? a : b);}
inline double cwiseMax(double a, double b) {return (a >= b ? a : b);}

#endif
