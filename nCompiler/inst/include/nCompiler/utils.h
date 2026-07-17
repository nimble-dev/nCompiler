inline int getNumThreads(double value_) {
  static int nThreads_nOption=0;   // This variable is preserved across calls.
  if(TBB_DEPTH==1) {   // Avoid calls to R API within threads.
     Rcpp::Environment nc = Rcpp::Environment::namespace_env("nCompiler");
     Rcpp::Function get_nOption = nc["get_nOption"];
     nThreads_nOption=Rcpp::as<int>(get_nOption("nThreads"));
  }
  int value = (int) value_;
  if (nThreads_nOption > 0)
    value = nThreads_nOption;
  if(value == 0)
    value = 100000;
  return value;
}

#ifndef _NC_UTILS_
#define _NC_UTILS_

inline double nc_mod(double a, double b) {return(fmod(a, b));}

inline double nc_cube(double a) {return a*a*a;}
inline double nc_square(double a) {return a*a;}

inline double cwiseMin(double a, double b) {return (a <= b ? a : b);}
inline double cwiseMax(double a, double b) {return (a >= b ? a : b);}

#endif
