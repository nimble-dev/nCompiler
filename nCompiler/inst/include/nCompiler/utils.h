#include<iostream>
#include<sstream>

#define PRINTF Rprintf

std::ostringstream _nCompiler_global_output;

void nCompiler_print_to_R(std::ostringstream &input) {
  PRINTF("%s", input.str().c_str());
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
