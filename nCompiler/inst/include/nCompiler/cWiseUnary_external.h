#ifndef _CWISEUNARY_EXTERNAL
#define _CWISEUNARY_EXTERNAL

inline double logit(double x) {return log(x/(1.-x));}
inline double ilogit(double x) {return(1./(1. + exp(-x)));}
inline double cloglog(double x) {return(log(-log(1.-x)));}
inline double icloglog(double x) {return(1.-exp(-exp(x)));}
inline double probit(double x) {return(R::qnorm(x, 0., 1., 1, 0));}
inline double iprobit(double x) {return(R::pnorm(x, 0., 1., 1, 0));}
inline double gammafn(double x) {return(R::gammafn(x));}
inline double lgammafn(double x) {return(R::lgammafn(x));}
inline double factorial(double x) {return(R::gammafn(1+x));}
inline double lfactorial(double x) {return(R::lgammafn(x + 1));}
inline double nRound(double x) {return(R::fround(x, 0.));}
inline double nTrunc(double x) {return(R::ftrunc(x));}

#endif
