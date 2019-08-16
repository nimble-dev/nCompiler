#ifndef _CWISEUNARY_EXTERNAL
#define _CWISEUNARY_EXTERNAL

double logit(double x) {return log(x/(1.-x));}
double ilogit(double x) {return(1./(1. + exp(-x)));}
double cloglog(double x) {return(log(-log(1.-x)));}
double icloglog(double x) {return(1.-exp(-exp(x)));}
double probit(double x) {return(R::qnorm(x, 0., 1., 1, 0));}
double iprobit(double x) {return(R::pnorm(x, 0., 1., 1, 0));}
double gammafn(double x) {return(R::gammafn(x));}
double lgammafn(double x) {return(R::lgammafn(x));}
double factorial(double x) {return(R::gammafn(1+x));}
double lfactorial(double x) {return(R::lgammafn(x + 1));}
double nRound(double x) {return(R::fround(x, 0.));}
double nTrunc(double x) {return(R::ftrunc(x));}

#endif
