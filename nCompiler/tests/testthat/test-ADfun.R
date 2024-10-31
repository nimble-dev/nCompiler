# test manual creation of nFunctions that use AD types

library(nCompiler)

## foo <- nFunction(
##   fun = function(x = numericScalar()) {
##     ans <- 2*x
##     return(ans)
##     returnType(numericScalar())
##   }
## )
## cfoo <- nCompile(foo)

ADfoo <- nFunction(
  fun = function(x = ADScalar()) {
    ans <- 2*x
    return(ans)
    returnType(ADScalar())
  },
  enableDerivs=list(isAD=TRUE)
)

nOptions(pause_after_writing_files=TRUE)
cADfoo <- nCompile(ADfoo)
# using package=TRUE is not yet complete
#cADfoo <- nCompile(ADfoo, package = TRUE)
# the control over full vs generic interfaces via packaging pipeline needs attention
# in addition, nCompile(..., package=TRUE) stops after writing package and needs to build/install it.

nc1 <- nClass(
  # put a method in here that has isAD=TRUE
  classname = "bar",
  Cpublic = list(
    Cfoo = nFunction(
      fun = function(x = ADScalar()) {
        ans <- 2*x
        return(ans)
        returnType(ADScalar())
      },
      enableDerivs=list(isAD=TRUE)
    ),
    derivsCfoo = nFunction(
      fun = function(x = numericScalar) {
        ans <- nDerivs(Cfoo(x))
      }
    )
  )
)

debug(nCompiler:::labelAbstractTypesEnv$nDerivs)

nOptions(pause_after_writing_files=TRUE)
Cnc1 <- nCompile(nc1)
test <- Cnc1$new()
test
test$Cfoo(2)

# To do:
# - normalize fn and method call arguments. is that already in place?
# - Label nFunctions and methods as part of simpleTransformations?
# - how am i collecting from nFunction methods artifacts to nClass level artifacts?



class bar : public genericInterfaceC<bar>, public loadedObjectHookC<bar> {
public:
  AD_tape_mgr tape_mgr;
  CppAD::AD<double>  nFun_2_NFID_2 ( CppAD::AD<double> x );
  deriv_mgr2< METHOD_TYPES(&bar::nFun_2_NFID_2),
              ADhandling<RETURN, WRT> > nf2_deriv_mgr;
  bar() : nf2_deriv_mgr(&bar::nFun_2_NFID_2, this) {};
};


f1 <- nFunction(
  fun = function(num = numericScalar(),
                 den = numericScalar()) {
    ans <- num/den
    return(ans)
    returnType(numericScalar())
  }
)

f2 <- nFunction(
  fun = function(num = numericScalar(),
                 den = numericScalar()) {
    ans <- f1(den = den, num = num)
    return(ans)
    returnType(numericScalar())
  }
)

comp <- nCompile(f1, f2)
