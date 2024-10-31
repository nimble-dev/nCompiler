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
    )
  )
)

nOptions(pause_after_writing_files=TRUE)
Cnc1 <- nCompile(nc1)
test <- Cnc1$new()
test
test$Cfoo(2)
