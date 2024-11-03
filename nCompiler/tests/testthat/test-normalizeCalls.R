library(nCompiler)

# These are not really tests. They were just toys during work on the normalizeCalls compiler stage,
# waiting to be turned into tests.

# simplest toy
foo <- nFunction(
  fun = function(x = numericScalar()) {
    ans <- 2*x
    return(ans)
    returnType(numericScalar())
  }
)

# example with a match_def
foo <- nFunction(
  fun = function(x = numericScalar()) {
    ans <- nRep(each = 3, x = c(5, 10)) # needs reordering
    ans <- nRep(c(5, 10), 2) # needs naming
    return(ans)
    returnType(numericVector())
  }
)

control <- list()
#control <- list(startDebugStage = "normalizeCalls")
cfoo <- nCompile(foo, control = control)

############
# example with calling another nf
library(nCompiler)

mydiv <- nFunction(
  \(num=numericScalar(), den=numericScalar) {
    return(num/den)
    returnType(numericScalar())
  })
foo <- nFunction(
  fun = function(x = numericScalar(), y=numericScalar()) {
    ans <- mydiv(den = y, num = x)
    return(ans)
    returnType(numericScalar())
  }
)

control <- list()
#control <- list(startDebugStage = "simpleTransformations",
#                endDebugStage = "simpleTransformations")
cfoo <- nCompile(mydiv, foo, control = control)
cfoo$foo(1,2)
