# This test file is to check on combinations of
# header file needs. It will be redundant with other
# tests but is organized around the permutations and
# combinations of header needs.
# Depending on how this develops, it may or may not
# make sense as a standard test files.

# To-Do: fix the situation with multiple nFunctions or nClasses.
# The problem is that we make the entire cppPackets and then concatenate the cpp parts
# to obtain one cpp file. but this means the second portion appears after the post_Rcpp
# includes have occurred. So if the second portion needed to invoke a component that the
# first one didn't, it would not be invoked in the pre_Rcpp stage and could thus cause an error.
# I think this needs to be addressed either by smarter integration of pieces of multiple cppDefs
# OR possibly by relying on the packaging pathway when there are multiple things to compile.
# Unfortunately the latter is not currently working.

library(nCompiler)
library(testthat)

nOptions(pause_after_writing_files = TRUE)
## Everthing is assumed to need Rcpp

# nFunction, no Eigen (no cereal buy definition)

foo <- nFunction(
  function(x = 'numericScalar') {return(x + 100);},
  returnType = 'numericScalar')
Cfoo <- nCompile(foo)
expect_equal(Cfoo(100), 200)

# nFunction with Eigen via body only - HAH DOES NOT WORK IN COMPILER YET
## foo <- nFunction(
##   function(x = 'numericScalar') {
##     y <- numericVector()
##     y[1] <- x
##     y[2] <- x
##     return(sum(y))
##     #return(sum(rep(x, 2))); # wrong answer
##   },
##   returnType = 'numericScalar')
## Cfoo <- nCompile(foo)
## expect_equal(Cfoo(100), 200)

# nFunction with Eigen via arguments only
foo <- nFunction(
  function(x = 'numericVector') {
    return(x[1] + 100)
  },
  returnType = 'numericScalar')
Cfoo <- nCompile(foo)
expect_equal(Cfoo(c(300, 500)), 400)


# nFunction with Eigen via arguments and body
foo <- nFunction(
  function(x = 'numericVector') {
    return(sum(x) + 100)
  },
  returnType = 'numericScalar')
Cfoo <- nCompile(foo)
expect_equal(Cfoo(c(300, 500)), 900)

message("Add case of nFunction via Eigen via body and return only, not arguments.")

message("It looks like blockRef is broken when used in an expression.")

# nFunction with Eigen via ref and blockRef arguments
foo <- nFunction(
  function(x = numericVector(), x2 = numericVector()) {
    x <- x + 1
    x2 <- x2 + 5
    return(x)
  },
  refArgs = 'x',
#  blockRefArgs = 'x2',
  returnType = 'numericVector'
)
Cfoo <- nCompile(foo)
x <- 1:3
x2 <- 101:103
y <- Cfoo(x, x2)
expect_equal(x, y)
# Um some other stuff looks broken.

# possibly check on TBB functionality (haven't used in a long time)

# nClass, no Eigen, no cereal
nc1 <- nClass(
  Cpublic = list(
    x = 'numericScalar',
    y = 'RcppNumericVector'
  )
)
Cnc1 <- nCompile(nc1)
obj <- Cnc1$new()
obj$x <- 3
obj$y <- 1:3
expect_equal(obj$x, 3)
#

# nClass, Eigen via member, no cereal
nc1 <- nClass(
  Cpublic = list(
    x = 'numericVector'
  )
)
Cnc1 <- nCompile(nc1)
obj <- Cnc1$new()
obj$x <- 1:3
expect_equal(obj$x, 1:3)
#

# nClass, Eigen via method, no cereal
nc1 <- nClass(
  Cpublic = list(
    x = 'numericScalar',
    foo = nFunction(function(x = 'numericVector') {return(x+1)},
                    returnType = 'numericVector')
  )
)
Cnc1 <- nCompile(nc1)
obj <- Cnc1$new()
x <- 1:3
expect_equal(obj$foo(x), 1:3 + 1)
#

# nClass, Eigen via method, with cereal
nOptions(serialize=TRUE)
nOptions(showCompilerOutput=TRUE)
nc1 <- nClass(
  Cpublic = list(
    x = 'numericScalar',
    foo = nFunction(function(x = 'numericVector') {return(x+1)},
                    returnType = 'numericVector')
  )
)
Cnc1 <- nCompile(nc1)
obj <- Cnc1$new()
x <- 1:3
expect_equal(obj$foo(x), 1:3 + 1)


# only Rcpp, multiple nClasses and nFunctions, no cereal, no Eigen

#

## What about eigen error-handling - check if that is being set up fully
nOptions(serialize=FALSE)
nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    x = 'test_predefined'
  ))
Cnc1 <- nCompile(nc1, test_predefined)

# predefined nClass
nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    x = 'derivClass'
  ))
Cnc1 <- nCompile(nc1, derivClass)
obj <- Cnc1$nc1$new()
obj$x <- Cnc1$derivClass$new()
obj$x$gradient <- matrix(1:4, nrow = 2)
expect_equal(obj$x$gradient, matrix(1:4, nrow = 2))
#uncompiled
obj <- nc1$new()
obj$x <- derivClass$new()
obj$x$gradient <- matrix(1:4, nrow = 2)
expect_equal(obj$x$gradient, matrix(1:4, nrow = 2))

# multiples
message("the tricky header include system need attention around multiple nFunction or nClasses")
foo <- nFunction(
  function(x = 'numericVector') {return(x[1] + 100);},
  returnType = 'numericScalar')
bar <- nFunction(
  function(x = 'numericVector') {return(x[1] + 101);},
  returnType = 'numericScalar')
comp <- nCompile(foo, bar)
comp <- nCompile(foo, bar, package=TRUE) # broken
expect_equal(comp$foo(100), 200)
expect_equal(comp$bar(100), 201)

nc1 <- nClass(
  Cpublic = list(
    x = 'numericScalar',
    foo = nFunction(function(x = 'numericVector') {return(x+1)},
                    returnType = 'numericVector')
  )
)
nc2 <- nClass(
  Cpublic = list(
    x = 'numericScalar',
    foo = nFunction(function(x = 'numericVector') {return(x+2)},
                    returnType = 'numericVector')
  )
)
comp <- nCompile(nc1, nc2)
comp <- nCompile(nc1, nc2, package=TRUE) #broken
