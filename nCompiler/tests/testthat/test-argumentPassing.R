# Tests of passing by copy, reference, and block reference,
# from R-R, R-C, and C-C,
# with nFunctions and nClass methods,
# with different argument ordering,
# for different kinds of numeric objects

library(testthat)
library(nCompiler)

foo <- nFunction(
  function(x = numericVector()) {
    x <- x + 1
    return(x)
  },
  returnType = 'numericVector'
)
x <- 1:3
y <- foo(x)
expect_equal(x+1, y)
cfoo <- nCompile(foo)
x <- 1:3
y <- cfoo(x)
expect_equal(x+1, y)

foo <- nFunction(
  function(x = numericVector()) {
    x <- x + 1
    return(x)
  },
  refArgs = 'x',
  returnType = 'numericVector'
)
x <- 1:3
y <- foo(x)
expect_equal(x, y)
x <- 1:5
expect_error(y <- foo(x[2:4]))
nOptions(showCompilerOutput = TRUE)
cfoo <- nCompile(foo)
cfoo2 <- nCompiler:::passByReferenceIntoC(cfoo, refArgs = 'x')
x <- 1:3
y <- cfoo(x)
y <- cfoo2(x)

foo <- nFunction(
  function(x = numericVector(), x2 = numericVector()) {
    x2 <- x2 + x
    return(x2)
  },
  blockRefArgs = 'x2',
  returnType = 'numericVector'
)
x <- 1:3
x2 <- 11:13
y <- foo(x, x2)
expect_equal(x2, y)
x <- 1:3
x2 <- 11:15
y <- foo(x, x2[2:4])
expect_equal(c(x2[1], y, x2[5]), x2)
x <- 1:4 # deliberate length mismatch
x2 <- 11:15
expect_warning(y <- foo(x, x2[2:4])) # We could error-trap more on this.
