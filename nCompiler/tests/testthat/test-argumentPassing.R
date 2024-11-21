# Tests of passing by copy, reference, and block reference,
# from R-R, R-C, and C-C,
# with nFunctions and nClass methods,
# with different argument ordering,
# for different kinds of numeric objects

library(testthat)
library(nCompiler)

message("More test coverage of argument passing cases is needed. See comments.")
# need cases of multiple function call layers
# need cases of >1D as well as scalar
# need cases of blockRef
message("doing scalar = vector + scalar does not error out if the vector in length>1.")

# compiled and uncompiled 1D by copy
test_that("pass 1D by copy works (compiled & uncompiled)", {
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
})


# compiled and uncompiled 1D by ref (foo here can be removed)
test_that("pass 1D by ref works (compiled & uncompiled)", {
  foo <- nFunction(
    function(x = numericVector()) {
      x <- x + 1
      return(x)
    },
    returnType = 'numericVector'
  )
  foo2 <- nFunction(
    function(x = numericVector()) {
      x <- x + 1
      return(x)
    },
    refArgs = 'x',
    returnType = 'numericVector'
  )
  x <- 1:3
  y <- foo(x)
  expect_equal(x+1, y)
  y <- foo2(x)
  expect_equal(x, y)

  cfoo <- nCompile(foo, foo2)
  x <- 1:3
  y <- cfoo$foo(x)
  expect_equal(x+1, y)
  y <- cfoo$foo2(x)
  expect_equal(x, y)
})

# compiled and uncompiled 1D by ref correctly errors when used as blockRef
test_that("pass by 1D ref errors out when used as blockRef (compiled & uncompiled)", {
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

  cfoo <- nCompile(foo)
  x <- 1:3
  y <- cfoo(x)
  expect_equal(x, y)
  x <- 1:5
  expect_error(y <- cfoo(x[2:4]))
})

# compiled and uncompiled mix 1D mix of by copy and by ref
test_that("pass by blockRef works (with or without []) and error-traps (compiled & uncompiled)", {
  # message("error-trapping differs between R and C++")
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

  cfoo <- nCompile(foo)
  x <- 1:3
  x2 <- as.numeric(11:13)
  y <- cfoo(x, x2)
  expect_equal(x2, y)
  x <- 1:3
  x2 <- as.numeric(11:15)
  y <- cfoo(x, x2[2:4])
  expect_equal(c(x2[1], y, x2[5]), x2)
  x <- 1:4 # deliberate length mismatch
  x2 <- as.numeric(11:15)
  expect_error(y <- cfoo(x, x2[2:4])) # We could error-trap more on this.
  # Discrepancy in compiled vs uncompiled is a warning in uncompiled but an error in compiled.
})

## It seems like this test should give an error because we are assigning
## from a vector to a scalar and the vector is not length 1.
## This seems to occur from flex_(y) assignment.
## The first element alone is used.
## test_that("scalar and vector pass by copy, basic test (compiled and uncompiled)", {
##   # 0D and 1D by copy in nClass
##   nc1 <- nClass(
##     Cpublic = list(
##       Cfoo = nFunction(
##         fun = function(x, z) {
##           y <- x + 1
##           return(y)
##         },
##         argTypes = list(x = 'numericScalar', z = 'numericVector'),
##         returnType = 'numericScalar')
##     )
##   )
##   Cnc1 <- nCompile(nc1)
##   Cobj <- Cnc1$new()
##   expect_equal(Cobj$Cfoo(10, 1:3), 10+(1:3))
## })
