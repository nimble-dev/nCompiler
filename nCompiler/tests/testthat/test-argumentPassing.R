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

test_ref <- function(copy_fun, ref_fun, x) {
  # ref_fun should set x to the same as returning in y
  y <- copy_fun(x)
  y2 <- ref_fun(x)
  expect_equal(y, x)
}

# compiled and uncompiled 1D by ref
test_that("pass 1D by ref works and error-traps (compiled & uncompiled)", {
  message("This test has four trapped errors.")
  foo <- nFunction(
    function(x = numericVector()) {
      x <- x[1:2] + 10
      return(x)
    },
    returnType = 'numericVector'
  )
  foo2 <- nFunction(
    function(x = numericVector()) {
      x <- x[1:2] + 10
      return(x)
    },
    refArgs = 'x',
    returnType = 'numericVector'
  )
  x <- 1:3
  expect_error(foo2(1:3))
  expect_error(foo2(x[1:3]))
  test_ref(foo, foo2, 1:3)
  cfoo <- nCompile(foo, foo2)
  expect_error(cfoo$foo2(1:3))
  expect_error(cfoo$foo2(x[1:3]))
  test_ref(cfoo$foo, cfoo$foo2, 1:3)
})

# compiled and uncompiled 1D by ref
test_that("pass 1D by ref works and error-traps via nClass method (compiled & uncompiled)", {
  message("This test has four trapped errors.")
  nc1 <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x = numericVector()) {
          x <- x[1:2] + 10
          return(x)
        },
        returnType = 'numericVector'
      )))
  nc2 <- nClass(
    Cpublic = list(
      foo2 = nFunction(
        function(x = numericVector()) {
          x <- x[1:2] + 10
          return(x)
        },
        refArgs = 'x',
        returnType = 'numericVector'
      )))
  obj1 <- nc1$new()
  obj2 <- nc2$new()
  x <- 1:3
  expect_error(obj2$foo2(1:3))
  expect_error(obj2$foo2(x[1:3]))
  test_ref(obj1$foo, obj2$foo2, 1:3)
  comp <- nCompile(nc1, nc2)
  Cobj1 <- comp$nc1$new()
  Cobj2 <- comp$nc2$new()

  cfoo <- nCompile(foo, foo2)
  expect_error(cfoo$foo2(1:3))
  expect_error(cfoo$foo2(x[1:3]))
  test_ref(cfoo$foo, cfoo$foo2, 1:3)
})


# compiled and uncompiled 2D by ref (foo here can be removed)
test_that("pass 1D by ref works and error-traps (compiled & uncompiled)", {
  message("This test has four trapped errors.")
  foo <- nFunction(
    function(x = numericMatrix()) {
      x <- x[1:2, 1:3] + 10
      return(x)
    },
    returnType = 'numericMatrix'
  )
  foo2 <- nFunction(
    function(x = numericMatrix()) {
      x <- x[1:2, 1:3] + 10
      return(x)
    },
    refArgs = 'x',
    returnType = 'numericMatrix'
  )
  x <- matrix(1:12, nrow = 3)
  expect_error(foo2( matrix(1:12, nrow = 3) ))
  expect_error(foo2(x[1:3, 1:4]))
  test_ref(foo, foo2, x)
  cfoo <- nCompile(foo, foo2)
  expect_error(cfoo$foo2(matrix(1:12, nrow = 3)))
  expect_error(cfoo$foo2(x[1:3, 1:4]))
  test_ref(cfoo$foo, cfoo$foo2, x)
})



# compiled and uncompiled mix 1D mix of by copy and by ref
test_that("pass by blockRef works (with or without []) and error-traps (compiled & uncompiled)", {
  message("This test may generate a trapped warning (from R) and a trapped error from C++.")
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
