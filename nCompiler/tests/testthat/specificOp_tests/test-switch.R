library(nCompiler)
library(testthat)

test_that("nSwitch works", {
  foo <- nFunction(
    function(opt = integerScalar()) {
      x <- 0;
      switch(opt, 1:2, x<-1, x<-2)
      return(x)
    },
    returnType = 'numericScalar'
  )
  # check that my_IDs is found by scoping
  # and that an option can give a {} set of code
  layer <- function() {
    my_IDs <- 5:6
    foo2 <- nFunction(
      function(opt = integerScalar()) {
        x <- 0;
        switch(opt, my_IDs, {x<-x+1; x<-x+4}, x<-6)
        return(x)
      },
      returnType = 'numericScalar'
    )
    foo2
  }
  foo2 <- layer()

  comp <- nCompile(foo, foo2)
  expect_equal(foo(2), 2)
  expect_equal(foo2(5), 5)
  expect_equal(foo2(6), 6)
  expect_equal(foo2(4), 0)

  expect_equal(comp$foo(2), 2)
  expect_equal(comp$foo2(5), 5)
  expect_equal(comp$foo2(6), 6)
  expect_equal(comp$foo2(4), 0)

  foo_error1 <-
    foo <- nFunction(
      function(opt = integerScalar()) {
        x <- 0;
        switch(opt, 1:3, x<-1, x<-2)
        return(x)
      },
      returnType = 'numericScalar'
    )
  cat("expecting two error messages about number of IDs for nSwitch not matching number of options:")
  expect_error(nCompile(foo_error1))
  expect_error(foo_error1(3))
})
