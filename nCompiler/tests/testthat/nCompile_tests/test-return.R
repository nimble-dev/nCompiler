# Tests of both casting and error-trapping from return statements

library(nCompiler)
library(testthat)


test_that("return statement will be cast", {

  debug(nCompiler:::eigenizeEnv$Return)

  foo = nFunction(
    fun = function() {
      y <- rnorm(4, 0, 10)
      return(y)
    }, returnType = 'integerVector')
  # We expect a warning message about returning doubles as integers, but it will compile and run
  cfoo <- nCompile(foo)
  cfoo()

  foo <- nFunction(
    function(x = 'integerVector' ) {
      return(x)
    },
    returnType = 'numericVector'
  )
  cfoo <- nCompile(foo)
  cfoo(1:4)

  foo <- nFunction(
    function(x = 'integerVector' ) {
      return(x)
    },
    returnType = 'integerScalar'
  )
  cfoo <- nCompile(foo)
  cfoo(1:4)

  # digression to look at other uses of scalar_cast
  foo <- nFunction(
    function(x = 'numericVector' ) {
      v <- var(x)
      return(v)
    },
    returnType = 'numericScalar'
  )
  cfoo <- nCompile(foo)
  cfoo(1:6)

})


# Look at how some other automatic casting is done

foo <- nFunction(
  fun = function() {
    y <- rnorm(4)
    x <- 1L:4L
    y <- x # we insert the cast
    z <- 1L
    z <- sum(y)
    return(y)
    returnType('numericVector')
  }
)
cfoo <- nCompile(foo)
cfoo()
