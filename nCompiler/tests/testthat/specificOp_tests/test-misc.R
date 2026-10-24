# miscellaneous tests such as regression tests of corner-case bugs
library(nCompiler)

test_that("diag(dim(m)[1]) works", {
  foo <- nFunction(
    function(m = 'numericMatrix') {
      #D <- DEBUG_EIGEN(diag(dim(m)[1]))
      D <- diag(dim(m)[1])
      return(D)
      returnType('numericMatrix')
    }
  )

  #undebug(nCompiler:::compile_eigenize)
  expect_no_error(cfoo <- nCompile(foo))
  m <- matrix(rnorm(9), nrow = 3)
  expect_equal(cfoo(m), diag(3))
  expect_equal(foo(m), diag(3))
})
