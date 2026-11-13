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

test_that("length() works for names, slices and other expressions", {
  foo <- nFunction(
    function(x = 'numericVector', m = 'numericMatrix') {
      a <- length(x)
      b <- length(x[2:3]) # a slice has no size() method in Eigen
      c <- length(m[1:2, ])
      d <- length(x + 1)
      e <- length(3)
      ans <- 10000 * a + 1000 * b + 100 * c + 10 * d + e
      return(ans)
      returnType('numericScalar')
    }
  )
  cfoo <- nCompile(foo)
  x <- 1:5
  m <- matrix(1:6, 3)
  expect_equal(cfoo(x, m), 52451)
  expect_equal(foo(x, m), 52451)
})
