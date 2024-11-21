# These tests work.
# Note that test-RcppTypes also uses cppLiteral,
# so this is largely redundant with that.
test_that("cppLiteral with list of scalars works", {
  nf <- nFunction(
    fun = function(x = double(0),
                   y = logical(0)) {
      a <- x + 1
      b <- y == TRUE
      cppLiteral(
        'ans = Rcpp::List::create(
  Rcpp::Named("item1") = a,
  Rcpp::Named("item2") = b
);',
types = list(ans = "RcppList")
)
      return(ans)
      returnType("RcppList")
    }
)
  nfC <- nCompile_nFunction(nf)
  ans <- nfC(2, FALSE)
  expect_true(is.list(ans))
  expect_equal(length(ans), 2)
  expect_equal(ans$item1, 3)
  expect_equal(ans$item2, FALSE)
})

test_that("cppLiteral with list including vector works", {
  nf <- nFunction(
    fun = function(x = double(1),
                   y = double(0)) {
      z <- x + y
      cppLiteral(
        'ans = Rcpp::List::create(
  Rcpp::Named("x") = Rcpp::wrap(x),
  Rcpp::Named("y") = y,
  Rcpp::Named("z") = Rcpp::wrap(z)
);',
types = list(ans = "RcppList")
)
      return(ans)
      returnType("RcppList")
    }
)
  nfC <- nCompile_nFunction(nf)
  ans <- nfC(c(1,2,3), 4)
  expect_true(is.list(ans))
  expect_equal(length(ans), 3)
  expect_equal(ans$x, c(1,2,3))
  expect_equal(ans$y, 4)
  expect_equal(ans$z, c(5,6,7))
})
