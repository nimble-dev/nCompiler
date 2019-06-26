context("Testing cppLiteral operator")

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
        types = list(ans = list())
      )
      return(ans)
      returnType(list())
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
        types = list(ans = list())
      )
      return(ans)
      returnType(list())
    }
  )
  nfC <- nCompile_nFunction(nf)
  ans <- nfC(c(1,2,3), 4)
  expect_true(is.list(ans))
  expect_equal(length(ans), 3)
  expect_equal(ans$x, as.array(c(1,2,3)))
  expect_equal(ans$y, 4)
  expect_equal(ans$z, as.array(c(5,6,7)))
})
