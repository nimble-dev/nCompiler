context("Test cppLiteral")

test_that("cppLiteral with list of scalars", {
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
