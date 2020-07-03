context("Testing Rcpp types")

test_that("RcppNumericVector works", {
  nf <- nFunction(
    fun = function(x = RcppNumericVector(),
                   y = 'RcppNumericVector') {
      cppLiteral(
'ans = x + y;
 return ans;',
        types = list(ans = RcppNumericVector())
      )
      returnType(RcppNumericVector())
    }
   #,types = list(y = 'RcppNumericVector')
  )
  nfC <- nCompile_nFunction(nf)
  x <- c(1, 2)
  y <- c(100, 200)
  ans <- nfC(x, y)
  expect_equal(ans, x+y)
})
