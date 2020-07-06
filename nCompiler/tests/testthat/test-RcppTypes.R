context("Testing Rcpp types")

test_that("RcppNumericVector works", {
  nf <- nFunction(
    fun = function(x = "RcppNumericVector()",
                   y = 'RcppNumericVector') {
      cppLiteral(
'ans = x + y;
 return ans;',
        types = list(ans = "RcppNumericVector()")
      )
      returnType("RcppNumericVector()")
    }
   #,types = list(y = 'RcppNumericVector')
  )
  nfC <- nCompile_nFunction(nf)
  x <- c(1, 2)
  y <- c(100, 200)
  ans <- nfC(x, y)
  expect_equal(ans, x+y)
})






test_that("RcppNumericMatrix works", {
  nfmx <- nFunction(
    fun = function(x = "RcppNumericMatrix()",
                   i = "numericScalar()") {
      cppLiteral(
'return x.row(i + 1);',
        types = list()
      )
      returnType("RcppNumericVector()")
    }
  )
  nfmxC <- nCompile_nFunction(nfmx)
  y <- matrix(c(10, 20, 30, 40), nrow = 2)
  ans <- nfmxC(y, 2)
  expect_equal(ans, y)
})



test_that("RcppDataFrame works", {
  nfdf <- nFunction(
    fun = function(x = "RcppDataFrame()",
                   v = "RcppNumericVector()") {
      cppLiteral(
'x.push_front(v, "new");
return x;',
        types = list()
      )
      returnType("RcppDataFrame()")
    }
  )
  nfdfC <- nCompile_nFunction(nfdf)
  y <- data.frame(v = c(1:10), sq = c(1:10)^2)
  ans <- nfdfC(y, c(101:110))
  expect_equal(ans, cbind(new = c(101:110), y))
})



test_that("RcppEigenMatrixXd works", {
  nfmxEigen <- nFunction(
    fun = function(x = "RcppEigenMatrixXd") {
      cppLiteral(
'return x.transpose();',
        types = list()
      )
      returnType("RcppEigenMatrixXd()")
    }
  )
  nfmxEigenC <- nCompile_nFunction(nfmxEigen)
  y <- matrix(c(0.1, 0.2, 0.5, 0.1, 10, 11), nrow = 2)
  ans <- nfmxEigenC(y)
  expect_equal(ans, t(y))
})


