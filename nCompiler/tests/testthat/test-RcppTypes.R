context("Testing Rcpp types")

test_that("RcppNumericVector works", {
  nf <- nFunction(
    fun = function(x = "RcppNumericVector",
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
        'return x.row(i - 1);',
        types = list()
      )
      returnType("RcppNumericVector()")
    }
  )
  nfmxC <- nCompile_nFunction(nfmx)
  y <- matrix(c(10, 20, 30, 40), nrow = 2)
  ans <- nfmxC(y, 2)
  expect_equal(ans, y[2,])
})


test_that("RcppIntegerVector works", {
  nf <- nFunction(
    fun = function(x = "RcppIntegerVector",
                   y = 'RcppIntegerVector') {
      cppLiteral(
        'ans = x + y;
 return ans;',
        types = list(ans = "RcppIntegerVector()")
      )
      returnType("RcppIntegerVector()")
    }
  )
  nfC <- nCompile_nFunction(nf)
  x <- c(1, 2)
  y <- c(100, 200)
  ans <- nfC(x, y)
  expect_equal(ans, x+y)
})

test_that("RcppIntegerMatrix works", {
  nfmx <- nFunction(
    fun = function(x = "RcppIntegerMatrix()",
                   i = "numericScalar()") {
      cppLiteral(
        'return x.row(i - 1);',
        types = list()
      )
      returnType("RcppIntegerVector()")
    }
  )
  nfmxC <- nCompile_nFunction(nfmx)
  y <- matrix(c(10, 20, 30, 40), nrow = 2)
  ans <- nfmxC(y, 2)
  expect_equal(ans, y[2,])
})


test_that("RcppLogicalVector works", {
  nf <- nFunction(
    fun = function(x = "RcppLogicalVector",
                   y = 'logicalScalar') {
      cppLiteral(
        'x.insert(y, x.length());
return x.sort();',
        types = list()
      )
      returnType("RcppLogicalVector()")
    })
  nfC <- nCompile_nFunction(nf)
  x <- c(FALSE, TRUE, FALSE, FALSE, FALSE)
  y <- TRUE
  ans <- nfC(x, y)
  expect_false(any(ans[1:4]))
  expect_true(all(ans[5:6]))
})

test_that("RcppLogicalMatrix works", {
  nfmx <- nFunction(
    fun = function(x = "RcppLogicalMatrix()",
                   i = "numericScalar()") {
      cppLiteral(
        'return x.row(i - 1);',
        types = list()
      )
      returnType("RcppLogicalVector()")
    }
  )
  nfmxC <- nCompile_nFunction(nfmx)
  y <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2)
  ans <- nfmxC(y, 2)
  expect_equal(ans, y[2,])
})


test_that("RcppComplexVector works", {
  nf <- nFunction(
    fun = function(x = "RcppComplexVector") {
      cppLiteral(
        'return x.sort();',
        types = list()
      )
      returnType("RcppComplexVector()")
    })
  nfC <- nCompile_nFunction(nf)
  x <- c(1 + 2i, 2 + 1i, -1 - 1i, 8 - 5i, 1)
  ans <- nfC(x)
  expect_equal(sort(x), ans)
})

test_that("RcppComplexMatrix works", {
  nfmx <- nFunction(
    fun = function(x = "RcppComplexMatrix()",
                   i = "numericScalar()") {
      cppLiteral(
        'return x.row(i - 1);',
        types = list()
      )
      returnType("RcppComplexVector()")
    }
  )
  nfmxC <- nCompile_nFunction(nfmx)
  y <- matrix(c(2i + 22, 3i, 2i+4, 9), nrow = 2)
  ans <- nfmxC(y, 2)
  expect_equal(ans, y[2,])
})

test_that("RcppCharacterVector works", {
  nf <- nFunction(
    fun = function(x = "RcppCharacterVector") {
      cppLiteral(
        'return x.sort();',
        types = list()
      )
      returnType("RcppCharacterVector()")
    })
  nfC <- nCompile_nFunction(nf)
  x <- c("Hi", "Test", "Foo1", "Foo2")
  ans <- nfC(x)
  expect_equal(sort(x), ans)
})

test_that("RcppCharacterMatrix works", {
  nfmx <- nFunction(
    fun = function(x = "RcppCharacterMatrix()",
                   i = "numericScalar()") {
      cppLiteral(
        'return x.row(i - 1);',
        types = list()
      )
      returnType("RcppCharacterVector()")
    }
  )
  nfmxC <- nCompile_nFunction(nfmx)
  y <- matrix(c("Hello", "Hi", "Foo1", "Foo2"), nrow = 2)
  ans <- nfmxC(y, 2)
  expect_equal(ans, y[2,])
})


test_that("RcppDateVector works", {
  nf <- nFunction(
    fun = function(x = "RcppDateVector") {
      cppLiteral(
        'return x.sort();',
        types = list()
      )
      returnType("RcppDateVector()")
    })
  nfC <- nCompile_nFunction(nf)
  x <- as.Date(c("1996-06-23", "1977-05-25", "1999-05-19", "2001-01-01"))
  ans <- nfC(x)
  expect_equal(sort(x), ans)
})

test_that("RcppDatetimeVector works", {
  nf <- nFunction(
    fun = function(x = "RcppDatetimeVector") {
      cppLiteral(
        'return x.sort();',
        types = list()
      )
      returnType("RcppDatetimeVector()")
    })
  nfC <- nCompile_nFunction(nf)
  x <- as.POSIXct("2020-07-07 10:25:06 PDT")
  x <- c(x, x - 1000000, x + 1000000)
  correct <- sort(x)
  ans <- nfC(x)
  expect_equal(correct, ans)
})

## TODO: Make a better test of RawVector. Can't figure out a simple case where
## this would be used (or what its methods are)
test_that("RcppRawVector works", {
  nf <- nFunction(
    fun = function(x = "RcppRawVector") {
      cppLiteral(
        'return x;',
        types = list()
      )
      returnType("RcppRawVector()")
    })
  nfC <- nCompile_nFunction(nf)
  x <- as.raw(c(10, 11, 22, 0))
  ans <- nfC(x)
  expect_equal(x, ans)
})

## TODO: RcppNames seems like it doesn't work, but I don't really get how we
## should expect to be able to use it.
# test_that("RcppNames works", {
#   nf <- nFunction(
#     fun = function(x = "RcppNamed") {
#       cppLiteral(
# '
# ans = Rcpp::List::create(Named("Zero") = 0, x = 2);
# return ans;',
#         types = list(ans = "RcppList")
#       )
#       returnType("RcppList()")
#     })
#   nfC <- nCompile_nFunction(nf)
#   x <- c("One" = 1)
#   ans <- nfC(x)
#   expect_equal(x, ans)
# })


test_that("RcppS4 works", {
  nfs4 <- nFunction(
    fun = function(x = "RcppS4()") {
      cppLiteral(
        'return x.hasSlot("y");',
        types = list()
      )
      returnType("logicalScalar")
    }
  )
  nfs4C <- nCompile_nFunction(nfs4)
  
  # Taken from ?setClass
  track <- setClass("track", slots = c(x="numeric", y="numeric"))
  t1 <- track(x = 1:10, y = 1:10 + rnorm(10))
  
  expect_true(nfs4C(t1))
})


## TODO: Figure out if Rcpp::Function works?
# test_that("RcppS4 works", {
#   nffn <- nFunction(
#     fun = function() {
#       cppLiteral(
#         'f = Rcpp::Function("rnorm"); return f(1, 0, 1);',
#         types = list(f = "RcppFunction()")
#       )
#       returnType("numericScalar")
#     }
#   )
#   nffnC <- nCompile_nFunction(nffn)
#   
#   test_fn <- function(l) {l + 1}
# 
#   expect_equal(nffnC(test_fn), 11)
#   expect_false(nffnC(false_fn))
# })

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
test_that("RcppEigenMatrixXi works", {
  nfmxEigen <- nFunction(
    fun = function(x = "RcppEigenMatrixXi") {
      cppLiteral(
        'return x.transpose();',
        types = list()
      )
      returnType("RcppEigenMatrixXi()")
    }
  )
  nfmxEigenC <- nCompile_nFunction(nfmxEigen)
  y <- matrix(c(11, 12, -10, 300, 10, 2), nrow = 2)
  ans <- nfmxEigenC(y)
  expect_equal(ans, t(y))
})

test_that("RcppEigenMatrixXcd works", {
  nfmxEigen <- nFunction(
    fun = function(x = "RcppEigenMatrixXcd") {
      cppLiteral(
        'return x.transpose();',
        types = list()
      )
      returnType("RcppEigenMatrixXcd()")
    }
  )
  nfmxEigenC <- nCompile_nFunction(nfmxEigen)
  y <- matrix(c(2i, 0.3i+1, -5i+2, 0, 0, 100i+0.2), nrow = 2)
  ans <- nfmxEigenC(y)
  expect_equal(ans, t(y))
})

test_that("RcppEigenVectorXd works", {
  nfEigen <- nFunction(
    fun = function(x = "RcppEigenVectorXd",
                   y = "RcppEigenMatrixXd") {
      cppLiteral(
        'return y * x;',
        types = list()
      )
      returnType("RcppEigenMatrixXd()")
    }
  )
  nfEigenC <- nCompile_nFunction(nfEigen)
  x <- c(0.1, 0.2, 0.5)
  y <- matrix(c(0.4, 0.3, 0.2, 0.1, 0, 0.5), nrow = 2, ncol = 3)
  ans <- nfEigenC(x, y)
  expect_equal(ans, y %*% x)
})


test_that("RcppEigenVectorXi works", {
  nfEigen <- nFunction(
    fun = function(x = "RcppEigenVectorXi",
                   y = "RcppEigenMatrixXi") {
      cppLiteral(
        'return y * x;',
        types = list()
      )
      returnType("RcppEigenMatrixXi()")
    }
  )
  nfEigenC <- nCompile_nFunction(nfEigen)
  x <- c(1, 2, 5)
  y <- matrix(c(4, 3, 2, 1, 0, 5), nrow = 2, ncol = 3)
  ans <- nfEigenC(x, y)
  expect_equal(ans, y %*% x)
})


test_that("RcppEigenVectorXcd works", {
  nfEigen <- nFunction(
    fun = function(x = "RcppEigenVectorXcd",
                   y = "RcppEigenMatrixXcd") {
      cppLiteral(
        'return y * x;',
        types = list()
      )
      returnType("RcppEigenMatrixXcd()")
    }
  )
  nfEigenC <- nCompile_nFunction(nfEigen)
  x <- c(1i, 2i, 5 + 2i)
  y <- matrix(c(4i+1, 0.3i, 0.2 + 1i, 10i + 10, 0+1i, 5), nrow = 2, ncol = 3)
  ans <- nfEigenC(x, y)
  expect_equal(ans, y %*% x)
})

