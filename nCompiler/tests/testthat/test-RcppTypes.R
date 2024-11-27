# Test that Rcpp types work in nFunctions

# Status: All tests pass.
# RcppDateVector and RcppDatetimeVector can be passed as arguments
# and used but not created as via nCompiler code-generation
# because they lack default constructors.

test_that("RcppNumericVector works in nFunctions", {
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
  nfC <- nCompile(nf)
  x <- c(1, 2)
  y <- c(100, 200)
  ans <- nfC(x, y)
  expect_equal(ans, x+y)
})

# prototype in place a function to evaluate compile-time args

test_that("RcppNumericMatrix works in nFunctions", {
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

test_that("RcppIntegerVector works in nFunctions", {
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

test_that("RcppIntegerMatrix works in nFunctions", {
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

test_that("RcppLogicalVector works in nFunctions", {
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

test_that("RcppLogicalMatrix works in nFunctions", {
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

test_that("RcppComplexVector works in nFunctions", {
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

test_that("RcppComplexMatrix works in nFunctions", {
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

test_that("RcppCharacterVector works in nFunctions", {
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

test_that("RcppCharacterMatrix works in nFunctions", {
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

test_that("RcppDateVector works in nFunctions", {
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

test_that("RcppDatetimeVector works in nFunctions", {
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
test_that("RcppRawVector works in nFunctions", {
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

## Rcpp::Named is not a primary type for passing.
## test_that("RcppNamed works", {
##   nf <- nFunction(
##     fun = function(x = "RcppNamed") {
##       cppLiteral(
## 'return x;',
##         types = list()
##       )
##       returnType("RcppNamed()")
##     })
##   nfC <- nCompile_nFunction(nf)
##   x <- c("One" = 1)
##   ans <- nfC(x)
##   expect_equal(x, ans)
## })

test_that("RcppS4 works in nFunctions", {
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

test_that("RcppFunction works", {
  nffn <- nFunction(
    fun = function(f = 'RcppFunction', x = 'numericScalar') {
      cppLiteral(
        'ans = f(x);
 return ans;',
        types = list(ans = "RcppNumericVector")
      )
      returnType("RcppNumericVector")
    })
  nffnC <- nCompile_nFunction(nffn)

  set.seed(505)
  result1 <- nffnC(rnorm, 10)
  set.seed(505)
  result1_Correct <- rnorm(10)
  expect_equal(result1, result1_Correct)
  
  result2 <- nffnC(nCompiler::logit, 0.4)
  result2_correct <- nCompiler::logit(0.4)
  expect_equal(result2, result2_correct)
})

test_that("RcppDataFrame works in nFunctions", {
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

test_that("RcppEigenMatrixXd works in nFunctions", {
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

test_that("RcppEigenMatrixXi works in nFunctions", {
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

test_that("RcppEigenMatrixXcd works in nFunctions", {
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

test_that("RcppEigenVectorXi works in nFunctions", {
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

test_that("RcppEigenVectorXcd works in nFunctions", {
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

# Test that Rcpp types work as public fields in nClasses
test_that("RcppNumericVector works in nClasses", {
  nc <- nClass(
    classname = "test_RcppNumericVector",
    Cpublic = list(
      x = "RcppNumericVector",
      set_x = nFunction(fun = function(new_x = "RcppNumericVector") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- c(10, 0, 0, 0.1)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- c(0, 100, 10, 0.01)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppNumericMatrix works in nClasses", {
  nc <- nClass(
    classname = "test_RcppNumericMatrix",
    Cpublic = list(
      x = "RcppNumericMatrix",
      set_x = nFunction(fun = function(new_x = "RcppNumericMatrix") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- matrix(c(10, 0, 0, 0.1), nrow = 2)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- matrix(c(0, 100, 10, 0.01), nrow = 2)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppIntegerVector works in nClasses", {
  nc <- nClass(
    classname = "test_RcppIntegerVector",
    Cpublic = list(
      x = "RcppIntegerVector",
      set_x = nFunction(fun = function(new_x = "RcppIntegerVector") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- c(10, 0, 0, 1)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- c(0, 100, 10, 2)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppIntegerMatrix works in nClasses", {
  nc <- nClass(
    classname = "test_RcppIntegerMatrix",
    Cpublic = list(
      x = "RcppIntegerMatrix",
      set_x = nFunction(fun = function(new_x = "RcppIntegerMatrix") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- matrix(c(10, 0, 0, 2), nrow = 2)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- matrix(c(0, 100, 10, 4), nrow = 2)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppCharacterVector works in nClasses", {
  nc <- nClass(
    classname = "test_RcppCharacterVector",
    Cpublic = list(
      x = "RcppCharacterVector",
      set_x = nFunction(fun = function(new_x = "RcppCharacterVector") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- c("Hello", "Hi", "foo1", "foo2")
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- c("Hi", "foo1", "Hello", "foo2")
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppCharacterMatrix works in nClasses", {
  nc <- nClass(
    classname = "test_RcppCharacterMatrix",
    Cpublic = list(
      x = "RcppCharacterMatrix",
      set_x = nFunction(fun = function(new_x = "RcppCharacterMatrix") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- matrix(c("Hello", "Hi", "foo1", "foo2"), nrow = 2)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- matrix(c("Hi", "foo1", "Hello", "foo2"), nrow = 2)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppComplexVector works in nClasses", {
  nc <- nClass(
    classname = "test_RcppComplexVector",
    Cpublic = list(
      x = "RcppComplexVector",
      set_x = nFunction(fun = function(new_x = "RcppComplexVector") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- c(5i, 4i, 1, 5i + 3)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- c(4i, 1, 5i + 3, 5i)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppComplexMatrix works in nClasses", {
  nc <- nClass(
    classname = "test_RcppComplexMatrix",
    Cpublic = list(
      x = "RcppComplexMatrix",
      set_x = nFunction(fun = function(new_x = "RcppComplexMatrix") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- matrix(c(4i, 1, 5i + 3, 10), nrow = 2)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- matrix(c(4i, 1, 5i + 3, 0), nrow = 2)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppLogicalVector works in nClasses", {
  nc <- nClass(
    classname = "test_RcppLogicalVector",
    Cpublic = list(
      x = "RcppLogicalVector",
      set_x = nFunction(fun = function(new_x = "RcppLogicalVector") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- c(TRUE, FALSE, FALSE, TRUE)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- c(TRUE, FALSE, FALSE, FALSE)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

test_that("RcppLogicalMatrix works in nClasses", {
  nc <- nClass(
    classname = "test_RcppLogicalMatrix",
    Cpublic = list(
      x = "RcppLogicalMatrix",
      set_x = nFunction(fun = function(new_x = "RcppLogicalMatrix") { 
        x <- new_x
        return(0) 
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- matrix(c(FALSE, FALSE, TRUE, FALSE), nrow = 2)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})

## # TODO: Why do the following tests not work?
## It looks like an Rcpp::DateVector requires an argument for construction.
## So it looks like we can accept the argument and use it but not create a new
## object on our own via nCompiler. Could be done with cppLiteral.
## test_that("RcppDateVector works in nClasses", {
##   nc <- nClass(
##     classname = "test_RcppDateVector",
##     Cpublic = list(
##       # x = "RcppDateVector",
##       set_x = nFunction(fun = function(new_x = "RcppDateVector") {
##         x <- new_x
##         return(0)
##       }, returnType = "integerScalar"
##       )
##     )
##   )
##   ncC <- nCompile(nc)
##   my_nc <- ncC$new()
##   test_x1 <- as.Date(c("2020-01-28", "2020-01-29", "2020-01-30", "2020-01-31"))
##   my_nc$set_x(test_x1)
##   expect_equal(my_nc$x, test_x1)
##   test_x2 <- as.Date(c("2019-01-28", "2019-01-29", "2019-01-30", "2019-01-31"))
##   my_nc$x <- test_x2
##   expect_equal(my_nc$x, test_x2)
## })
# test_that("RcppDatetimeVector works in nClasses", {
#   nc <- nClass(
#     classname = "test_RcppDatetimeVector",
#     Cpublic = list(
#       x = "RcppDatetimeVector",
#       set_x = nFunction(fun = function(new_x = "RcppDatetimeVector") { 
#         x <- new_x
#         return(0) 
#       }, returnType = "integerScalar"
#       )
#     )
#   )
#   ncC <- nCompile(nc)
#   my_nc <- ncC$new()
#   test_x1 <- as.POSIXct(c(1593166562, 1593066562, 1592166562, 159316562), 
#                         origin = as.Date("1950-01-01"))
#   my_nc$set_x(test_x1)
#   expect_equal(my_nc$x, test_x1)
#   test_x2 <- as.POSIXct(c(1493166562, 1593066562, 2592166562, 159316562), 
#                         origin = as.Date("1950-01-01"))
#   my_nc$x <- test_x2
#   expect_equal(my_nc$x, test_x2)
# })
# 
# 
test_that("RcppDataFrame works in nClasses", {
  nc <- nClass(
    classname = "test_RcppDataFrame",
    Cpublic = list(
      x = "RcppDataFrame",
      set_x = nFunction(fun = function(new_x = "RcppDataFrame") {
        x <- new_x
        return(0)
      }, returnType = "integerScalar"
      )
    )
  )
  ncC <- nCompile(nc)
  my_nc <- ncC$new()
  test_x1 <- data.frame(x = 1:10, y = 101:110)
  my_nc$set_x(test_x1)
  expect_equal(my_nc$x, test_x1)
  test_x2 <- data.frame(a = 1:100, b = 101:200)
  my_nc$x <- test_x2
  expect_equal(my_nc$x, test_x2)
})
