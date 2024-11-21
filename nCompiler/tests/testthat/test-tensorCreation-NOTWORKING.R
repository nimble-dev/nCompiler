# The last test breaks because of C++ error on casting a double to long.

test_that("tensor creation C++ implementation works", {
  library(Rcpp)
  cppfile <- system.file(file.path('tests', 'testthat', 'cpp', 'tensorCreation_tests.cpp'), package = 'nCompiler')
  test <- nCompiler:::QuietSourceCpp(cppfile)
  expect_equivalent(tensorCreation1(1, 10), rep(1, 10))
  expect_equal(tensorCreation2(1:6), matrix(1:6, 2))
  expect_equal(tensorCreation3(1:12), array(1:12, c(2, 3, 2)))
  x <- c(3, 2, 4, 7)
  expect_equal(tensorCreation4(TRUE, x), array(TRUE, x))
  x <- c(2, 3, 2)
  expect_equal(tensorCreation5(1:12, x), array(1:12, x))
})

test_that("data initialization working: nNumeric, nInteger, nLogical", {
  nc <- nClass(
    classname = "nc_init",
    Cpublic = list(
      # dummy = nFunction(function() return(10), returnType = "numericScalar")
      nf1 = nFunction(
        function() {
          ans <- numeric() + integer() + logical()
          return(ans)
          returnType(numericVector)
        }
      ),
    nf2 = nFunction(
      function(x = "numericVector",
               y = "integerVector",
               z = "logicalVector") {
        ans <- numeric(value = x, length = 4) + exp(integer(value = y, length = 4)) *
          logical(value = z, length = 4)
        return(ans)
        returnType("numericVector")
      }
    ),
      nf3 = nFunction(
        function() {
          ans <- numeric(length = 7) + exp(integer(length = 7)) -
            logical(length = 7)
          return(ans)
          returnType("numericVector")
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(), nc$public_methods$nf1())
  expect_equivalent(
    nc_obj$nf2(1:4, 3:6, c(TRUE, TRUE, FALSE, TRUE)),
    nc$public_methods$nf2(1:4, 3:6, c(TRUE, TRUE, FALSE, TRUE))
  )
  expect_equivalent(nc_obj$nf3(), nc$public_methods$nf3())
})

test_that("data initialization working: nMatrix", {
  nc <- nClass(
    Cpublic = list(
      nf1 = nFunction(
        function() {
          ans <- nMatrix()
          return(ans)
          returnType(numericMatrix)
        }
      ),
      nf2 = nFunction(
        function() {
          ans <- exp(nMatrix(type = 'logical', nrow = 3, ncol = 7)) +
            nMatrix(type = 'double', nrow = 3, ncol = 7) -
            nMatrix(type = 'integer', nrow = 3, ncol = 7)
          return(ans)
          returnType(numericMatrix)
        }
      ),
      nf3 = nFunction(
        function(x = integerVector) {
          ans <- nMatrix(type = 'integer', nrow = 7, ncol = 2, value = x)
          return(ans)
          returnType(integerMatrix)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(), nc$public_methods$nf1())
  expect_equivalent(nc_obj$nf2(), nc$public_methods$nf2())
  expect_equivalent(nc_obj$nf3(1:14), nc$public_methods$nf3(1:14))
})

test_that("data initialization working: nArray", {
  nc <- nClass(
    Cpublic = list(
      nf1 = nFunction(
        function() {
          ans <- nArray()
          return(ans)
          returnType(numericArray(nDim = 2))
        }
      ),
      nf2 = nFunction(
        function() {
          ans <- nArray(type = 'logical', dim = c(2, 4, 3)) +
            nArray(type = 'double', dim = c(2, 4, 3)) -
            exp(nArray(type = 'integer', dim = c(2, 4, 3)))
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      ),
      nf3 = nFunction(
        function(x = integerVector) {
          ans <- nArray(type = 'integer', dim = x, nDim = 4)
          return(ans)
          returnType(integerArray(nDim = 4))
        }
      ),
      nf4 = nFunction(
        function(x = integerVector) {
          ans <- nArray(type = 'integer', value = x, dim = c(3, 2, 5))
          return(ans)
          returnType(integerArray(nDim = 3))
        }
      ),
      nf5 = nFunction(
        function(x = integerVector, y = integerVector) {
          ans <- nArray(type = 'integer', value = x, dim = y, nDim = 4)
          return(ans)
          returnType(integerArray(nDim = 4))
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(), nc$public_methods$nf1())
  expect_equivalent(nc_obj$nf2(), nc$public_methods$nf2())
  expect_equivalent(nc_obj$nf3(1:4), nc$public_methods$nf3(1:4))
  expect_equivalent(nc_obj$nf4(1:30), nc$public_methods$nf4(1:30))
  expect_equivalent(
    nc_obj$nf5(1:168, c(2,4,3,7)),
    nc$public_methods$nf5(1:168, c(2,4,3,7))
  )
})
