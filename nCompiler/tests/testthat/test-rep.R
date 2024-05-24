# NOT WORKING
context('Testing rep operator')

test_that("Basic rep usage works", {
  nc <- nClass(
    Cpublic = list(
      ## repTimes
      nf1 = nFunction(
        function(x = numericVector()) {
          ans <- 1 + log(rep(exp(x)))
          return(ans)
          returnType(numericVector)
        }
      ),
      nf1b = nFunction(
        function(x = numericScalar) {
          ans <- 1 + log(rep(exp(x), 3))
          return(ans)
          returnType(numericVector)
        }
      ),
      nf2 = nFunction(
        function(x = logicalVector, times = numericScalar) {
          ans <- rep(x, times = times)
          return(ans)
          returnType(logicalVector)
        }
      ),
      nf3 = nFunction(
        function(x = numericMatrix, times = numericScalar) {
          ans <- 1 + log(rep(exp(x), times))
          return(ans)
          returnType(numericVector)
        }
      ),
      ## repLen
      nf4 = nFunction( ## PROBLEM IF THERE IS ARITHMETIC HERE
        function(x = integerArray(nDim = 3), length.out = numericScalar) {
          ans <- rep(x, length.out = length.out)
          return(ans)
          returnType(integerVector)
        }
      ),
      nf5 = nFunction(
        function(x = numericVector, length.out = numericScalar) {
          ans <- 1 + rep(exp(x), length.out = length.out)
          return(ans)
          returnType(numericVector)
        }
      ),
      ## rep with two unnamed args
      nf6 = nFunction(
        function(x = numericVector, times = numericScalar, length.out = numericScalar) {
          ans <- 1 + log(rep(exp(x), times, length.out))
          return(ans)
          returnType(numericVector)
        }
      ),
      ## repEach
      nf7 = nFunction(
        function(x = numericVector, each = numericScalar) {
          ans <- 1 + log(rep(exp(x), each = each))
          return(ans)
          returnType(numericVector)
        }
      ),
      nf7b = nFunction(
        function(x = numericScalar, each = numericScalar) {
          ans <- 1 + log(rep(exp(x), each = each))
          return(ans)
          returnType(numericVector)
        }
      ),
      nf8 = nFunction(
        function(x = numericMatrix, each = numericScalar) {
          ans <- 1 + log(rep(exp(x), each = each))
          return(ans)
          returnType(numericVector)
        }
      ),
      ## repTimesEach
      nf9 = nFunction(
        function(x = numericVector, times = numericScalar, each = numericScalar) {
          ans <- 1 + log(rep(exp(x), times = times, each = each))
          return(ans)
          returnType(numericVector)
        }
      ),
      ## repLenEach
      nf10 = nFunction(
        function(x = numericVector, each = numericScalar, length.out = numericScalar) {
          ans <- 1 + log(rep(exp(x), each = each, length.out = length.out))
          return(ans)
          returnType(numericVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(1:7), 1 + log(exp(1:7)))
  expect_equivalent(nc_obj$nf1b(7), 1 + log(exp(rep(7, 3))))
  x1 <- c(TRUE, FALSE, TRUE)
  expect_equivalent(as.vector(nc_obj$nf2(x1, 3)), rep(x1, 3))
  x2 <- matrix(1:10, 2)
  expect_equivalent(nc_obj$nf3(x2, 7), 1 + log(rep(exp(x2), 7)))
  x3 <- array(1:24, c(2, 3, 4))
  expect_equivalent(nc_obj$nf4(x3, 13), rep(x3, length.out = 13))
  expect_equivalent(nc_obj$nf4(x3, 57), rep(x3, length.out = 57))
  expect_equivalent(nc_obj$nf5(1:7, 3), 1 + rep(exp(1:7), length.out = 3))
  expect_equivalent(nc_obj$nf5(12:7, 24), 1 + rep(exp(12:7), length.out = 24))
  expect_equivalent(nc_obj$nf6(7:1, 20, 9), 1 + log(rep(exp(7:1), 20, 9)))
  expect_equivalent(nc_obj$nf7(1:7, 3), 1 + log(rep(exp(1:7), each = 3)))
  expect_equivalent(nc_obj$nf7b(7, 3), 1 + log(rep(exp(7), each = 3)))
  expect_equivalent(nc_obj$nf8(x2, 3), 1 + log(rep(exp(x2), each = 3)))
  expect_equivalent(nc_obj$nf9(1:5, 3, 2), 1 + log(rep(exp(1:5), times = 3, each = 2)))
  expect_equivalent(nc_obj$nf10(1:5, 3, 2), 1 + log(rep(exp(1:5), each = 3, length.out =2)))
})

## To do: Try adding EIGEN_STRONG_INLINE
