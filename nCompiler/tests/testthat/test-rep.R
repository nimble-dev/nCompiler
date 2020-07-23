# Not working
context('Testing rep operator')

test_that("Basic rep usage works", {
  nc <- nClass(
    Cpublic = list(
      nf1 = nFunction(
        function(x = numericVector()) {
          ans <- rep(x)
          return(ans)
          returnType(numericVector)
        }
      ),
      nf2 = nFunction(
        function(x = logicalVector, times = numericScalar) {
          ans <- rep(x, times)
          return(ans)
          returnType(logicalVector)
        }
      ),
      nf3 = nFunction(
        function(x = numericMatrix, times = numericScalar) {
          ans <- rep(x, times)
          return(ans)
          returnType(numericVector)
        }
      ),
      nf4 = nFunction(
        function(x = integerArray(nDim = 3), length.out = numericScalar) {
          ans <- rep(x, length.out = length.out)
          return(ans)
          returnType(integerVector)
        }
      ),
      ## nf5 = nFunction(
      ##   function(x = numericVector, length.out = numericScalar) {
      ##     ans <- 1 + rep(exp(x), length.out = length.out)
      ##     return(ans)
      ##     returnType(numericVector)
      ##   }
      ## ),
      nf6 = nFunction(
        function(x = numericVector, times = numericScalar, length.out = numericScalar) {
          ans <- rep(x, times, length.out)
          return(ans)
          returnType(numericVector)
        }
      ),
      nf7 = nFunction(
        function(x = numericMatrix, times = numericScalar) {
          ans <- exp(rep(x, times))
          return(ans)
          returnType(numericVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(1:7), 1:7)
  x1 <- c(TRUE, FALSE, TRUE)
  expect_equivalent(as.vector(nc_obj$nf2(x1, 3)), rep(x1, 3))
  x2 <- matrix(1:10, 2)
  expect_equivalent(nc_obj$nf3(x2, 7), rep(x2, 7))
  x3 <- array(1:24, c(2, 3, 4))
  expect_equivalent(nc_obj$nf4(x3, 13), rep(x3, length.out = 13))
  expect_equivalent(nc_obj$nf4(x3, 57), rep(x3, length.out = 57))
  ## expect_equivalent(nc_obj$nf5(1:7, 3), 1 + rep(max(1:7), length.out = 3))
  ## expect_equivalent(nc_obj$nf5(12:7, 24), 1 + rep(max(12:7), length.out = 24))
  expect_equivalent(nc_obj$nf6(7:1, 20, 9), rep(7:1, 20, 9))
  expect_equivalent(nc_obj$nf6(7:1, 20, 9), rep(7:1, 20, 9))
  expect_equivalent(nc_obj$nf7(x2, 3), exp(rep(x2, 3)))
})
