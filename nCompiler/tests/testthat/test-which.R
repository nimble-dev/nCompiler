test_that("Basic which usage works", {
  nc <- nClass(
    Cpublic = list(
      nf1 = nFunction(
        function(x = logicalScalar) {
          ans <- which(x)
          return(ans)
          returnType(integerVector)
        }
      ),
      nf2 = nFunction(
        function(x = logicalVector) {
          ans <- which(x)
          return(ans)
          returnType(integerVector)
        }
      ),
      nf3 = nFunction(
        function(x = numericVector()) {
          ans <- which(exp(x) > 1)
          return(ans)
          returnType(integerVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(TRUE), 1)
  expect_equivalent(nc_obj$nf1(FALSE), which(FALSE))
  x1 <- c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
  expect_equivalent(nc_obj$nf2(x1), which(x1))
  set.seed(0)
  x2 <- rnorm(10)
  expect_equivalent(nc_obj$nf3(x2), which(exp(x2) > 1))
})
