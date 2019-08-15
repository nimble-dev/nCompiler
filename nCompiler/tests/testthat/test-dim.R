context('Testing dim operator')

test_that("Basic dim usage works", {
  nc <- nClass(
    Cpublic = list(
      nf1 = nFunction(
        function(x = numericVector()) {
          ans <- dim(x)
          return(ans)
          returnType(integerVector())
        }
      ),
      nf2 = nFunction(
        function(x = logicalMatrix()) {
          ans <- dim(x)
          return(ans)
          returnType(integerVector())
        }
      ),
      nf3 = nFunction(
        function(x = integerArray(nDim = 3)) {
          ans <- dim(x)
          return(ans)
          returnType(integerVector())
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(1:7), 7)
  expect_equivalent(nc_obj$nf2(matrix(1:10, 5)), c(5, 2))
  expect_equivalent(nc_obj$nf3(array(1:12, dim = c(2, 3, 2))), c(2, 3, 2))
}
