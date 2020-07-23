# Not working
context("Testing seq and colon operators")

test_that("Basic colon usage works", {
  nf <- nFunction(
   function(x = numericScalar, y = numericScalar) {
     ans <- x:y
     return(ans)
     returnType(numericVector)
   }
  )
  nfc <- nCompile(nf)
  expect_equivalent(nfc(2, 7), 2:7)
})

test_that("Basic seq usage works", {
  nc <- nClass(
    Cpublic = list(
      nf1 = nFunction(
        function() {
          ans <- seq()
          return(ans)
          returnType(numericScalar)
        }
      ),
      nf2 = nFunction(
        function(x = numericScalar) {
          ans <- seq(x)
          return(ans)
          returnType(numericVector)
        }
      ),
      nf3 = nFunction(
        function(x = numericScalar, y = numericScalar) {
          ans <- seq(x, y)
          return(ans)
          returnType(numericVector)
        }
      ),
      nf4 = nFunction(
        function(x = numericScalar) {
          ans <- seq(length.out = x)
          return(ans)
          returnType(numericVector)
        }
      ),
      nf5 = nFunction(
        function(x = numericScalar, y = numericScalar, z = numericScalar) {
          ans <- seq(x, y, z)
          return(ans)
          returnType(numericVector)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
  expect_equivalent(nc_obj$nf1(), 1)
  expect_equivalent(nc_obj$nf2(7), 1:7)
  expect_equivalent(nc_obj$nf3(2, 7), 2:7)
  expect_equivalent(nc_obj$nf4(7), 1:7)
  expect_equivalent(nc_obj$nf4(7.3), 1:8)
  expect_equivalent(nc_obj$nf5(7, 20, 1.3), seq(7, 20, 1.3))
})
