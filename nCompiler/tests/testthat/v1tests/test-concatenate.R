
# doesn't work! code-generation needs to be developed
test_that("concatenate works", {
  nc <- nClass(
    Cpublic = list(
      c_vd_vd = nFunction(
        function(x = numericVector, y = numericVector) {
          ans <- c(x, y)
          returnType(numericVector)
          return(ans)
        }
      )
    )
  )
  ncc <- nCompile(nc)
  nco <- ncc$new()
})
