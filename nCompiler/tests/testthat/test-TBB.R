context("Testing parallelization via TBB.")

test_that("Simple parallel example works", {
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- x
          parallel_for(i, 1:10,
                       {
                         y[i] <- 2 * x[i]
                       },
                       "x",
                       "y")
          return(y)
        },
        returnType = 'numericVector'
      )
    )
  )

  # Cnc <- nCompile_nClass(nc, control = list(endStage = "makeCppDef"))
  # writeCode(Cnc$generate())
  # writeCode(Cnc$generate(TRUE))
  
  Cnc <- nCompile_nClass(nc)
  nc1 <- nc$new()
  Cnc1 <- Cnc$new()
  expect_equal(nc1$go(101:110), 2*(101:110))
  expect_equal(Cnc1$go(101:110), array(2*(101:110)))
})
