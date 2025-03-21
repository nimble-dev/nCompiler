
## parallel_for works
## parallel_reduce is broken

test_that("Simple parallel example works", {
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- x
          parallel_for(i, 1:10,
                       {y[i] <- 2 * x[i]},
                       "x", ## copy for each thread
                       "y") ## share across threads
          return(y)
        },
        returnType = 'numericVector'
      )
    )
  )
  
  # Cnc <- nCompile_nClass(nc, control = list(endStage = "makeCppDef"))
  # writeCode(Cnc$generate())
  # writeCode(Cnc$generate(TRUE))
  Cnc <- nCompile(nc)
  nc1 <- nc$new()
  Cnc1 <- Cnc$new()
  expect_equal(nc1$go(101:110), 2*(101:110))
  expect_equal(Cnc1$go(101:110), 2*(101:110))
})

test_that("Parallel reduction example works", {
  ## this doesn't work yet: see TODO beginning on line 187 of cppDefs_nClass.R
  ## nc <- nClass(
  ##   Cpublic = list(
  ##     reduction_fun = nFunction(
  ##       fun = function(x = 'numericScalar', y = 'numericScalar') {
  ##         ans <- x + y
  ##         return(ans)
  ##       },
  ##       returnType = 'numericScalar'
  ##     ),
  ##     parallel_fun = nFunction(
  ##       fun = function(x = 'numericVector') {
  ##         y <- parallel_reduce(reduction_fun, x, 0)
  ##         return(y)
  ##       },
  ##       returnType = 'numericVector'
  ##     )
  ##   )
  ## )

  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce('+', x, 0)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )

  # Cnc <- nCompile_nClass(nc, control = list(endStage = "makeCppDef"))
  # writeCode(Cnc$generate())
  # writeCode(Cnc$generate(TRUE))
  Cnc <- nCompile(nc)
  nc1 <- nc$new()
  Cnc1 <- Cnc$new()
  expect_equal(nc1$go(101:110), sum(101:110))
  expect_equal(Cnc1$go(101:110), sum(101:110))
})
