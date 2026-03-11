test_that("basic usage of parallel_reduce", {
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce('+', x)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
  Cnc <- nCompile(nc)
  obj <- nc$new()
  Cobj <- Cnc$new()
  expect_identical(obj$go(101:110), as.numeric(sum(101:110)))
  expect_identical(Cobj$go(101:110), as.numeric(sum(101:110)))

  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce('+', x, 5)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
  Cnc <- nCompile(nc)
  obj <- nc$new()
  Cobj <- Cnc$new()
  expect_identical(obj$go(101:110), as.numeric(5+sum(101:110)))
  expect_identical(Cobj$go(101:110), as.numeric(5+sum(101:110)))

  ## Negative values required some additional processing, so test that case explicitly.
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce('+', x, -5)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
  Cnc <- nCompile(nc)
  obj <- nc$new()
  Cobj <- Cnc$new()
  expect_identical(obj$go(101:110), as.numeric(sum(101:110)-5))
  expect_identical(Cobj$go(101:110), as.numeric(sum(101:110)-5))

  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce('pairmin', x)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
  Cnc <- nCompile(nc)
  obj <- nc$new()
  Cobj <- Cnc$new()
  x <- c(3.7, 2.5, 4.9, 3.1)
  expect_identical(obj$go(x), 2.5)
  expect_identical(Cobj$go(x), 2.5)

  ## Operator as function (user-defined), not char.
  mypairmin <- nFunction(
      fun = function(x = 'numericScalar', y = 'numericScalar') {
          return(pmin(x,y))
      }, returnType = 'numericScalar'
  )
  
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce(mypairmin, x, Inf)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
  Cnc <- nCompile(nc, mypairmin)[[1]]
  obj <- nc$new()
  Cobj <- Cnc$new()
  x <- c(3.7, 2.5, 4.9, 3.1)
  expect_identical(obj$go(x), 2.5)
  expect_identical(Cobj$go(x), 2.5)

})

test_that("error trapping for parallel_reduce", {
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce('-', x)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
  ## The error message is not silent. 
  expect_error(Cnc <- nCompile(nc), "is not a valid reduction")  ## Compile-time error.
  obj <- nc$new()
  expect_error(obj$go(1:5), "not a valid reduction")  ## Run-time error.

  ## No init for user-defined reduction function.
  mypairmin <- nFunction(
      fun = function(x = 'numericScalar', y = 'numericScalar') {
          return(pmin(x,y))
      }, returnType = 'numericScalar'
  )
  
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector') {
          y <- parallel_reduce(mypairmin, x)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
  expect_error(Cnc <- nCompile(nc, mypairmin)[[1]], "expected 3 arguments")
  obj <- nc$new()
  expect_error(obj$go(1:5), "no default value provided")


  nc <- nClass(
      Cpublic = list(
          go = nFunction(
              fun = function(x = 'numericVector', start = 'numericScalar') {
                  y <- parallel_reduce('pairmin', x, start)
                  return(y)
              },
              returnType = 'numericScalar'
          )
      )
  )
  expect_error(Cnc <- nCompile(nc), "must be a literal")

  go = nFunction(
      fun = function(x = 'numericVector') {
          y <- parallel_reduce('+', x)
          return(y)
      },
      returnType = 'numericScalar'
  )
  expect_error(Cgo <- nCompile(go), "must be used in a method of an nClass")

})

## Could add check for user-defined reduction function with defined default init via operatorDef.

test_that("user-defined reduction functions", {
    ## User-defined nFunction
    reduction_fun <- nFunction(
        fun = function(x = 'numericScalar', y = 'numericScalar') {
            ans <- x + y
            return(ans)
        },
        returnType = 'numericScalar'
    )
    
    
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- parallel_reduce('reduction_fun', x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    Cnc <- nCompile(nc, reduction_fun)[[1]]
    obj <- nc$new()
    Cobj <- Cnc$new()
    expect_identical(obj$go(101:110), as.numeric(sum(101:110)))
    expect_identical(Cobj$go(101:110), as.numeric(sum(101:110)))
 
    ## See issue 133.
    nc <- nClass(
        Cpublic = list(
            reduction_fun = nFunction(
                fun = function(x = 'numericScalar', y = 'numericScalar') {
                    ans <- x + y
                    return(ans)
                },
                returnType = 'numericScalar'
            ),
            parallel_fun = nFunction(
                fun = function(x = 'numericVector') {
                    y <- parallel_reduce(reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    ## This particular error doesn't fit well with testthat for some reason.
    ## expect_error(Cnc <- nCompile(nc))

    nc0 <- nClass(
        Cpublic = list(
            reduction_fun = nFunction(
                fun = function(x = 'numericScalar', y = 'numericScalar') {
                    ans <- x + y
                    return(ans)
                },
                returnType = 'numericScalar'
            )
        ))
    
    nc <- nClass(
        Cpublic = list(
            parallel_fun = nFunction(
                fun = function(x = 'numericVector', obj = 'nc0') {
                    y <- parallel_reduce(obj$reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    expect_error(result <- nCompile(nc, nc0), "not a valid reduction")
    
})


test_that("reduction cases that don't work", {
  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector', y = 'numericVector') {
          z <- parallel_reduce('+', x+y)
          return(z)
        },
        returnType = 'numericScalar'
      )
    )
  )
  expect_error(Cnc <- nCompile(nc))
  obj <- nc$new()
  obj$go(1:5, 6:10)
  expect_identical(obj$go(1:5, 6:10) , as.numeric(55))

  nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'integerVector') {
          z <- parallel_reduce('pairmin', x)
          return(z)
        },
        returnType = 'integerScalar'
      )
    )
  )
  expect_error(Cnc <- nCompile(nc))  ## Lots of C++ compiler output.
  
})


