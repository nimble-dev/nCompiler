test_that("basic usage of parallel_reduce", {
    ## newly failing with threads stuff
    library(nCompiler);paciorek=7
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


  ## With additional code.
  nc <- nClass(
      Cpublic = list(
          go = nFunction(
              fun = function(x = 'numericVector') {
                  tmp <- 7    
                  y <- 3 + exp(parallel_reduce('+', x, 0))
                  return(y)
              },
              returnType = 'numericScalar'
          )
      )
  )
  Cnc <- nCompile(nc)
  Cobj <- Cnc$new()
  expect_identical(Cobj$go(1:3), exp(6)+3)

  ## Use in `return()`.
  nc <- nClass(
    Cpublic = list(
        go = nFunction(
        fun = function(x = 'numericVector') {
          return(3+parallel_reduce('+', x, 0))
        },
        returnType = 'numericScalar'
      )
    )
  )
  Cnc <- nCompile(nc)
  Cobj <- Cnc$new()
  expect_identical(Cobj$go(1:3), 9)
  
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

    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- parallel_reduce(reduction_fun, x, 0) 
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

    ## See issue 133. This should work now.
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
    Cnc <- nCompile(nc)
    Cobj = Cnc$new()
    Cobj$go(1:5)


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
    Cnc <- nCompile(nc, nc0)
    Cobj0 <- Cnc[[2]]$new()
    Cobj = Cnc[[1]]$new()
    Cobj$go(1:5, Cobj0)

    nc <- nClass(
        Cpublic = list(
            obj = 'nc0',
            parallel_fun = nFunction(
                fun = function(x = 'numericVector') {
                    y <- parallel_reduce(obj$reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    Cnc <- nCompile(nc, nc0)
    Cobj = Cnc[[1]]$new()
    Cobj$obj <- Cnc[[2]]$new()
    Cobj$go(1:5, Cobj0)
    
    nc <- nClass(
        Cpublic = list(
            obj = 'nc0',
            parallel_fun = nFunction(
                fun = function(x = 'numericVector') {
                    obj <<- nc0$new()
                    y <- parallel_reduce(obj$reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    Cnc <- nCompile(nc, nc0)
    Cobj = Cnc[[1]]$new()
    Cobj$obj <- Cnc[[2]]$new()
    Cobj$go(1:5, Cobj0)
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
  
nc1 <- nClass(
    Cpublic = list(
        x = 'numericVector'
        )
)

nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(o = 'nc1') {
            y <- parallel_reduce('+', o$x, 0)
            return(y)
        },
        returnType = 'numericScalar'
      )
    )
)
Cnc <- nCompile(nc,nc1)

nc1 <- nClass(
    Cpublic = list(
        y = 'nc2'
    ))

nc2 <- nClass(
    Cpublic = list(
        plus = nFunction(
          fun = function(x = 'numericScalar', y = 'numericScalar') {
              ans <- x + y
              return(ans)
          },
          returnType = 'numericScalar'
        )
    )
)

nc <- nClass(
    Cpublic = list(
      go = nFunction(
        fun = function(x = 'numericVector', o = 'nc1') {
            y <- parallel_reduce(o$y$plus, x, 0)
            return(y)
        },
        returnType = 'numericScalar'
      )
    )
)
Cnc <- nCompile(nc,nc1, nc2)

})

## Not clear why one would do this, but we do allow it.
test_that("reduction without assignment", {
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    tmp <- 7
                    parallel_reduce('+', x, 0)
                    return(0)
                },
                returnType = 'numericScalar'
            )
        )
    )
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(Cobj$go(1:3), 0)
    
    
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    3 + exp(parallel_reduce('+', x, 0))
                    return(0)
                },
                returnType = 'numericScalar'
            )
        )
    )
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(Cobj$go(1:3), 0)
})

test_that("multiple reduction functions", {
    nc <- nClass(
    Cpublic = list(
        go = nFunction(
        fun = function(x = 'numericVector', z = 'numericVector') {
          y <- parallel_reduce('+', x, 0) + 3*parallel_reduce('+',z,0)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
    )
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(Cobj$go(1:3, 4:7), sum(1:3)+3*sum(4:7)))

    nc <- nClass(
    Cpublic = list(
        go = nFunction(
        fun = function(x = 'numericVector', z = 'numericVector') {
          y <- parallel_reduce('+', x, 0) 
          return(y + 3*parallel_reduce('+',z,0))
        },
        returnType = 'numericScalar'
      )
    )
    )
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(Cobj$go(1:3, 4:7), sum(1:3)+3*sum(4:7)))

## Nested case.
nc <- nClass(
    Cpublic = list(
        adder = nFunction(
            fun = function(x = 'numericScalar', y = 'numericScalar') {
                z <- 1:5
                tmp <- parallel_reduce('+',z,0)
              ans <- x + y + tmp
              return(ans)
          },
          returnType = 'numericScalar'
    ),
        go = nFunction(
        fun = function(x = 'numericVector') {
            y <- parallel_reduce('adder', x, 0)
          return(y)
        },
        returnType = 'numericScalar'
      )
    )
  )
Cnc <- nCompile(nc)
o = Cnc$new()
o$go(1:5)


})

    ## test parallel_reduce(o$adder) with o either passed as arg or as member data and from nc1 class; check it works in R too



