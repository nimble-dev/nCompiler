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
  obj <- nc$new()
  Cobj <- Cnc$new()
  expect_identical(obj$go(1:3), exp(6)+3)
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
  obj <- nc$new()
  Cobj <- Cnc$new()
  expect_identical(Cobj$go(1:3), 9)
  expect_identical(obj$go(1:3), 9)

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
  expect_error(Cnc <- nCompile(nc, mypairmin), "no default `init`")
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
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- parallel_reduce(reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    Cnc <- nCompile(nc)
    obj <- nc$new()
    Cobj = Cnc$new()
    expect_identical(obj$go(1:5), 15)
    expect_identical(Cobj$go(1:5), 15)
    
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
            go = nFunction(
                fun = function(x = 'numericVector', obj = 'nc0') {
                    y <- parallel_reduce(obj$reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )

    obj0 <- nc0$new()
    obj = nc$new()
    expect_identical(obj$go(1:5, obj0), 15)
    
    Cnc <- nCompile(nc, nc0)
    Cobj0 <- Cnc[[2]]$new()
    Cobj = Cnc[[1]]$new()
    expect_identical(Cobj$go(1:5, Cobj0), 15)

    nc <- nClass(
        Cpublic = list(
            obj = 'nc0',
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- parallel_reduce(obj$reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    obj <- nc$new()
    obj$obj <- nc0$new()
    expect_identical(obj$go(1:5), 15)
    
    Cnc <- nCompile(nc, nc0)
    Cobj = Cnc[[1]]$new()
    expect_identical(Cobj$go(1:5), 15)
    
    nc <- nClass(
        Cpublic = list(
            obj = 'nc0',
            go = nFunction(
                fun = function(x = 'numericVector') {
                    obj <<- nc0$new()
                    y <- parallel_reduce(obj$reduction_fun, x, 0) 
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    obj <- nc$new()
    expect_identical(obj$go(1:5), 15)
    
    Cnc <- nCompile(nc, nc0)
    Cobj = Cnc[[1]]$new()
    expect_identical(Cobj$go(1:5), 15)

    # Using `self`. Currently we can't handle `self$x` as the vector just as we can't handle `obj$x`.
    # We also can't handle the `init` not being a literal.
    nc <- nClass(
      Cpublic = list(
        reduction_fun = nFunction(
          fun = function(x = 'numericScalar', y = 'numericScalar') {
            ans <- x + y
            return(ans)
          },
          returnType = 'numericScalar'
        ),
        go = nFunction(
          fun = function(x = 'numericVector') {
            y <- parallel_reduce(self$reduction_fun, x, 0)
            return(y)
          },
          returnType = 'numericScalar'
        )
      )
    )
    Cnc <- nCompile(nc)
    obj <- nc$new()
    Cobj = Cnc$new()
    expect_identical(obj$go(1:5), 15)
    expect_identical(Cobj$go(1:5), 15)

})


test_that("reduction cases that don't work", {
  ## This doesn't work at present but we should make it work, presumably by lifting the `object` expression.  
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
  expect_error(Cnc <- nCompile(nc), 'found an expression')

  ## Similar issue for this use case.
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
  expect_error(Cnc <- nCompile(nc,nc1), 'found an expression')
  
  ## Issue 136. This should work, but some type issue.
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
  expect_error(out <- capture.output(nCompile(nc)))  ## Lots of C++ compiler output.

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
                  y <- parallel_reduce(o$y$plus, x)
                  return(y)
              },
              returnType = 'numericScalar'
          )
      )
  )
  expect_error(Cnc <- nCompile(nc,nc1, nc2),  'too many levels of class hierarchy')

})

## Not clear why one would do this, but we do allow it.
test_that("reduction without assignment", {
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    tmp <- 7
                    parallel_reduce('+', x)
                    return(0)
                },
                returnType = 'numericScalar'
            )
        )
    )
    obj <- nc$new()
    expect_identical(obj$go(1:3), 0)
    
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(Cobj$go(1:3), 0)
    
    
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    3 + exp(parallel_reduce('+', x))
                    return(0)
                },
                returnType = 'numericScalar'
            )
        )
    )
    obj <- nc$new()
    expect_identical(obj$go(1:3), 0)
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(Cobj$go(1:3), 0)
})

test_that("multiple reduction functions", {
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector', z = 'numericVector') {
                    y <- parallel_reduce('+', x) + 3*parallel_reduce('+',z)
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    obj <- nc$new()
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(obj$go(1:3, 4:7), sum(1:3)+3*sum(4:7))
    expect_identical(Cobj$go(1:3, 4:7), sum(1:3)+3*sum(4:7))

    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector', z = 'numericVector') {
                    y <- parallel_reduce('+', x) 
                    return(y + 3*parallel_reduce('+',z,0))
                },
                returnType = 'numericScalar'
            )
        )
    )
    obj <- nc$new()
    Cnc <- nCompile(nc)
    Cobj <- Cnc$new()
    expect_identical(obj$go(1:3, 4:7), sum(1:3)+3*sum(4:7))
    expect_identical(Cobj$go(1:3, 4:7), sum(1:3)+3*sum(4:7))

    ## Nested case.
    nc <- nClass(
        Cpublic = list(
            adder = nFunction(
                fun = function(x = 'numericScalar', y = 'numericScalar') {
                    z <- 1:5
                    tmp <- parallel_reduce('+',z)
                    ans <- x + y + tmp
                    return(ans)
                },
                returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- parallel_reduce(adder, x, 0)
                    return(y)
                },
                returnType = 'numericScalar'
            )
        )
    )
    obj <- nc$new() 
    Cnc <- nCompile(nc)
    Cobj = Cnc$new()
    expect_identical(obj$go(1:5), 90) # Note that this would not work if use `parallel_reduce('adder', x, 0)`.
    expect_identical(Cobj$go(1:5), 90)
    
})




