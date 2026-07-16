test_that("basic usage of parallel_for", {
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    mult <- 2
                    parallel_for(i, 1:length(x), {y[i] <- mult*x[i]})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    obj <- nc$new()
    Cobj <- Cnc$new()
    expect_identical(obj$go(2:6), as.numeric(2*(2:6)))
    expect_identical(Cobj$go(2:6), as.numeric(2*(2:6)))
    
    nc <- nClass(
        Cpublic = list(
            myconst = 'numericScalar',
            twice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(2*x)
                }, returnType = 'numericScalar'
            ),
            thrice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(3*x)
                }, returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x), {y[i] <- myconst + twice(thrice(x[i]))})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    obj <- nc$new()
    obj$myconst <- 7
    Cobj <- Cnc$new()
    Cobj$myconst <- 7
    expect_identical(obj$go(2:6), as.numeric(6*(2:6)+7))
    expect_identical(Cobj$go(2:6), as.numeric(6*(2:6)+7))

    mult = nFunction(
        fun=function(x = 'numericScalar', c = 'numericScalar') {
            return(c*x)
        }, returnType = 'numericScalar'
    )
    
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector'){
                    y <- x
                    parallel_for(i, 1:length(x), {y[i] <- mult(x[i], 3)})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc,mult)[[1]]
    obj <- nc$new()
    Cobj <- Cnc$new()
    expect_identical(obj$go(2:6), as.numeric(3*(2:6)))
    expect_identical(Cobj$go(2:6), as.numeric(3*(2:6)))
    
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    mult <- 2
                    parallel_for(i, 2:length(x), {y[i] <- mult*x[i]})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    obj <- nc$new()
    Cobj <- Cnc$new()
    expect_identical(obj$go(2:6), as.numeric(c(2, 2*(3:6))))
    expect_identical(Cobj$go(2:6), as.numeric(c(2, 2*(3:6))))
 })

test_that("use of `self`", {
  nc <- nClass(
        Cpublic = list(
            myconst = 'numericScalar',
            twice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(2*x)
                }, returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x), {y[i] <- self$myconst + twice(x[i])})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
  )
  Cnc <- nCompile(nc)
  obj <- nc$new()
  obj$myconst <- 1
  Cobj <- Cnc$new()
  Cobj$myconst <- 1
  expect_identical(obj$go(2:6), as.numeric(1 + 2*2:6)) 
  expect_identical(Cobj$go(2:6), as.numeric(1 + 2*2:6)) 

  nc <- nClass(
        Cpublic = list(
            myconst = 'numericScalar',
            twice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(2*x)
                }, returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x), {y[i] <- myconst + self$twice(x[i])})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
  )
  Cnc <- nCompile(nc)
  obj <- nc$new()
  obj$myconst <- 1
  Cobj <- Cnc$new()
  Cobj$myconst <- 1
  expect_identical(obj$go(2:6), as.numeric(1 + 2*2:6))
  expect_identical(Cobj$go(2:6), as.numeric(1 + 2*2:6))

  nc <- nClass(
        Cpublic = list(
            myconst = 'numericScalar',
            twice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(2*x)
                }, returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x), {y[i] <- self$twice(self$myconst + x[i])})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
  )
  Cnc <- nCompile(nc)
  Cnc <- nCompile(nc)
  obj <- nc$new()
  obj$myconst <- 1
  Cobj <- Cnc$new()
  Cobj$myconst <- 1
  expect_identical(obj$go(2:6), as.numeric(2*3:7))
  expect_identical(Cobj$go(2:6), as.numeric(2*3:7))

  nc <- nClass(
        Cpublic = list(
            myconst = 'numericScalar',
            myotherconst = 'numericScalar',
            twice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(2*x)
                }, returnType = 'numericScalar'
            ),
            thrice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(3*x)
                }, returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                  y <- x
                  tmp <- self$myotherconst + 3   # Use `self` outside of parallel body.
                  parallel_for(i, 1:length(x), {y[i] <- tmp + self$myconst + myotherconst + self$twice(thrice(x[i]))})
                  return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    
    Cnc <- nCompile(nc)
    obj <- nc$new()
    obj$myconst <- 7
    obj$myotherconst <- 1
    Cobj <- Cnc$new()
    Cobj$myconst <- 7
    Cobj$myotherconst <- 1
    expect_identical(obj$go(2:6), as.numeric(6*(2:6)+12))
    expect_identical(Cobj$go(2:6), as.numeric(6*(2:6)+12))
})

test_that("use of object from another class", {
    nc0 <- nClass(
        Cpublic = list(
            foo = 'numericScalar',
            twice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(2*x)
                }, returnType = 'numericScalar'
            )
        )
    )
    
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector', o = 'nc0') {
                    y <- x
                    parallel_for(i, 1:length(x),  
                    {y[i] <- o$twice( x[i]) + o$foo },
                    )
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc,nc0)

    tmp1 <- nc0$new()
    tmp1$foo <- 5
    nc1 <- nc$new()
    expect_identical(nc1$go(1:5, tmp1), as.numeric(2*(1:5)+5))
    
    Ctmp1 <- Cnc[[2]]$new()
    Ctmp1$foo <- 5
    Cnc1 <- Cnc[[1]]$new()
    expect_identical(Cnc1$go(1:5, Ctmp1), as.numeric(2*(1:5)+5))

    nc <- nClass(
        Cpublic = list(
            o = 'nc0',
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x),  
                    {y[i] <- o$twice( x[i]) + o$foo },
                    )
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc,nc0)

    nc1 <- nc$new()
    o <- nc0$new()
    o$foo <- 5
    nc1$o <- o
    expect_identical(nc1$go(1:5), as.numeric(2*(1:5)+5))
    Cnc1 <- Cnc[[1]]$new()
    Co <- Cnc[[2]]$new()
    Co$foo <- 5
    Cnc1$o <- Co
    expect_identical(Cnc1$go(1:5), as.numeric(2*(1:5)+5))

})

test_that("specifying {copy,share}Vars", {
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x),  
                    {y[i] <- 2*x[i]; x[i] <- 0},
                    copyVars = 'x',
                    )
                    return(x)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    Cnc1 <- Cnc$new()
    expect_identical(Cnc1$go(1:5), as.numeric(1:5))
    
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x),  
                    {y[i] <- 2*x[i]; x[i] <- 0},
                    shareVars = 'x'  # Also the default, so this is not really needed.
                    )
                    return(x)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    Cnc1 <- Cnc$new()
    expect_identical(Cnc1$go(1:5), as.numeric(rep(0,5)))
    
    
    nc <- nClass(
        Cpublic = list(
            x = 'numericVector',
            go = nFunction(
                fun = function() {
                    y <- x
                    parallel_for(i, 1:length(x),  
                    {y[i] <- 2*x[i]; x[i] <- 0}  # By default 'x' is shared (and therefore modified).
                    )
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    Cnc1 <- Cnc$new()
    Cnc1$x <- 1:5
    Cnc1$go()
    expect_identical(Cnc1$x, as.numeric(rep(0,5)))

    nc <- nClass(
        Cpublic = list(
            x = 'numericVector',
            go = nFunction(
                fun = function() {
                    y <- x
                    parallel_for(i, 1:length(x),  
                    {y[i] <- 2*x[i]; x[i] <- 0},
                    copyVars = 'x'
                    )
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    Cnc1 <- Cnc$new()
    Cnc1$x <- 1:5
    Cnc1$go()
    expect_identical(Cnc1$x, as.numeric(1:5))
    
})


test_that("lookup precedence", {
    twice = nFunction(
        fun=function(x = 'numericScalar') {
            return(3*x)
        }, returnType = 'numericScalar'
    )
    
    nc <- nClass(
        Cpublic = list(
            twice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(2*x)
                }, returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x), {y[i] <- twice(x[i])})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc,twice)[[1]]
    obj <- nc$new()
    Cobj <- Cnc$new()
    expect_identical(obj$go(2:6), as.numeric(2*(2:6)))
    expect_identical(Cobj$go(2:6), as.numeric(2*(2:6)))
    
})
    


test_that("multiple non-nested loops", {
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(j, 1:length(x), {y[j] <- 2*x[j]})
                    parallel_for(j, 1:length(x), {y[j] <- 3*y[j]})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)
    obj <- nc$new()
    Cobj <- Cnc$new()
    expect_identical(obj$go(2:6), as.numeric(6*(2:6)))
    expect_identical(Cobj$go(2:6), as.numeric(6*(2:6)))

    twice = nFunction(
        fun=function(x = 'numericScalar') {
            return(2*x)
        }, returnType = 'numericScalar'
    )
    
    nc <- nClass(
        Cpublic = list(
            thrice = nFunction(
                fun=function(x = 'numericScalar') {
                    return(3*x)
                }, returnType = 'numericScalar'
            ),
            go = nFunction(
                fun = function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x),
                    {y[i] <- twice(x[i])})
                    parallel_for(i, 1:length(y),
                    {y[i] <- thrice(y[i])})
                    return(y)
                },
                returnType = 'numericVector'
            ),
            go2 = nFunction(
                fun = function(x = 'numericVector',z='numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x),
                    {y[i] <- twice(x[i])+z[i]+3})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    
    Cnc <- nCompile(nc, twice)[[1]]
    obj <- nc$new()
    Cobj <- Cnc$new()
    expect_identical(obj$go(2:6), as.numeric(6*(2:6)))
    expect_identical(Cobj$go(2:6), as.numeric(6*(2:6)))
    expect_identical(obj$go2(2:6, 100:104), as.numeric(2*(2:6)+100:104+3))
    expect_identical(Cobj$go2(2:6,100:104), as.numeric(2*(2:6)+100:104+3))
})
 
test_that("nested loops", {   # See issue 152.

    nc <- nClass(
        Cpublic = list(
            thrice = nFunction(
                fun=function(x = 'numericVector') {
                    y <- x
                    parallel_for(i, 1:length(x), {y[i] <- 3*x[i]})
                    return(y)
                }, returnType = 'numericVector'
            ),
            go = nFunction(
                fun = function(x = 'numericMatrix') {
                    y <- x
                    nc <- dim(x)[2]
                    nr <- dim(x)[1]
                    parallel_for(i, 1:nr, {y[i,1:nc] <- thrice(x[i,1:nc])})
                    return(y)
                },
                returnType = 'numericMatrix'
            )
        ))

    Cnc <- nCompile(nc)
    obj <- nc$new()
    Cobj <- Cnc$new()
    input <- matrix(as.numeric(1:6), nrow=2)
    expect_identical(obj$go(input), 3*input)
    expect_identical(Cobj$go(input), 3*input)
})
