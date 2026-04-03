## Note that it is hard to test that the number of threads is actually being
## modified as on Linux TBB is not scaling well so testing timing won't be
## useful. Therefore these tests mostly check that code compiles and runs.
## But the design of the parallelized code is such that we could check
## run time in the future.

test_that("basic usage of setting number of threads", {

    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(n = 'numericScalar', m = 'numericScalar') {
                    y <- numeric(length=n)
                    parallel_for(i, 1:n, {y[i] <- mean(rnorm(m))})
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)

    Cnc1 <- Cnc$new()
    expect_silent(out <- Cnc1$go(100, 100))
    
    val <- set_nOption('nThreads', 2)
    expect_silent(out <- Cnc1$go(100, 100))
    
    val <- set_nOption('nThreads', 100000)
    expect_silent(out <- Cnc1$go(100, 100))

    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(n = 'numericScalar', m = 'numericScalar') {
                    y <- numeric(length=n)
                    parallel_for(i, 1:n, {y[i] <- mean(rnorm(m))}, nThreads = 2)
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)

    Cnc1 <- Cnc$new()
    expect_silent(out <- Cnc1$go(100, 100))

    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(n = 'numericScalar', m = 'numericScalar', nThreads = 'numericScalar') {
                    y <- numeric(length=n)
                    parallel_for(i, 1:n, {y[i] <- mean(rnorm(m))}, nThreads = nThreads)
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)

    Cnc1 <- Cnc$new()
    expect_silent(out <- Cnc1$go(100, 100, 2))
    expect_silent(out <- Cnc1$go(100, 100, 0))
    val <- set_nOption('nThreads', 4)
    expect_silent(out <- Cnc1$go(100, 100, 2))

    ## Should execute in R but will ignore threads argument.
    nc1 <- nc$new()
    expect_silent(out <- nc1$go(100, 100, 2))

    ## Multiple loops with different numbers of threads.
    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(n = 'numericScalar', m = 'numericScalar') { 
                    y <- numeric(length=n)
                    parallel_for(i, 1:n,
                    {y[i] <- mean(rnorm(m))}, nThreads=2)
                    
                    parallel_for(i, 1:n,
                    {y[i] <- mean(rnorm(m))}, nThreads=8)
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    
    Cnc <- nCompile(nc)
    
    Cnc1 <- Cnc$new()
    expect_silent(out <- Cnc1$go(100, 100))

    nc <- nClass(
        Cpublic = list(
            go = nFunction(
                fun = function(n = 'numericScalar', m = 'numericScalar', nThreads = 'numericScalar') {
                    y <- numeric(length=n)
                    parallel_for(i, 1:n, {y[i] <- mean(rnorm(m))}, nThreads = nThreads+2)
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    Cnc <- nCompile(nc)

    Cnc1 <- Cnc$new()
    expect_silent(out <- Cnc1$go(100, 100, 2))

    ## Nested loops
    nc <- nClass(
        Cpublic = list(
            myfun = nFunction(
                fun = function(m = 'numericScalar', k = 'numericScalar') {
                    y <- numeric(length=m)
                    parallel_for(i, 1:m, {y[i] = mean(rnorm(k))}, nThreads = 4)
                    return(y)
                }, returnType = 'numericVector'
            ),                                 
            go = nFunction(
                fun = function(n = 'numericScalar', m = 'numericScalar', k = 'numericScalar') { 
                    y <- numeric(length=n)
                    parallel_for(i, 1:n, {
                        y[i] <- mean(myfun(m, k))
                    }, nThreads=2)
                    return(y)
                },
                returnType = 'numericVector'
            )
        )
    )
    
    Cnc <- nCompile(nc)
    
    Cnc1 <- Cnc$new()
    ## expect_silent(out <- Cnc1$go(100, 100, 10))
    ## Error: C stack usage  14983114845556 is too close to the limit
    ## Error: no more error handlers available (recursive errors?); invoking 'abort' restart
    ## *** longjmp causes uninitialized stack frame ***: terminated

})
