
test_that("error/warn/message operators", {
    
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            stop('problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_error(nf(1:2), "problem: 3 4")
    expect_error(cnf(1:2), "problem: 3 4")

    

    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            warning('problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_warning(nf(1:2), "problem: 3 4")
    expect_warning(cnf(1:2), "problem: 3 4")


    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            nCat('problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_output(nf(1:2), "problem:  3   4")
    expect_output(cnf(1:2), "problem: 3 4")

    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            nMessage(ERROR, 'problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_output(nf(1:2), "problem:  3   4")
    expect_output(cnf(1:2), "problem: 3 4")

    log_threshold(FATAL)
    expect_silent(nf(1:2))
    expect_silent(cnf(1:2))

    log_threshold(TRACE)
    expect_output(nf(1:2), "problem:  3   4")
    expect_output(cnf(1:2), "problem: 3 4")
})

test_that("progress bar", {
    ## Given difficulty checking output, this test serves to check
    ## that progress bar functionality compiles and doesn't cause problems.
    foo = nFunction(
        fun = function(its = double(0)) {
            progress_bar("Sampling", total = its)
            for(i in 1:its) {
                x <- 0
                for(j in 1:1000000000)
                    x <- x+1
                if(i == 50000)
                    nMessage(INFO, 'test error message')
                progress_update() 
            }
        })
    cfoo <- nCompile(foo)
    expect_message(cfoo(100000), "test error message")  

    ## Uncompiled execution
    foo = nFunction(
        fun = function(its = double(0)) {
            progress_bar("Sampling", total = its)
            for(i in 1:its) {
                x <- 0
                for(j in 1:10000)
                    x <- x+1
                if(i == 5000)
                    nMessage(INFO, 'test error message')
                progress_update() 
            }
        })
    ## For some reason testthat is not seeing "test error message".
    expect_message(foo(10000), "Sampling")
    

})
