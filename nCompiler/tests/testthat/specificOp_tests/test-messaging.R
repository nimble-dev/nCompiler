library(nCompiler);library(testthat)

test_that("various error/warn/message operators", {
    
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            stop('problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_error(cnf(1:2), "problem: 3 4")

    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            warning('problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_warning(cnf(1:2), "problem: 3 4")


    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            nCat('problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_output(cnf(1:2), "problem: 3 4")

    ## logger output defaults to stderr.
    ## When we (presumably) do this onLoad, this won't be needed.
    log_appender(appender_stdout)  

    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            nMessage(ERROR, 'problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_output(cnf(1:2), "problem: 3 4")

    logger::log_threshold(FATAL)
    expect_silent(cnf(1:2))

    logger::log_threshold(TRACE)
    expect_output(cnf(1:2), "problem: 3 4")
}
