
test_that("error/warn/message operators", {
    # On GitHub Actions, logger's default layout is layout_gha, which omits the level name for DEBUG and TRACE.
    old_layout <- logger::log_layout()
    logger::log_layout(logger::layout_simple)
    on.exit(logger::log_layout(eval(old_layout, getNamespace("logger"))), add = TRUE)

    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            stop('problem: ', y, ' ', x[2])
        })
    cnf <- nCompile(nf)
    expect_error(nf(1:2), "problem: 3 2", fixed = TRUE)
    expect_error(cnf(1:2), "problem: 3 2", fixed = TRUE)

    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            warning('problem: ', y, ' ', x[2], ' 50%')
        })
    cnf <- nCompile(nf)
    expect_warning(nf(1:2), "problem: 3 2 50%", fixed = TRUE)
    expect_warning(cnf(1:2), "problem: 3 2 50%", fixed = TRUE)

    # Non-scalar arguments are streamed in C++, so compiled output format differs from R.
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            stop('problem: ', y, ' ', x+y)
        })
    cnf <- nCompile(nf)
    expect_error(nf(1:2), "problem: 3 4")
    expect_error(cnf(1:2), "problem: 3 4")

    # message() becomes nMessage(), which uses the `logger` system with default level INFO.
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            message('problem: ', y, ' ', x[2])
        })
    cnf <- nCompile(nf)
    expect_output(nf(1:2), "INFO.*problem: 3 2")
    expect_output(cnf(1:2), "INFO.*problem: 3 2")

    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            nMessage('problem: ', y, ' ', x[2], level = ERROR)
        })
    cnf <- nCompile(nf)
    expect_output(nf(1:2), "ERROR.*problem: 3 2")
    expect_output(cnf(1:2), "ERROR.*problem: 3 2")

    old_threshold <- log_threshold()
    log_threshold(FATAL)
    expect_silent(nf(1:2))
    expect_silent(cnf(1:2))

    log_threshold(TRACE)
    expect_output(nf(1:2), "problem: 3 2")
    expect_output(cnf(1:2), "problem: 3 2")

    nf <- nFunction(
        fun = function() {
            nMessage('debug message', level = DEBUG)
        })
    cnf <- nCompile(nf)
    expect_output(nf(), "DEBUG.*debug message")
    expect_output(cnf(), "DEBUG.*debug message")
    log_threshold(INFO)
    expect_silent(nf())
    expect_silent(cnf())
    log_threshold(old_threshold)
})

test_that("cat, nimCat, print and nimPrint operators", {
    # cat (nCat) is like R's cat: arguments separated by " ".
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            cat('a', y, 'b', x[1])
        })
    cnf <- nCompile(nf)
    expect_identical(capture.output(nf(1:2)), "a 3 b 1")
    expect_identical(capture.output(cnf(1:2)), "a 3 b 1")

    # nimCat is like nimble's nimCat: arguments not separated, no newline.
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            nimCat('a', y, 'b', x[1])
            nimCat('c')
        })
    cnf <- nCompile(nf)
    expect_identical(capture.output(nf(1:2)), "a3b1c")
    expect_identical(capture.output(cnf(1:2)), "a3b1c")

    # print (nPrint) and nimPrint are like nimble's nimPrint: nimCat followed by a newline.
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- 3
            print('a', y, 'b', x[1])
            nimPrint('c')
        })
    cnf <- nCompile(nf)
    expect_identical(capture.output(nf(1:2)), c("a3b1", "c"))
    expect_identical(capture.output(cnf(1:2)), c("a3b1", "c"))

    # Non-scalar arguments compile; C++ output format differs from R.
    nf <- nFunction(
        fun = function(x = double(1)) {
            cat('v:', x + 1)
        })
    cnf <- nCompile(nf)
    expect_output(nf(1:2), "v: 2 3")
    expect_output(cnf(1:2), "v: 2")
})

test_that("cppStacktrace reports the DSL line of an error", {
    old <- get_nOption("compilerOptions")
    on.exit(set_nOption("compilerOptions", old))
    opt <- old
    opt$cppStacktrace <- TRUE
    set_nOption("compilerOptions", opt)
    nf <- nFunction(
        fun = function(x = double(1)) {
            y <- x[1] + 1
            if(y > 1) stop("y too big: ", y)
            return(y)
        }, returnType = 'double')
    cnf <- nCompile(nf)
    expect_error(cnf(c(1, 2)), 'at "nStop("y too big: ", y)"', fixed = TRUE)
    expect_error(cnf(c(1, 2)), "y too big: 2", fixed = TRUE)
    expect_equal(cnf(c(-5, 2)), -4)
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
                    nMessage('test error message', level = INFO)
                progress_update()
            }
        })
    cfoo <- nCompile(foo)
    expect_message(cfoo(100000), "test error message")

    ## Uncompiled execution
    ## cli shows a progress bar only after `cli.progress_show_after` seconds (default 2),
    ## and redraws it only on its timer ticks, so the loop must take some time.
    old_opts <- options(cli.progress_show_after = 0)
    on.exit(options(old_opts))
    foo = nFunction(
        fun = function(its = double(0)) {
            progress_bar("Sampling", total = its)
            for(i in 1:its) {
                x <- 0
                for(j in 1:10000)
                    x <- x+1
                if(i == 5000)
                    nMessage('test error message', level = INFO)
                progress_update()
            }
        })
    ## For some reason testthat is not seeing "test error message".
    expect_message(foo(10000), "Sampling")
})
