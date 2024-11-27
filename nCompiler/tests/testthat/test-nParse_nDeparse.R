# Deparsing for loops needs to be fixed.
# The argument matching and preprocessing of compile time args
# needs tests.

test_exprClass <- function(Rcode,
                           verbose = FALSE) {
    if(is.character(Rcode))
        Rcode <- parse(text = Rcode, keep.source = FALSE)[[1]]
    nimCode <- nParse(Rcode)
    deparsedRcode <- deparse(Rcode)
    deparsedNimCode <- nDeparse(nimCode)
    if(verbose) {
        cat(paste("Deparsed R code:\n",
                  paste(deparsedRcode, collapse = "\n"),
                  "\n"))
        cat(paste("Deparsed nCompiler code:\n",
                  paste(deparsedNimCode, collapse = "\n"),
                  "\n"))
        cat("Printed nCompiler code:\n")
        nimCode$print()
    }
    expect_true(identical(deparsedRcode, deparsedNimCode),
                info = paste0('Deparsed R and nimble nCompiler are not identical:',
                              deparsedRcode,
                              deparsedNimCode,
                              sep="\n"))    
}

test_that("basics",
{
    ## basic calls
    test_exprClass("foo()")
    test_exprClass("foo(a)")
    test_exprClass("foo(a, b)")
    test_exprClass("foo(x = a, b)")
    test_exprClass("foo(a, y = b)")
    test_exprClass("foo(x = a, y = b)")
    test_exprClass("foo(x = 1, y = TRUE, 'hello world')")
    ## chained calls
    test_exprClass("foo()(x = 1, y = TRUE, 'hello world')")
    test_exprClass("foo(a, x = b, FALSE)(x = 1, y = TRUE, 'hello world')")
    ##"["
    test_exprClass("x[]")
    test_exprClass("x[1]")
    test_exprClass("x[1, 2]")
    test_exprClass("x[1, 2, drop = FALSE]")
    
    test_exprClass("x[i:j, 2, drop = FALSE]")
    test_exprClass("x[i:j,][ , k, drop = FALSE]")
    test_exprClass("foo(x[i:j,][ , k, drop = FALSE])")
    test_exprClass("foo(x[i:j,])[ , k, drop = FALSE]")
    ##"[["
    test_exprClass("x[[]]")
    test_exprClass("x[[a, TRUE]]")
    test_exprClass("x[[f(a, TRUE)]]")
    ## operator precedence reconstruction
    test_exprClass("a + b * c")
    test_exprClass("(a + b) * c")
    test_exprClass("(a + b) * (c)")
    test_exprClass("(a + b) * (arg1 = c)") ## `=` operator, not a named argument
    ## multiline:
    test_exprClass("{a <- foo(b); c <- foo(d)}")
    ## for, if, while, swtich
    ##    debug(test_exprClass)
    ## NOT WORKING
    cat("KNOWN PROBLEM: Deparsing for loops is not right\n")
    expect_error(test_exprClass("for(i in 1:10) {a[i] <- foo(i)}"))
})
