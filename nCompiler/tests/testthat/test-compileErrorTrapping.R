message("test-compileErrorTrapping works but has only the narrowest use case.")
test_that("nCompiler's error handler works",
{
    doSomething <- function(x) {
        x <- a + 1
        x
    }
    
    go <- function() {
        nCompiler:::NFtry({
            doSomething(5)
        })
    }
    
    expect_error(go())
}
)
