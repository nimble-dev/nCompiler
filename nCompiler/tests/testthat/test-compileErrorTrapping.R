context("Testing NFtry error handling")
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
