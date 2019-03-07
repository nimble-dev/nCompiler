## not working

context("Compiler stage: setInputOutputTypes")

test_that("double()",
{
    nf <- nFunction(
        function(x = double()) {
            returnType(double())
            y <- x + 1
            return(y)
        }
    )
    test <- compileNimbleFunction(
        nf,
        control = list(
            endStage = 'setInputOutputTypes'
        )
    )
})
