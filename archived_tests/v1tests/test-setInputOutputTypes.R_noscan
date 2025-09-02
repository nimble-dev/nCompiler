context("Compiler stage: setInputOutputTypes")

test_that("numericScalar()",
{
    nf <- nFunction(
        function(x = "numericScalar()") {
            returnType("numericScalar()")
            y <- x + 1
            return(y)
        }
    )
    test <- nCompile_nFunction(
        nf,
        control = list(
            endStage = 'setInputOutputTypes'
        )
    )
})
