context("Compiler stage: simpleTransformations")

test_that("replacement",
{
    nf <- nFunction(
        function(x = double()) {
            returnType(double())
            y <- expit(x)
            return(y)
        }
    )
    test <- nCompile_nFunction(
        nf,
        control = list(
            endStage = 'simpleTransformations'
        )
    )
})
