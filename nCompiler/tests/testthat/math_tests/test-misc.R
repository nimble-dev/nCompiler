test_that("Numerical issues", {
    myfun <- nFunction(
        fun = function() {
            x <- 1e200
            return(x)
        }, returnType = 'numericScalar')
    
    expect_silent(cmyfun <- nCompile(myfun))
    expect_identical(cmyfun(), 1e200)
})
