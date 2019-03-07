context("Testing nFunction compilation")

test_that("addScalars double",
{
    addScalars <- nFunction(
        fun = function(x = double(0),
                       y = double(0)) {
            returnType(double(0))
            ans <- x + y
            return(ans)
        }
    )
    test <- nCompile_nFunction(addScalars)
    expect_equal(test(2, 3), 5)
})

test_that("addVectors double", 
{
    addVectors <- nFunction(
        fun = function(x = double(1),
                       y = double(1)) {
            returnType(double(1))
            ans <- x + 1.1 ## 1 is mistaken for integer
            return(ans)
        }
    )
    test <- nCompile_nFunction(addVectors)
    set.seed(1)
    x <- rnorm(3)
    y <- rnorm(3)
    expect_equal(test(x, y), array(x + 1.1, dim = length(x)))
})

test_that("addArrays double",
{
    addArrays <- nFunction(
        fun = function(x = double(2),
                       y = double(2)) {
            returnType(double(2))
            ans <- x + 1.1 ## 1 is mistaken for integer
            return(ans)
        }
    )
    test <- nCompile_nFunction(addArrays)
    x <- matrix(as.numeric(1:4), nrow = 2)
    y <- matrix(as.numeric(2:5), nrow = 2)
    expect_equal(test(x, y), x + 1.1)
})

test_that("add3D double",
{
    library(nCompiler)
    addArrays <- nFunction(
        fun = function(x = double(3),
                       y = double(3)) {
            returnType(double(3))
            ans <- x + 1.1 ## 1 is mistaken for integer
            return(ans)
        }
    )
    test <- nCompile_nFunction(addArrays)
    x <- array(as.numeric(1:8), dim = c(2,2,2))
    y <- array(as.numeric(2:9), dim = c(2,2,2))    
    expect_equal(test(x, y), x + 1.1)
    cat('Add tests of argument casting\n')
})
