# works

test_that("simple pass by value",
{
    ## hello world
    foo <- nFunction(
        fun = function(a = numericScalar()) {
            return(a + 1)
            returnType(numericScalar())
        }
    )
    expect_equal(foo(2),
                 3)
})

test_that("simple pass by reference",
{
    ## pass by reference
    foo <- nFunction(
        fun = function(a = numericScalar()) {
            a <- a + 1
            a
        },
        refArgs = 'a',
        returnType = quote(numericScalar())
    )
    a <- 10
    expect_equal(foo(a),
                 11)
    expect_equal(a,
                 11)
    expect_equal(foo@internals$returnSym$type,
                 "double")
})

test_that("combination of pass by value and reference",
{
    ## combination of reference and non-reference
    foo <- nFunction(
        fun = function(a,
                       b = ref(numericVector())) {
            b <- b + a
            return(a)
            returnType(numericScalar())
        },
        argTypes = list(a = "numericScalar()")
    )
    b <- 1:3
    expect_equal(foo(10, b),
                 10)
    expect_equal(b,
                 10+1:3)
})

test_that("providing fun as a separate function",
{
    ## separately written function
    ## also checks conversion from 'numericScalar' to 'numericScalar()'
    rawfoo <- function(a = 5,
                       b) {
        b <- b + a
        return(a) ## explicit return still necessary
    }
    
    foo <- nFunction(
        fun = rawfoo,
        argTypes = list(a = "numericScalar()",
                        b = "ref(integerVector())"),
        returnType = "numericVector()"
    )
    b <- 1:3
    expect_equal(foo(10, b),
                 10)
    expect_equal(b,
                 10+1:3)
    expect_equal(foo@internals$returnSym$type,
                 "double")
})
