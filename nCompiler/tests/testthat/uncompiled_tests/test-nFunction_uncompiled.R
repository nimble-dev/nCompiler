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


test_that("updateArgPassing = FALSE keeps refArgs out of R, for an nFunction and an nClass method",
{
    ## refArgs apply only in C++, so R calls can pass any expression by value
    foo <- nFunction(
        fun = function(a = numericVector()) {
            a[1] <- 0
            return(sum(a))
        },
        refArgs = 'a',
        returnType = "numericScalar()",
        control = list(updateArgPassing = FALSE)
    )
    a <- 1:3
    expect_equal(foo(a), 5)
    expect_equal(a, 1:3)
    expect_equal(foo(c(10, 20)), 20)
    nc <- nClass(Cpublic = list(foo = foo))
    obj <- nc$new()
    expect_equal(obj$foo(a), 5)
    expect_equal(a, 1:3)
    expect_equal(obj$foo(c(10, 20)), 20)
})
