context("Testing nCompiler types")

test_that("numericVector(5)",
{
    ## as if `a = numericVector(5)` declared in function argument
    a <- quote(numericVector(5))
    aSym <- nCompiler:::argType2symbol(a,
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE)
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_identical(aSym$nDim, 1)
    expect_identical(aSym$isRef, FALSE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("ref(numericVector(5))",
{
    ## as if `a = ref(numericVector(5))` declared in function-argument
    a <- quote(ref(numericVector(5)))
    aSym <- nCompiler:::argType2symbol(a,
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE)
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_identical(aSym$nDim, 1)
    expect_identical(aSym$isRef, TRUE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("numericVector(5) isRef=TRUE",
{
    ## as if `a = numericVector(5)` declared in function-argument
    ## and isRef=TRUE used to indicate reference
    a <- quote(numericVector(5))
    aSym <- nCompiler:::argType2symbol(a,
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE,
                                            isRef = TRUE)
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_identical(aSym$nDim, 1)
    expect_identical(aSym$isRef, TRUE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("ref(numericVector(5) via explicitType)",
{
    ## as if `a` declared in function-argument,
    ## with argType = `ref(numericVector(5))`
    a <- NULL
    aExplicit <- quote(ref(numericVector(5)))
    
    aSym <- nCompiler:::argType2symbol(a,
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE,
                                            explicitType = aExplicit)
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_identical(aSym$nDim, 1)
    expect_identical(aSym$isRef, TRUE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("numericVector(5) via explicitType",
{
    ## as if `a` declared in function-argument,
    ## with argType = `numericVector(5)`
    a <- NULL
    aExplicit <- quote(numericVector(5))
    aSym <- nCompiler:::argType2symbol(a,
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE,
                                            explicitType = aExplicit)
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_identical(aSym$nDim, 1)
    expect_identical(aSym$isRef, FALSE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("numericVector(5) isRef = TRUE via explicitType",
{
    ## as if `a` declared in function-argument,
    ## with argType = `numericVector(5)`
    a <- NULL
    aExplicit <- quote(numericVector(5))
    aSym <- nCompiler:::argType2symbol(a,
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE,
                                            isRef = TRUE,
                                            explicitType = aExplicit)
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_identical(aSym$nDim, 1)
    expect_identical(aSym$isRef, TRUE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("ref(numericVector(5)) via explicitType with default value to ignore",
{
    ## as if `a` declared in function-argument with default and
    ## with argType = `ref(numericVector(5))`
    a <- quote(rnorm(5)) ## ignored
    aExplicit <- quote(ref(numericVector(5)))
    aSym <- nCompiler:::argType2symbol(a,
                                            name = "a",
                                            origName = "orig_a",
                                            isArg = TRUE,
                                            explicitType = aExplicit)
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_identical(aSym$nDim, 1)
    expect_identical(aSym$isRef, TRUE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("infer type from evaluating default",
{
    ## infer type of `a` from default
    a <- quote(rnorm(5))
    aSym <- nCompiler:::argType2symbol(a,
                                       name = "a",
                                       origName = "orig_a",
                                       isArg = TRUE
                                       )
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_equal(aSym$nDim, 1)
    expect_identical(aSym$isRef, FALSE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("infer type from evaluating default, with scoping needed",
{
    ## infer type of `a` from default with scoping needed to
    ## evaluate the default expression.
    fun1 <- function() {
        fun2 <- function(n) rnorm(n)
        a <- quote(fun2(5))
        aSym <- nCompiler:::argType2symbol(a,
                                                name = "a",
                                                origName = "orig_a",
                                                isArg = TRUE
                                                )
        aSym
    }
    aSym <- fun1()
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_equal(aSym$nDim, 1)
    expect_identical(aSym$isRef, FALSE)
    expect_identical(aSym$isArg, TRUE)
})

test_that("trap error from duplicate setting of isRef",
{
    ## Error-trapping:
    ## Duplicate setting of ref
    a <- quote(ref(numericVector(5)))
    expect_error(
        nCompiler:::argType2symbol(a,
                                        name = "a",
                                        origName = "orig_a",
                                        isArg = TRUE,
                                        isRef = TRUE)
    )

    ## Error-trapping
    ## Type incompatible with default
    a <- quote(matrix(1:4, nrow = 2, ncol = 2))
    aExplicit <- quote(numericVector())
    expect_error(
        nCompiler:::argType2symbol(a,
                                        name = "a",
                                        origName = "orig_a",
                                        isArg = TRUE,
                                        explicitType = aExplicit)
    )
})

test_that("nimMatrix(type = \"integer\")",
{
    ## Type constructed from object
    cat('\nTo do: test of using nimType object to define type\n')
    
    ## some other basic types
    b <- quote(nimMatrix(type = "integer",
                         nrow = 3,
                         ncol = 5))
    bSym <- nCompiler:::argType2symbol(b,
                                            name = "b",
                                            origName = "orig_b",
                                            isArg = TRUE)
    expect_identical(bSym$name, "b")
    expect_identical(bSym$type, "integer")
    expect_equal(bSym$nDim, 2)
    expect_identical(bSym$isRef, FALSE)
    expect_identical(bSym$isArg, TRUE)
})

test_that("list arguments handled correctly",
{
    ## working from a list
    a <- quote(numericMatrix())
    b <- NULL
    bExplicit = quote(nimMatrix(type = "integer"))
    aRef <- TRUE
    bRef <- FALSE
    symTab <- nCompiler:::argTypeList2symbolTable(
        argTypeList = list(a = a,
                           b = b),
        origNames = c("orig_a", "orig_b"),
        isRef = list(a = aRef, b = bRef),
        explicitTypeList = list(b = bExplicit) 
    )
    expect_identical(symTab$getSymbolNames(),
                     c("a","b"))
    aSym <- symTab$getSymbol("a")
    expect_identical(aSym$name, "a")
    expect_identical(aSym$type, "double")
    expect_equal(aSym$nDim, 2)
    expect_identical(aSym$isRef, TRUE)
    expect_identical(aSym$isArg, FALSE)

    bSym <- symTab$getSymbol("b")
    expect_identical(bSym$name, "b")
    expect_identical(bSym$type, "integer")
    expect_equal(bSym$nDim, 2)
    expect_identical(bSym$isRef, FALSE)
    expect_identical(bSym$isArg, FALSE)

    ## void() (return type default)
    vSym <- nCompiler:::argType2symbol(quote(void()))
    expect_identical(vSym$type, "void")
    expect_identical(vSym$nDim, 0)
})

test_that("symbolNC works",
          {
            nc1 <- nClass(
              Cpublic = list(a = 'numericScalar')
            )
            sym_nc1 <- argType2symbol('nc1', 'nc1obj')
            expect_equal(sym_nc1$genCppVar()$generate(),
                         "std::shared_ptr<nc1> nc1obj")
          })

cat("\nSee test-types.R for notes on remaining issues to test.\n")
## Need to make refs work with slices and blocks
## Need to enforce that with isRef=TRUE, no form of default value is valid,
## even in a nimble type declaration.
## Need to add expect_error tests.
## Need to test case of providing a nimType object directly.


