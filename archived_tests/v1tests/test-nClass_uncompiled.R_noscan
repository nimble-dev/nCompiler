context("Testing nClass basics")

test_that("nClass basics",
{
    nc1 <- nClass(
        Rpublic = list(
            Rv = NULL,
            Rfoo = function(x) x+1
        ),
        Cpublic = list(
            Cv = 'numericScalar',
            Cfoo = nFunction(
                fun = function(x) {
                    return(x+1)
                },
                argTypes = list(x = 'numericScalar'),
                returnType = 'numericScalar')
            )
    )

    my.nc1 <- nc1$new()
    my.nc1$Cfoo(3)
    expect_true(inherits(my.nc1, "nClass"))
    expect_true(inherits(my.nc1, "R6"))
    expect_true(inherits(my.nc1$Cfoo, 'nFunction'))
    expect_equal(my.nc1$Cfoo(2), 3)
    expect_true(inherits(nc1$.nCompiler$symbolTable, "symbolTableClass"))
    expect_equal(nc1$.nCompiler$symbolTable$getSymbol("Cv")$nDim, 0)
    expect_true(isNC(my.nc1))
    expect_true(isNCgenerator(nc1))
    expect_true(inherits(nCompiler:::NCinternals(my.nc1), "NC_InternalsClass"))
    expect_true(inherits(nCompiler:::NCinternals(nc1), "NC_InternalsClass"))
    expect_equal(nCompiler:::NCinternals(nc1)$methodNames, 'Cfoo')
}
)
