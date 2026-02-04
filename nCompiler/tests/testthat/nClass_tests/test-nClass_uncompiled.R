# works

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

  my_nc1 <- nc1$new()

  expect_true(inherits(my_nc1, "nClass"))
  expect_true(inherits(my_nc1, "R6"))
  expect_true(inherits(my_nc1$Cfoo, 'nFunction'))
  expect_equal(my_nc1$Cfoo(2), 3)
  expect_true(inherits(nc1$.nCompiler$symbolTable, "symbolTableClass"))
  expect_equal(nc1$.nCompiler$symbolTable$getSymbol("Cv")$nDim, 0)
  expect_true(isNC(my_nc1))
  expect_true(isNCgenerator(nc1))
  expect_error(inherits(NCinternals(my_nc1), "NC_InternalsClass"))
  expect_true(inherits(NCinternals(nc1), "NC_InternalsClass"))
  expect_equal(NCinternals(nc1)$methodNames, 'Cfoo')

  ## Check that `$clone` works.
  values <- rnorm(3)
  my_nc1$Rv <- values
  expect_silent(my_nc1_clone <- my_nc1$clone())
  expect_identical(my_nc1$Rv, my_nc1_clone$Rv)
  my_nc1$Rv[2] <- 3
  expect_identical(values[2], my_nc1_clone$Rv[2])
}
)
