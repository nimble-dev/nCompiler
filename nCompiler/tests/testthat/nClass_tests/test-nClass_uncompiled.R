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
        returnType = 'numericScalar'),
      Cbreak = nFunction(
        fun = function(x) {
          return(Rfoo(x)) # should give an error
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar')
    )
  )

  my.nc1 <- nc1$new()

  expect_true(inherits(my.nc1, "nClass"))
  expect_true(inherits(my.nc1, "R6"))
  expect_true(inherits(my.nc1$private$Cpublic_obj$Cfoo, 'nFunction'))
  expect_true(inherits(my.nc1$Cfoo, 'function'))
  expect_equal(my.nc1$Cfoo(2), 3)
  expect_true(inherits(nc1$.nCompiler$symbolTable, "symbolTableClass"))
  expect_equal(nc1$.nCompiler$symbolTable$getSymbol("Cv")$nDim, 0)
  expect_true(isNC(my.nc1))
  expect_true(isNCgenerator(nc1))
  expect_error(inherits(NCinternals(my.nc1), "NC_InternalsClass"))
  expect_true(inherits(NCinternals(nc1), "NC_InternalsClass"))
  expect_equal(NCinternals(nc1)$methodNames, 'Cfoo')
}
)

test_that("nClass inheritance basics", {
  nc1 <- nClass(
    classname = "surprise",
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

  nc2 <- nClass(
    inherit = nc1,
    Rpublic = list(
      Rfoo2 = function(x) Rfoo(x)
    ),
    Cpublic = list(
      Cv2 = 'numericVector',
      Cfoo2 = nFunction(
        fun = function(x) {
          return(Cfoo(x))
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar')
    )
  )

  my.nc2 <- nc2$new()
  expect_equal(my.nc2$Rfoo(1), 2)
  expect_equal(my.nc2$Rfoo2(1), 2)
  expect_equal(my.nc2$Cfoo(1), 2)
  expect_equal(my.nc2$Cfoo2(1), 2)
  my.nc2$Cv <- 5
  expect_equal(my.nc2$Cv, 5)
  my.nc2$Cv2 <- 6
  expect_equal(my.nc2$Cv2, 6)

  expect_true(inherits(my.nc2, "surprise"))
  expect_true(inherits(nc1$new(), "surprise"))
})
