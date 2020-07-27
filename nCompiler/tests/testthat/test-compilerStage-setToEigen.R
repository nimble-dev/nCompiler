
context("Compiler stage: setInputOutputTypes")

cases <- list()
cases[["D0+literal_logical"]] <- list(
  fun = function(x = double()) y <- x + TRUE
)
cases[["D0+literal_integer"]] <- list(
    fun = function(x = double()) y <- x + 1L
  )
cases[["D0+literal_double"]] <- list(
  fun = function(x = double()) y <- x + 1.1
)
cases[["D1+literal_logical"]] <- list(
  fun = function(x = numericVector()) y <- x + TRUE
)
cases[["D1+literal_integer"]] <- list(
  fun = function(x = numericVector()) y <- x + 1L
)
cases[["D1+literal_double"]] <- list(
  fun = function(x = numericVector()) y <- x + 1.1
)
cases[["literal_logical+D2"]] <- list(
  fun = function(x = numericMatrix()) y <- TRUE + x
)
cases[["literal_integer+D2"]] <- list(
  fun = function(x = numericMatrix()) y <- 1L + x
)
cases[["literal_double+D2"]] <- list(
  fun = function(x = numericMatrix()) y <- 1.1 + x
)
cases[["I0+literal_logical"]] <- list(
  fun = function(x = integer()) y <- x + TRUE
)
cases[["I0+literal_integer"]] <- list(
  fun = function(x = integer()) y <- x + 1L
)
cases[["I0+literal_double"]] <- list(
  fun = function(x = integer()) y <- x + 1.1
)
cases[["I1+literal_logical"]] <- list(
  fun = function(x = integerVector()) y <- x + TRUE
)
cases[["I1+literal_integer"]] <- list(
  fun = function(x = integerVector()) y <- x + 1L
)
cases[["I1+literal_double"]] <- list(
  fun = function(x = integerVector()) y <- x + 1.1
)
cases[["literal_logical+I2"]] <- list(
  fun = function(x = integerMatrix()) y <- TRUE + x
)
cases[["literal_integer+I2"]] <- list(
  fun = function(x = integerMatrix()) y <- 1L + x
)
cases[["literal_double+I2"]] <- list(
  fun = function(x = integerMatrix()) y <- 1.1 + x
)
## We could automate construction of tests
## to accomplish many permutations of argument types

cases[["literal_double>I2"]] <- list(
  fun = function(x = integerMatrix()) y <- 1.1 > x
)

cases[["D1>I1"]] <- list(
  fun = function(x = numericVector(), y = integerVector) z <- x > y
)

cases[["mean(D1)"]] <- list(
  fun = function(x = numericVector()) y <- mean(x)
)
cases[["prod(D1)"]] <- list(
  fun = function(x = numericVector()) y <- mean(x)
)
cases[["squaredNorm(D1)"]] <- list(
  fun = function(x = numericVector()) y <- mean(x)
)
cases[["mean(I1)"]] <- list(
  fun = function(x = integerVector()) y <- mean(x)
)

writeLines(nCompiler:::generateTypesText_oneCase(cases[["D0+literal_integer"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["D0+literal_double"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["D0+literal_logical"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["D1+literal_integer"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["D1+literal_double"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["D1+literal_logical"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["literal_integer+D2"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["literal_double+D2"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["literal_logical+D2"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["I0+literal_double"]]))
writeLines(nCompiler:::generateTypesText_oneCase(cases[["mean(D1)"]]))

nCompiler:::writeTypesText_manyCases(cases,
                         # endStage = 'setToEigen',
                         showArgs = list(showImpl = TRUE))
nCompiler:::writeTypesText_manyCases(cases, "correct_labelAbstractTypes")
nCompiler:::writeTypesText_manyCases(cases, "trial_labelAbstractTypes")

nCompiler:::compareAllFilesInDir("correct_labelAbstractTypes", 
                                 "trial_labelAbstractTypes")

test_that("double() + literal",
          {
            nf <- nFunction(
              function(x = "numericScalar") {
                y <- x + 1
              }
            )
            test <- nCompile(
              nf
              # control = list(
              #   endStage = 'labelAbstractTypes'
              # )
            )
          })
