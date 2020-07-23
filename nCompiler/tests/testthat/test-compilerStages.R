
## This version will test each successive compiler stage
context("Compiler stages")

cases <- list()
cases[["D2+literal_double"]] <- list(
  fun = function(x = numericMatrix()) {y <- x + 1.1}
)
cases[["D1+literal_double"]] <- list(
  fun = function(x = numericVector()) {y <- x + 1.1; return(y); returnType(numericVector())}
)
cases[["min(D1)"]] <- list(
  fun = function(x = numericVector()) y <- min(x)
)
cases[["mean(D1)"]] <- list(
  fun = function(x = numericVector()) y <- mean(x)
)

nCompiler:::writeTypesText_manyCases(cases["mean(D1)"],
                                          endStage = 'makeRcppPacket')
# debugonce(nCompiler:::eigenizeEnv$ReductionEither)
test <- nCompile(nFunction(fun = cases[["mean(D1)"]]$fun))

nCompiler:::writeTypesText_manyCases(cases)
# nCompiler:::writeTypesText_manyCases(cases,
#                                           # endStage = 'setToEigen',
#                                           showArgs = list(showImpl = TRUE))
# nCompiler:::writeTypesText_manyCases(cases,
#                                           endStage = 'labelForEigen')
#debug(nCompiler:::compile_eigenize)
nCompiler:::writeTypesText_manyCases(cases,
                                          endStage = 'doImplementation')
#debugonce(generateTypesText_oneCase)
#debugonce(compileNimbleFunction)
nCompiler:::generateTypesText_oneCase(cases[[1]],
                          endStage = 'makeRcppPacket')

# debugonce(compileNimbleFunction)
test <- nCompile(nFunction(fun = cases[[1]]$fun))

# compareAllFilesInDir("correct_labelAbstractTypes", "trial_labelAbstractTypes")

test_that("double() + literal",
          {
            nf <- nFunction(
              function(x = double()) {
                y <- x + 1
              }
            )

            # debug(processNFstages)
            test <- nCompile(
              nf#,
              # control = list(
              #   endStage = 'labelAbstractTypes'
              # )
            )
          })
