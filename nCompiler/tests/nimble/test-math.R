## Copied from nimble's test-math
## Perhaps in the future we can source by chunks of lines

### INSTRUCTIONS:
## got to mathTestLists.R:
## enter each test as a list, with an informative name, NIMBLE expression to evaluate, vector of input dimensions, value of output dimension, and (if NIMBLE expression cannot be directly evaluated in R) the equivalent pure R expression whose result should match the NIMBLE result

library(nimble)
#library(nCompiler)

divert_to_nCompiler <- function(fun, replacements) {
  diversion_env <- list2env(replacements, parent = environment(fun))
  environment(fun) <- diversion_env
  fun
}

source(system.file(file.path('tests', 'testthat', 'test_utils.R'), package = 'nimble'))

## Changes start
test_math_internal <-
  divert_to_nCompiler(test_math_internal, list(compileNimble = `:::`("nCompiler", "compileNimble")))
## Changes stop

#debug(nCompiler:::compileNimble)
#undebug(nCompile)

RwarnLevel <- options('warn')$warn
options(warn = 1)
nimbleVerboseSetting <- nimbleOptions('verbose')
nimbleOptions(verbose = FALSE)

source(system.file(file.path('tests', 'testthat', 'mathTestLists.R'), package = 'nimble'))



set.seed(0)
ans1 <- sapply(testsVaried, test_math, 'math')    ## 12
ans2 <- sapply(testsBasicMath, test_math, 'math') ## 70

if(.Platform$OS.type == 'windows') {
    message("Since you are running on Windows, tests stopped prior to reaching max DLL limit.  Please use test-math2 to continue")
    stop()
}
ans3 <- sapply(testsMoreMath, test_math, 'math')  ## 41
ans4 <- sapply(testsReduction, test_math, 'math') ## 13
ans5 <- sapply(testsComparison, test_math, 'math')## 12
ans6 <- sapply(testsMatrix, test_math, 'math')    ## 19
ans7 <- sapply(testsRecyclingRuleMatrices, test_math, 'math')

options(warn = RwarnLevel)
nimbleOptions(verbose = nimbleVerboseSetting)
