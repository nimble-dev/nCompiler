context("Testing Automatic Differentiation")

test_that("compileNimbleClass works (with AD for a scalar)", {
  library(nCompiler)
  nc1 <- nClass(
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cfoo = nFunction(
        fun = function(x) {
          return(1.5*x+1)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar')
    ),
    enableDerivs = 'Cfoo'
  )
  set_nOption('automaticDerivatives', TRUE)
  ans <- try(nCompiler:::nCompile_nClass(nc1, interface = "generic"))
  expect_true(is.function(ans)) ## compilation succeeded
  obj <- ans()
  expect_true(nCompiler:::is.loadedObjectEnv(obj))
  expect_equal(method(obj, "Cfoo")(1.32), 1.5*1.32 + 1)
  derivs <- method(obj, "Cfoo_derivs_")(1.32, c(0, 1))
  expect_true(nCompiler:::is.loadedObjectEnv(derivs))
  expect_equal(value(derivs, "gradient"), array(1.5, dim = c(1, 1)))
})

test_that("compileNimbleClass works (with AD for a vector)", {
  library(nCompiler)
  nc1 <- nClass(
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cfoo = nFunction(
        fun = function(x) {
          ans <- 1.5 *x
          return(ans)
        },
        argTypes = list(x = 'numericVector(length = 2)'),
        returnType = 'numericVector(length = 2)')
    ),
    enableDerivs = 'Cfoo'
  )
  set_nOption('automaticDerivatives', TRUE)
  ans <- try(nCompiler:::nCompile_nClass(nc1, interface = "generic"))
  expect_true(is.function(ans)) ## compilation succeeded
  obj <- ans()
  expect_true(nCompiler:::is.loadedObjectEnv(obj))
  expect_equal(method(obj, "Cfoo")(c(1.48, 1.52)), array(1.5*c(1.48, 1.52), dim = 2))
  derivs <- method(obj, "Cfoo_derivs_")(c(1.48, 1.52), c(0, 1))
  expect_true(nCompiler:::is.loadedObjectEnv(derivs))
  expect_equal(value(derivs, "gradient"), diag(c(1.5, 1.5)))
})

test_that("AD with indexing works (with AD for a matrix)", {
  library(nCompiler)
  nc1 <- nClass(
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cfoo = nFunction(
        fun = function(x) {
          ans <- 1.5 * exp(x[2:5])^2
          return(ans)
        },
        argTypes = list(x = 'numericVector(length = 7)'),
        returnType = 'numericVector(length = 4)')
    ),
    enableDerivs = 'Cfoo'
  )
  set_nOption('automaticDerivatives', TRUE)
  ans <- try(nCompiler:::nCompile_nClass(nc1, interface = "generic"))
  expect_true(is.function(ans)) ## compilation succeeded
  obj <- ans()
  expect_true(nCompiler:::is.loadedObjectEnv(obj))
  input <- c(0.9861515, 0.3756302, 1.0109309, 0.5898856, 0.3941390, 1.1837561, -2.2858289)
  expect_equal(method(obj, "Cfoo")(input), array(1.5*exp(input[2:5])^2, dim = 4))
  derivs <- method(obj, "Cfoo_derivs_")(input, c(0, 1))
  expect_true(nCompiler:::is.loadedObjectEnv(derivs))
  gradient <- matrix(0, nrow = 7, ncol = 4)
  gradient[2:5, ] <- diag(3*exp(input[2:5])^2)
  expect_equal(value(derivs, "gradient"), gradient)
})

utils <- system.file(
  file.path(
    'tests', 'testthat',
    c('testing_utils.R', 'AD_utils.R')
  ),
  package = 'nCompiler'
)
for (util_file in utils) source(util_file)

###################################
# construct AD test lists
# TODO: move this into another file
###################################

unaryOpsAD <- intersect(
  getMatchingOps('testthat', 'testAD', TRUE),
  getMatchingOps('testthat', 'isUnary', TRUE)
)
binaryOpsAD <- intersect(
  getMatchingOps('testthat', 'testAD', TRUE),
  getMatchingOps('testthat', 'isBinary', TRUE)
)

## derivatives currently only available for scalar and vector inputs
unaryArgTypes <- c('numericScalar', 'numericVector(7)')
unaryOpTestsAD <- make_AD_test_batch(unaryOpsAD, unaryArgTypes)

binaryArgTypes <- list(
  c('numericScalar', 'numericScalar'),
  c('numericScalar', 'numericVector(7)'),
  c('numericVector(7)', 'numericVector(7)'),
  c('numericVector(7)', 'numericScalar')
)
binaryOpTestsAD <- make_AD_test_batch(binaryOpsAD, binaryArgTypes)

#############
# run testing
#############

## TODO: clear comments of what to do at the top of files
WRITE_GOLD_FILES <- FALSE ## ignored if FULL_TESTING is TRUE
FULL_TESTING <- FALSE

## FULL_TESTING_GRANULARITY levels:
##   1 = put all test params in one giant nClass
##   2 = group operators by type in one nClass
##   3 = one nClass per operator (this is also what gold testing does)
##   4 = one nClass with one nFunction per operator/input combo
FULL_TESTING_GRANULARITY <- NA

if (WRITE_GOLD_FILES) {
  gold_file_dir <- readline(
    # set this to your local repo's gold_files dir
    prompt = 'Where do you want to save the gold files? '
  )
} else {
  gold_file_dir <- system.file(
    file.path('tests', 'testthat', 'gold_files'),
    package = 'nCompiler'
  )
}

## run_test_suite handles granularity levels 2-4 and does nothing if
## FULL_TESTING_GRANULARITY is 1. Instead, we handle that case by
## putting all of the tests in one long list and calling test_math directly.
run_test_suite(
  unaryOpTestsAD, 'AD_unaryOpTests', test_AD, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)

run_test_suite(
  binaryOpTestsAD, 'AD_binaryOpTests', test_AD, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)

## handle granularity level 1
if (FULL_TESTING && isTRUE(FULL_TESTING_GRANULARITY == 1)) {
  ## put everything in one giant nClass
  test_base(
    c(unlist(unaryOpTestsAD), unlist(binaryOpTestsAD)), 'testing AD', test_AD
  )
}

# TODO: move knownFailures to separate file
## tests with numericVector fail to compile for an unknown reason
# modifyOnMatch(ADopTests, '.+ numericVector', 'knownFailure', '.* compiles')

## .method operators don't work with scalar input
## modifyOnMatch(
##   ADopTests,
##   '(\\^|abs|atan|cube|exp|inverse|lgamma|log|logit|rsqrt|sqrt|square|tanh) numericScalar',
##   'knownFailure', '.* compiles'
## )
