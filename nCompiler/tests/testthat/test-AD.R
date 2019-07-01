context("Testing Automatic Differentiation")

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
  getMatchingOps('testing', 'testAD', TRUE),
  getMatchingOps('testing', 'isUnary', TRUE)
)
binaryOpsAD <- intersect(
  getMatchingOps('testing', 'testAD', TRUE),
  getMatchingOps('testing', 'isBinary', TRUE)
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

derivs_option <- nOptions('automaticDerivatives')
set_nOption('automaticDerivatives', TRUE)

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

## reset the automaticDerivatives option
set_nOption('automaticDerivatives', derivs_option)

# TODO: move knownFailures to separate file
## tests with numericVector fail to compile for an unknown reason
# modifyOnMatch(ADopTests, '.+ numericVector', 'knownFailure', '.* compiles')

## .method operators don't work with scalar input
## modifyOnMatch(
##   ADopTests,
##   '(\\^|abs|atan|cube|exp|inverse|lgamma|log|logit|rsqrt|sqrt|square|tanh) numericScalar',
##   'knownFailure', '.* compiles'
## )
