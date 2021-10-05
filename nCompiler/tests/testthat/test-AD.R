# not working - as of switch to github actions for CI testing
context("Testing Automatic Differentiation")

## To add AD tests for a new operator, see instructions in
## testing_operatorLists.R.

## Utility files:
## * testing_utils.R: useful testing utility functions.
## * testing_operatorLists.R: updates operator definitions with info needed for
##                            testing.
## * known_failures.R: updates operator definitions with info needed to catch
##                     failures of specific tests.
## * AD_utils.R: utility functions that are specific to this test file.
utils <- system.file(
  file.path(
    'tests', 'testthat',
    c('testing_utils.R', 'testing_operatorLists.R', 'known_failures.R',
      'AD_utils.R')
  ),
  package = 'nCompiler'
)
for (util_file in utils) source(util_file)

#####################################
# construct AD test parameterizations
#####################################

seed <- 0
AD_test_params <- make_AD_test_params(get_AD_ops(), seed)

#############
# run testing
#############

WRITE_GOLD_FILES <- FALSE ## ignored if FULL_TESTING is TRUE
FULL_TESTING <- FALSE

## FULL_TESTING_GRANULARITY levels:
##   1 = put all test params in one giant nClass
##   2 = one nClass per operator (this is also what gold testing does)
##   3 = one nClass with one nFunction per operator/input combo
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

## You may find a test fails / many tests fail in full testing and want to
## drill down. There are a couple of ways to do so:
##
## 1. Only test a range of operators in math_test_params by indexing the list:
##    run_test_suite(
##      AD_test_params[2:10], 'AD', test_AD, FULL_TESTING,
##      FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
##      gold_file_dir
##    )
## 2. Only test one operator using name indexing:
##    run_test_suite(
##      AD_test_params['exp'], 'AD', test_AD, FULL_TESTING,
##      FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
##      gold_file_dir
##    )
## 3. Use test_base() directly to test a subset of one operator's tests:
##    test_base(
##      AD_test_params[['-']][5:6],
##      test_name = '-', test_fun = test_AD, suppress_err_msgs = FALSE
##    )
## 4. Use test_base() directly to test a single operator / input combo:
##    test_base(
##      AD_test_params[['+']]['+ arg1 = numericScalar arg2 = numericVector(7)'],
##      test_name = '+', test_fun = test_AD, suppress_err_msgs = FALSE
##    )

run_test_suite(
  AD_test_params, 'AD', test_AD, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)

## reset the automaticDerivatives option
set_nOption('automaticDerivatives', derivs_option)
