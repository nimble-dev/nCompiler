context("Testing of math functions in nCompiler code\n")

## To add math tests for a new operator, see instructions in
## testing_operatorLists.R.

## Utility files:
## * testing_utils.R: useful testing utility functions.
## * testing_operatorLists.R: updates operator definitions with info needed for
##                            testing.
## * known_failures.R: updates operator definitions with info needed to catch
##                     failures of specific tests.
## * math_utils.R: utility functions that are specific to this test file.
utils <- system.file(
  file.path(
    'tests', 'testthat',
    c('testing_utils.R', 'testing_operatorLists.R', 'known_failures.R',
      'math_utils.R')
  ),
  package = 'nCompiler'
)
for (util_file in utils) source(util_file)

#############
# run testing
#############

## Gold file testing takes approx. 1 minute.

WRITE_GOLD_FILES <- TRUE ## ignored if FULL_TESTING is TRUE
FULL_TESTING <- FALSE

## FULL_TESTING_GRANULARITY levels:
##   1 = put all test params in one giant nClass (approx. 15-20 minutes)
##   2 = one nClass per operator
##   3 = one nClass (with only one method) per operator/input combo
##
## Right now FULL_TESTING_GRANULARITY levels 2 and 3 are not working because of
## segfaults encountered when compiling many nClasses sequentially. Level 1
## should work.
##
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

math_test_params <- make_math_test_params(get_math_ops())

## You may find a test fails / many tests fail in full testing and want to
## drill down. There are a couple of ways to do so:
##
## 1. Only test a range of operators in math_test_params by indexing the list:
##    run_test_suite(
##      math_test_params[2:10], 'math', test_math, FULL_TESTING,
##      FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
##      gold_file_dir
##    )
## 2. Only test one operator using name indexing:
##    run_test_suite(
##      math_test_params['exp'], 'math', test_math, FULL_TESTING,
##      FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
##      gold_file_dir
##    )
## 3. Use test_base() directly to test a single operator / input combo:
##    test_base(
##      math_test_params[['pmax']]['pmax arg1 = numericScalar arg2 = numericMatrix'],
##      test_name = 'pmax', test_fun = test_math, suppress_err_msgs = FALSE
##    )

run_test_suite(
  math_test_params, 'math', test_math, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)
