context("Testing of math functions in nCompiler code\n")

## To add math tests for a new operator, see instructions in
## testing_operatorLists.R.

## Utility files:
## * testing_utils.R: useful testing utility functions.
## * testing_operatorLists.R: updates operator definitions with info needed for
##                            testing.
## * math_utils.R: utility functions that are specific to this test file.
## * math_test_lists.R: creates math_test_params and adds knownFailures.
utils <- system.file(
  file.path(
    'tests', 'testthat',
    c('testing_utils.R', 'testing_operatorLists.R', 'math_utils.R',
      'math_test_lists.R')
  ),
  package = 'nCompiler'
)
for (util_file in utils) source(util_file)

#############
# run testing
#############

WRITE_GOLD_FILES <- FALSE ## ignored if FULL_TESTING is TRUE
FULL_TESTING <- FALSE

## FULL_TESTING_GRANULARITY levels:
##   1 = put all test params in one giant nClass
##   2 = one nClass per operator
##   3 = one nClass (with only one method) per operator/input combo
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

## the first argument to run_test_suite() is a list created by make_math_test_params()
run_test_suite(
  math_test_params, 'math', test_math, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)
