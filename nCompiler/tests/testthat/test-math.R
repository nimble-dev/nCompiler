context("Testing of math functions in nCompiler code\n")

utils <- system.file(
  file.path(
    'tests', 'testthat',
    c('testing_utils.R', 'math_utils.R', 'math_test_lists.R')
  ),
  package = 'nCompiler'
)
for (util_file in utils) source(util_file)

## TODO: clear comments of what to do at the top of files
WRITE_GOLD_FILES <- FALSE ## ignored if FULL_TESTING is TRUE
FULL_TESTING <- TRUE

if (WRITE_GOLD_FILES) {
  gold_file_dir <- readline(
    prompt = 'Where do you want to save the gold files? '
  )
} else {
  gold_file_dir <- system.file(
    file.path('tests', 'testthat', 'gold_files'),
    package = 'nCompiler'
  )
}

## Ensure that generated strings for unique names
## have counters that start at 1
nCompiler:::nFunctionIDMaker(reset = TRUE)
nCompiler:::nClassIDMaker(reset = TRUE)

## FULL_TESTING_GRANULARITY levels:
##   1 = put all test params in one giant nClass
##   2 = group operators by type in one nClass
##   3 = one nClass per operator
##   4 = one nClass with one nFunction per operator/input combo
FULL_TESTING_GRANULARITY <- 1

## test_math_suite handles granularity levels 2-4 and does nothing if
## FULL_TESTING_GRANULARITY is 1. Instead, we handle that case by putting all
## of the tests in one long list and calling test_math directly.
test_math_suite(
  unaryOpTests, 'math_unaryOpTests', FULL_TESTING, FULL_TESTING_GRANULARITY,
  write_gold_file = WRITE_GOLD_FILES, gold_file_dir
)

test_math_suite(
  binaryOpTests, 'math_binaryOpTests', FULL_TESTING, FULL_TESTING_GRANULARITY,
  write_gold_file = WRITE_GOLD_FILES, gold_file_dir
)

## handle granularity level 1
if (FULL_TESTING && FULL_TESTING_GRANULARITY == 1) {
  ## put everything in one giant nClass
  test_math(
    c(unlist(unaryOpTests, recursive = FALSE),
      unlist(binaryOpTests, recursive = FALSE)),
    'testing math'
  )
}
