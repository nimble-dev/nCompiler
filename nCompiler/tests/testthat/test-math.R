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
FULL_TESTING <- FALSE

## FULL_TESTING_GRANULARITY levels:
##   1 = put all test params in one giant nClass
##   2 = group operators by type in one nClass
##   3 = one nClass per operator
##   4 = one nClass with one nFunction per operator/input combo
FULL_TESTING_GRANULARITY <- 1

## TODO: add comments explaining all of this
test_math_suite(unaryOpTests, FULL_TESTING, FULL_TESTING_GRANULARITY)
test_math_suite(binaryOpTests, FULL_TESTING, FULL_TESTING_GRANULARITY)

if (FULL_TESTING && FULL_TESTING_GRANULARITY == 1) {
  ## put everything in one giant nClass
  test_math(
    c(unlist(unaryOpTests), unlist(binaryOpTests)), 'testing math'
  )
}
