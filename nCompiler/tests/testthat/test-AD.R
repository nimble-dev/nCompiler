context("Testing Automatic Differentiation")

## To add AD tests for a new operator, see instructions in
## testing_operatorLists.R.

utils <- system.file(
  file.path(
    'tests', 'testthat',
    c('testing_utils.R', 'testing_operatorLists.R', 'AD_utils.R')
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

run_test_suite(
  AD_test_params, 'AD', test_AD, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)

## reset the automaticDerivatives option
set_nOption('automaticDerivatives', derivs_option)

