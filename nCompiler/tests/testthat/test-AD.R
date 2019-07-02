context("Testing Automatic Differentiation")

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

ops_AD <- get_matching_ops('testing', 'AD_argTypes', function(x) !is.null(x))
params_AD <- sapply(ops_AD, make_AD_test_param_batch, simplify = FALSE)

#############
# run testing
#############

## TODO: clear comments of what to do at the top of files
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
  params_AD, 'AD', test_AD, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)

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
