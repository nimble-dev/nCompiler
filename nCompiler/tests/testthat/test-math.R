## To add math tests for a new operator, see instructions in
## testing_operatorLists.R.

## Utility files:
## * testing_utils.R: useful testing utility functions.
## * testing_operatorLists.R: updates operator definitions with info needed for
##                            testing.
## * known_failures.R: updates operator definitions with info needed to catch
##                     failures of specific tests.
## * math_utils.R: utility functions that are specific to this test file.
respect_known_failures <- TRUE
# respect_known_failures <- FALSE

utils <- system.file(
  file.path(
    'tests', 'testthat',
    c('testing_utils.R', 'testing_operatorLists.R',
      if(respect_known_failures) 'known_failures.R' else character(),
      'math_utils.R')
  ),
  package = 'nCompiler'
)
for (util_file in utils) source(util_file)

#############
# run testing
#############

## Overview of approach:
## 
## We generate a (very) large set of testing pemutations, involving number of dimensions,
##   scalar element type, and operators.  For example, even for "+", we want
##   double + double, integer + double, logical + double, logical + integer, etc.
##   
##   And we want all of those type combinations crossed with
##   scalar + scalar, scalar + vector, scalar + matrix. vector + vector, etc.
##   
##   These configurations are contained in the math_test_params nested lists below.
##
##   These create an issue of efficiency.  To generate each test as a separate nFunction, compile and run it, takes a long time.
##   We have two shortcuts:
##        (i) We can combined multiple tests as different methods of an nClass.  
##            This reduces the number of DLLs generated and is much faster.  We do this at 
##            two scales: combine all the tests for one operator into an nClass, or combine *ALL* tests for all 
##            operators into one very large (many method) nClass.  These three choices are controlled by 
##            FULL_TESTING_GRANULARIY below
##        (ii) If we have seen given generated C++ work correctly, and if any changes to our code do not result in 
##             different generated C++, and if there are no changes to any hard-coded C++, we can test simply 
##             whether generated C++ matches known, valid generated C++.  We do this by comparing to "gold files"
##             that we trust give valid results.  This is called "gold file testing" as opposed to "full testing".
##             Full testing includes compiling and running everything, comparing uncompiled to compiled results.

## Gold file testing takes approx. 1 minute.
WRITE_GOLD_FILES <- FALSE ## If TRUE, this run of the code will create what we consider correct "gold" files.
                          ## If FALSE, then gold file testing will compare to existing gold files.
                          ## This is ignored if FULL_TESTING is TRUE
FULL_TESTING <- TRUE     ## If TRUE, run actual tests involving compiling and running code.
                          ## If FALSE, only compare generated code to what is in the gold files, but don't actually run it.

## FULL_TESTING_GRANULARITY levels:
##   1 = put all test params in one giant nClass (approx. 15-20 minutes)
##   2 = one nClass per operator
##   3 = one nClass (with only one method) per operator/input combo
##
## Right now FULL_TESTING_GRANULARITY levels 2 and 3 are not working because of
## segfaults encountered when compiling many nClasses sequentially. Level 1
## should work.
##
FULL_TESTING_GRANULARITY <- 2

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

# FIXED: problem with 5: & -- seems like an Eigen quirk (C++ compile error)?
# FIXED: same kind of problem with 14: |
# FIXED: problem with 19: any (mismatch b/w R and C answers -- looks like a type issue?)
# FIXED: problem with 34: length (failure to nCompile)
# problem with 46: pmax: This looks like it was known previously (results mismatch)
# problem with 47: pmin: Ditto

nOptions(pause_after_writing_files=TRUE)
run_test_suite(
  math_test_params[46], 'math', test_math, FULL_TESTING,
  FULL_TESTING_GRANULARITY, write_gold_file = WRITE_GOLD_FILES,
  gold_file_dir
)

debug(test_math)
test_base(
  math_test_params[[46]][55], test_name = "length", test_fun = test_math, suppress_err_msgs = FALSE
)

# Only current known failures are runtime failures for pmin and pmax in pmin(scalar, non-scalar) cases.

# ## Here is code to see tests for every operator run in full mode.
# ## Full mode means the code will actually be compiled and tested (checking compiled to uncompiled results).
# for(opName in names(math_test_params)) {
#   cat(paste0("Testing operator ", opName, " with ", length(math_test_params[[opName]]), " tests.\n"))
#   known_compilation_failures <- lapply(math_test_params[[opName]], `[[`, 'compilation_failure')
#   known_runtime_failures <- lapply(math_test_params[[opName]], `[[`, 'runtime_failure')
#   cat(paste0("There are ", sum(unlist(known_compilation_failures)), " known compilation failures and ",
#              sum(unlist(known_runtime_failures)), " known runtime failures.\n"))  
#   run_test_suite(math_test_params[opName], 'math', test_math, TRUE, 1, FALSE, NA)
# }
# 
# ## Here is code to run a single case, such op 11 in the math_test_params
# run_test_suite(list(test = math_test_params[[opName]][11]), 'math', test_math, TRUE, 1, FALSE, NA)
# 
# ## Here is code to run each case within the list for an op, one by one
# for(j in seq_along(math_test_params[[opName]])) {
#   print(j)
#   test_base(
#     math_test_params[[opName]][j], verbose = TRUE,
#     test_name = paste0('case_',j), test_fun = test_math, suppress_err_msgs = FALSE
#   )
# }


# We fixed a problem giving crashes after lots of test cases.
# The problem was:
# 1. Id counters were reset each time
# 2. This made file names re-used
# 3. This made sourceCpp automatically unload DLLs from the previous version of the same name.
# 4. This left finalizers hanging when garbage collection occurred for an object from an unloaded DLL.
#
# We fixed this in two ways:
# 1. We only reset Id counters in gold file mode, when that is needed to ensure identical code
# 2. We defensively gc() after each test to call finalizers while relevant DLL is loaded.

# For logical operators, compilation tests come from (scalar, non-scalar) cases


