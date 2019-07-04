########################################
## construct math test parameterizations
########################################

math_ops <- get_matching_ops('testing', 'math_argTypes',
                             function(x) !is.null(x))
math_test_params <- sapply(math_ops, make_math_test_param_batch,
                           simplify = FALSE)

#################
## known failures
#################

## cannot yet handle scalar input with .method or unaryExpr operators
modifyBatchOnMatch(
  math_test_params, paste0(
  '^(abs|all|any|cube|exp|log|prod|rsqrt|sqrt|square|tanh',
  '|atan|inverse|logit|min|max|mean|squaredNorm)'),
  'arg1 = .+Scalar',
  'knownFailure', '.*compiles'
)

## not implemented yet
modifyBatchOnMatch(
  math_test_params,
  'squaredNorm', ' .+',
  'knownFailure', '.*compiles'
)

##                 ##
## run-time errors ##
##                 ##

## vector is not recycled in Eigen as in R
modifyBatchOnMatch(
  math_test_params, '(\\+|-|pmin|pmax|/|\\*)',
  'arg1 = .+(Matrix|Array.+) arg2 = numericVector',
  'knownFailure', '.*runs'
)

## difference in the way pmin/pmax works in R and cwiseMin/cwiseMax works in Eigen
modifyBatchOnMatch(
  math_test_params, '(pmin|pmax)',
  'arg1 = (integer|logical)Vector arg2 = .+Matrix',
  'knownFailure', '.*runs'
)
modifyBatchOnMatch(
  math_test_params, '(pmin|pmax)',
  'arg1 = .+Vector arg2 = .+Array',
  'knownFailure', '.*runs'
)

## Eigen's seems to pad the matrix with 0's to make an array of the same shape,
## whereas R uses recycling
modifyBatchOnMatch(
  math_test_params, '(pmin|pmax)',
  'arg1 = .+Array.+ arg2 = numericMatrix',
  'knownFailure', '.*runs'
)

## std:bad_alloc error ... R would return a matrix but in Eigen we're calling the vector's +/- method
## similar issue for pmin/pmax, although the R output would be different
modifyBatchOnMatch(
  math_test_params,
  '(\\+|-|pmin|pmax|==|!=|<=|>=|<|>|&|\\||/|\\*)',
  'arg1 = .+Vector arg2 = .+(Matrix|Array)',
  'knownFailure', '.*runs'
)
## pmin/pmax also lead to std:bad_alloc in these cases
modifyBatchOnMatch(
  math_test_params, '(pmin|pmax)',
  'arg1 = .+Matrix arg2 = .+Array',
  'knownFailure', '.*runs'
)

##                     ##
## compile-time errors ##
##                     ##

## no cwiseMin/cwiseMax method for primitive types
modifyBatchOnMatch(
  math_test_params,
  '(pmin|pmax)', 'arg1 = .+Scalar arg2 = .+Scalar',
  'knownFailure', '.*compiles'
)

## swapping args won't work for scalar and matrix/array combos
modifyBatchOnMatch(
  math_test_params,
  '(pmin|pmax)', 'arg1 = .+Scalar arg2 = .+(Matrix|Array)',
  'knownFailure', '.*compiles'
)

## Eigen doesn't seem to have implementations for && and || when one arg is a scalar
modifyBatchOnMatch(
  math_test_params, '(&|\\|)',
  'arg1 = .+(Vector|Matrix|Array.+) arg2 = .+Scalar',
  'knownFailure', '.*compiles'
)
modifyBatchOnMatch(
  math_test_params, '(&|\\|)',
  'arg1 = .+Scalar arg2 = .+(Vector|Matrix|Array.+)',
  'knownFailure', '.*compiles'
)

## no pow method for scalars in C++
modifyBatchOnMatch(
  math_test_params, '(\\^)', 'arg1 = .+Scalar arg2 = .+',
  'knownFailure', '.*compiles'
)
## Eigen pow and % don't know how to handle non-scalar exponents
modifyBatchOnMatch(
  math_test_params, '(\\^|%%)',
  'arg1 = .+ arg2 = .+(Vector|Matrix|Array)',
  'knownFailure', '.*compiles'
)

## with scalars, %% only works for combos of integerScalar and logicalScalar
modifyBatchOnMatch(
  math_test_params, '%%',
  'arg1 = .+Scalar arg2 = numericScalar',
  'knownFailure', '.*compiles'
)
modifyBatchOnMatch(
  math_test_params, '%%',
  'arg1 = numericScalar arg2 = .+Scalar',
  'knownFailure', '.*compiles'
)

## Eigen % seems to need an integer or logical scalar rhs
modifyBatchOnMatch(
  math_test_params, '%%',
  'arg1 = .+(Vector|Matrix|Array.+) arg2 = numericScalar',
  'knownFailure', '.*compiles'
)
modifyBatchOnMatch(
  math_test_params, '%%',
  'arg1 = numeric(Vector|Matrix|Array.+) arg2 = logicalScalar',
  'knownFailure', '.*compiles'
)

##               ##
## tests to skip ##
##               ##

## vector is not recycled properly, but test doesn't always fail
modifyBatchOnMatch(
  math_test_params, '(==|!=|<=|>=|<|>|&|\\||/)',
  'arg1 = .+(Matrix|Array.+) arg2 = .+Vector',
  'skip', TRUE
)
modifyBatchOnMatch(
  math_test_params,
  '(\\+|-|pmin|pmax|/|\\*)',
  'arg1 = numeric(Matrix|Array.+) arg2 = (integer|logical)Vector',
  'skip', TRUE
)
modifyBatchOnMatch(
  math_test_params,
  '(\\+|-|pmin|pmax|/|\\*)',
  'arg1 = (integer|logical)(Matrix|Array.+) arg2 = .+Vector',
  'skip', TRUE
)
modifyBatchOnMatch(
  math_test_params, '(pmin|pmax)',
  'arg1 = .+Array.+ arg2 = (integer|logical)Matrix',
  'skip', TRUE
)

## scalar division/moldulo by 0 in C++ leads to R floating point exception
modifyBatchOnMatch(
  math_test_params, '/',
  'arg1 = .+Scalar arg2 = (integer|logical)Scalar',
  'skip', TRUE
)
modifyBatchOnMatch(
  math_test_params, '%%',
  'arg1 = .+ arg2 = (integer|logical)Scalar',
  'skip', TRUE
)
