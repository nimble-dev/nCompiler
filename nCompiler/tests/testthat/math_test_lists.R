## TODO: improve test creation (e.g. how AD testing in nimble does it)

## every operator needs an R version of the same name in order to call
## the uncompiled nFunctions
square <- function(x) x*x
cube <- function(x) x*x*x
logit <- function(x) log(x/(1-x))
rsqrt <- function(x) 1/sqrt(x)

argTypes <- c(
  'numericScalar', 'integerScalar', 'logicalScalar',
  'numericVector', 'integerVector', 'logicalVector',
  'numericMatrix', 'integerMatrix', 'logicalMatrix',
  'numericArray(nDim=3)',  'integerArray(nDim=3)',  'logicalArray(nDim=3)'
)

###############
## unaryOpTests
###############

unaryOps <- intersect(
  getMatchingOps('testing', 'testMath', TRUE),
  getMatchingOps('testing', 'isUnary', TRUE)
)

makeUnaryOpTestBatch <- function(op, argTypes) {
  o <- lapply(argTypes, function(argType) makeOperatorParam(op, argType))
  names(o) <- sapply(o, `[[`, 'name')
  return(o)
}

unaryOpTests <- lapply(
  unaryOps, makeUnaryOpTestBatch, argTypes = argTypes
)
names(unaryOpTests) <- unaryOps

## cannot yet handle scalar input with .method or unaryExpr operators
modifyBatchOnMatch(
  unaryOpTests, paste0(
  '^(abs|all|any|cube|exp|log|prod|rsqrt|sqrt|square|tanh',
  '|atan|inverse|logit|min|max|mean|squaredNorm)'), '.+Scalar',
  'knownFailure', '.*compiles'
)

## not implemented yet
modifyBatchOnMatch(
  unaryOpTests,
  'squaredNorm', ' .+',
  'knownFailure', '.*compiles'
)

################
## binaryOpTests
################

## TODO: 

makeBinaryOpTestBatch <- function(op, argTypes) {
  o <- mapply(function(argTuple) makeOperatorParam(op, argTuple),
              argTypes, SIMPLIFY = FALSE)
  names(o) <- sapply(o, `[[`, 'name')
  return(o)
}

binaryOps <- intersect(
  getMatchingOps('testing', 'testMath', TRUE),
  getMatchingOps('testing', 'isBinary', TRUE)
)

binaryArgTypes <- as.list(
  data.frame(t(expand.grid(argTypes, argTypes)), stringsAsFactors=FALSE)
)

binaryOpTests <- lapply(
    binaryOps, makeBinaryOpTestBatch, argTypes = binaryArgTypes
)
names(binaryOpTests) <- binaryOps

##                 ##
## run-time errors ##
##                 ##

## vector is not recycled in Eigen as in R
modifyBatchOnMatch(
  binaryOpTests, '(\\+|-|pmin|pmax|/|\\*)', '.+(Matrix|Array.+) numericVector',
  'knownFailure', '.*runs'
)

## difference in the way pmin/pmax works in R and cwiseMin/cwiseMax works in Eigen
modifyBatchOnMatch(
  binaryOpTests, '(pmin|pmax)', '(integer|logical)Vector .+Matrix',
  'knownFailure', '.*runs'
)
modifyBatchOnMatch(
  binaryOpTests, '(pmin|pmax)', '.+Vector .+Array',
  'knownFailure', '.*runs'
)

## Eigen's seems to pad the matrix with 0's to make an array of the same shape,
## whereas R uses recycling
modifyBatchOnMatch(
  binaryOpTests, '(pmin|pmax)', '.+Array.+ numericMatrix',
  'knownFailure', '.*runs'
)

## std:bad_alloc error ... R would return a matrix but in Eigen we're calling the vector's +/- method
## similar issue for pmin/pmax, although the R output would be different
modifyBatchOnMatch(
  binaryOpTests,
  '(\\+|-|pmin|pmax|==|!=|<=|>=|<|>|&|\\||/|\\*)', '.+Vector .+(Matrix|Array)',
  'knownFailure', '.*runs'
)
## pmin/pmax also lead to std:bad_alloc in these cases
modifyBatchOnMatch(
  binaryOpTests, '(pmin|pmax)', '.+Matrix .+Array',
  'knownFailure', '.*runs'
)

##                     ##
## compile-time errors ##
##                     ##

## no cwiseMin/cwiseMax method for primitive types
modifyBatchOnMatch(
  binaryOpTests,
  '(pmin|pmax)', '.+Scalar .+Scalar',
  'knownFailure', '.*compiles'
)

## swapping args won't work for scalar and matrix/array combos
modifyBatchOnMatch(
  binaryOpTests,
  '(pmin|pmax)', '.+Scalar .+(Matrix|Array)',
  'knownFailure', '.*compiles'
)

## Eigen doesn't seem to have implementations for && and || when one arg is a scalar
modifyBatchOnMatch(
  binaryOpTests, '(&|\\|)', '.+(Vector|Matrix|Array.+) .+Scalar',
  'knownFailure', '.*compiles'
)
modifyBatchOnMatch(
  binaryOpTests, '(&|\\|)',
  '.+Scalar .+(Vector|Matrix|Array.+)',
  'knownFailure', '.*compiles'
)

## no pow method for scalars in C++
modifyBatchOnMatch(
  binaryOpTests, '(\\^)', '.+Scalar .+.+',
  'knownFailure', '.*compiles'
)
## Eigen pow and % don't know how to handle non-scalar exponents
modifyBatchOnMatch(
  binaryOpTests, '(\\^|%%)', '.+ .+(Vector|Matrix|Array)',
  'knownFailure', '.*compiles'
)

## % only works for integerScalar integerScalar input
modifyBatchOnMatch(
  binaryOpTests, '%%', '.+Scalar (numeric|logical)Scalar',
  'knownFailure', '.*compiles'
)
modifyBatchOnMatch(
  binaryOpTests, '%%', '(numeric|logical)Scalar integerScalar',
  'knownFailure', '.*compiles'
)

## Eigen % seems to need an integer or logical scalar rhs
modifyBatchOnMatch(
  binaryOpTests, '%%', '.+(Vector|Matrix|Array.+) numericScalar',
  'knownFailure', '.*compiles'
)
modifyBatchOnMatch(
  binaryOpTests, '%%', 'numeric(Vector|Matrix|Array.+) logicalScalar',
  'knownFailure', '.*compiles'
)

##               ##
## tests to skip ##
##               ##

## vector is not recycled properly, but test doesn't always fail
modifyBatchOnMatch(
  binaryOpTests, '(==|!=|<=|>=|<|>|&|\\||/)', '.+(Matrix|Array.+) .+Vector',
  'skip', TRUE
)
modifyBatchOnMatch(
  binaryOpTests,
  '(\\+|-|pmin|pmax|/|\\*)', 'numeric(Matrix|Array.+) (integer|logical)Vector',
  'skip', TRUE
)
modifyBatchOnMatch(
  binaryOpTests,
  '(\\+|-|pmin|pmax|/|\\*)', '(integer|logical)(Matrix|Array.+) .+Vector',
  'skip', TRUE
)
modifyBatchOnMatch(
  binaryOpTests, '(pmin|pmax)', '.+Array.+ (integer|logical)Matrix',
  'skip', TRUE
)

## scalar division/moldulo by 0 in C++ leads to R floating point exception
modifyBatchOnMatch(
  binaryOpTests, '/', '.+Scalar (integer|logical)Scalar',
  'skip', TRUE
)
modifyBatchOnMatch(
  binaryOpTests, '%%', '.+ (integer|logical)Scalar',
  'skip', TRUE
)
