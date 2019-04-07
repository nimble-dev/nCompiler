context("Testing of math functions in nCompiler code\n")

source('./testing_utils.R')

test_math <- function(param, info, size = 3,
                      dir = file.path(tempdir(), "nCompiler_generatedCode"),
                      control = list(), verbose = nOptions('verbose')) {
  if (verbose) {
    cat("### -------------------------------------------- ###\n")
    cat("### Testing", param$name, "###\n")
  }
  if (!is.null(param$skip) && param$skip) {
    if (verbose) cat("### Skipping this test ###\n")
    return(invisible(NULL))
  }
  if (!is.null(param$debug) && param$debug) browser()

  nfR <- gen_nFunction(param)

  info <- paste0(info, ": compiles")
  ## need expect_error not expect_failure(expect_something()) because otherwise
  ## R error will stop execution
  wrap_if_matches(param$knownFailure, info, expect_error, {
    if (!is.null(param$argChecks)) {
      argsR <- mapply(
        make_input, param$argTypes, param$argChecks, size, SIMPLIFY = FALSE
      )
    } else argsR <- lapply(param$argTypes, make_input, size = size)

    argsC <- argsR
    names(argsC) <- nCompiler:::mangleArgumentNames(names(argsC))

    if (verbose) cat("## Calling R version of nFunction ##\n")
    ansR <- try(do.call(nfR, argsR), silent = TRUE)
    if (inherits(ansR, 'try-error')) {
      if(verbose) {
        cat("## Calling R version resulted in an error: ##\n")
        cat(paste('##', ansR[1]))
        cat("## Skipping the rest of test ##\n")
      }
      return(invisible(NULL))
    }

    if (verbose) cat("## Compiling nFunction ##\n")
    if (!is.null(param$dir)) dir <- param$dir
    nfC <- nCompile_nFunction(nfR, dir = dir, control = control)

    info <- paste0(info, ": runs")
    wrap_if_matches(param$knownFailure, info, expect_failure, {
      ## For vectors, current R returns a vector while C++ returns a 1D array.
      ## We can modify the wrap<> specialization to change this.
      if (verbose) cat("## Calling compiled nFunction ##\n")
      ansC <- do.call(nfC, argsC)
      if(verbose) cat("## Testing equality ##\n")
      if(is.array(ansC)) {
        expect_equal(as.array(ansR),
                     ansC,
                     info = paste0("Test: ", info))
      } else {
        expect_equal(ansR,
                     ansC,
                     info = paste0("Test: ", info))
      }
      if (verbose) cat("### -------------------------------------------- ###\n")

    })
  })
  invisible(NULL)
}

## As in NIMBLE's BUGS_mathCompatability.R. Where to put this?
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

###################
## cWiseUnaryTests
###################

makeUnaryCwiseTest <- function(name, op, argType) {
  makeOperatorParam(name, op, argType)
}

## TODO: move this info to compile_aaa_operatorLists.R
cWiseUnaryOps <- c(
  '-', 'min', 'max', 'mean', 'prod', 'squaredNorm', 'exp', 'inverse',
  'log', 'rsqrt', 'sqrt', 'square', 'tanh', 'abs', 'cube', 'atan'
)
cWiseUnaryTests <- unlist(
  recursive = FALSE,
  x = lapply(
    cWiseUnaryOps,
    function(x) {
      mapply(
        makeUnaryCwiseTest,
        argType = argTypes,
        MoreArgs = list(name = x, op = x),
        SIMPLIFY = FALSE
      )
    })
)
names(cWiseUnaryTests) <- sapply(cWiseUnaryTests, `[[`, 'name')

## cannot yet handle scalar input with .method or unaryExpr operators
dot_method_regex <- paste0(
  '^(abs|cube|exp|log|prod|rsqrt|sqrt|square|tanh',
  '|atan|inverse|logit|min|max|mean|squaredNorm) .+Scalar$'
)
modifyOnMatch(cWiseUnaryTests, dot_method_regex, 'knownFailure', '.*compiles')

set.seed(0)
test_batch(test_math, cWiseUnaryTests, 'cWiseUnaryOps', verbose = TRUE)

###################
## cWiseBinaryTests
###################

makeBinaryCwiseTest <- function(name, op, argTypes) {
  param <- makeOperatorParam(name, op, argTypes)
  param$argChecks = getBinaryArgChecks(op)
  param
}

## TODO: move this info to compile_aaa_operatorLists.R
cWiseBinaryOps <- c(
  '+', '-', 'pmin', 'pmax', '==', '!=', '<=', '>=', '<', '>', '&', '|', '/', '*',
  '^', '%%'
)

binaryArgTypes <- as.list(
  data.frame(t(expand.grid(argTypes, argTypes)), stringsAsFactors=FALSE)
)
cWiseBinaryTests <- unlist(
  recursive = FALSE,
  x = lapply(
    cWiseBinaryOps,
    function(x) {
      mapply(
        makeBinaryCwiseTest,
        argTypes = binaryArgTypes,
        MoreArgs = list(name = x, op = x),
        SIMPLIFY = FALSE
      )
    })
)
names(cWiseBinaryTests) <- sapply(cWiseBinaryTests, `[[`, 'name')

##                 ##
## run-time errors ##
##                 ##

## vector is not recycled in Eigen as in R
modifyOnMatch(cWiseBinaryTests,
              '(\\+|-|pmin|pmax|/|\\*) .+(Matrix|Array.+) numericVector',
              'knownFailure', '.*runs')

## difference in the way pmin/pmax works in R and cwiseMin/cwiseMax works in Eigen
modifyOnMatch(cWiseBinaryTests, '(pmin|pmax) (integer|logical)Vector .+Matrix',
              'knownFailure', '.*runs')
modifyOnMatch(cWiseBinaryTests, '(pmin|pmax) .+Vector .+Array',
              'knownFailure', '.*runs')

## Eigen's seems to pad the matrix with 0's to make an array of the same shape,
## whereas R uses recycling
modifyOnMatch(cWiseBinaryTests, '(pmin|pmax) .+Array.+ numericMatrix',
              'knownFailure', '.*runs')

##                     ##
## compile-time errors ##
##                     ##

## std:bad_alloc error ... R would return a matrix but in Eigen we're calling the vector's +/- method
## similar issue for pmin/pmax, although the R output would be different
modifyOnMatch(
  cWiseBinaryTests,
  paste0('(\\+|-|pmin|pmax|==|!=|<=|>=|<|>|&|\\||/|\\*) ',
         '.+Vector .+(Matrix|Array)'),
  'knownFailure', '.*compiles'
)
## pmin/pmax also lead to std:bad_alloc in these cases
modifyOnMatch(cWiseBinaryTests, '(pmin|pmax) .+Matrix .+Array',
              'knownFailure', '.*compiles')

## no cwiseMin/cwiseMax method for primitive types
modifyOnMatch(cWiseBinaryTests,
              '(pmin|pmax) .+Scalar .+Scalar',
              'knownFailure', '.*compiles')

## swapping args won't work for scalar and matrix/array combos
modifyOnMatch(cWiseBinaryTests,
              '(pmin|pmax) .+Scalar .+(Matrix|Array)',
              'knownFailure', '.*compiles')

## Eigen doesn't seem to have implementations for && and || when one arg is a scalar
modifyOnMatch(cWiseBinaryTests,
              '(&|\\|) .+(Vector|Matrix|Array.+) .+Scalar',
              'knownFailure', '.*compiles')
modifyOnMatch(cWiseBinaryTests,
              '(&|\\|) .+Scalar .+(Vector|Matrix|Array.+)',
              'knownFailure', '.*compiles')

## no pow method for scalars in C++
modifyOnMatch(cWiseBinaryTests,
              '(\\^) .+Scalar .+.+',
              'knownFailure', '.*compiles')
## Eigen pow and % don't know how to handle non-scalar exponents
modifyOnMatch(cWiseBinaryTests, '(\\^|%%) .+ .+(Vector|Matrix|Array)', 'knownFailure', '.*compiles')

## % only works for integerScalar integerScalar input
modifyOnMatch(
  cWiseBinaryTests,
  '%% .+Scalar (numeric|logical)Scalar',
  'knownFailure', '.*compiles'
)
modifyOnMatch(
  cWiseBinaryTests,
  '%% (numeric|logical)Scalar integerScalar',
  'knownFailure', '.*compiles'
)

## Eigen % seems to need an integer or logical scalar rhs
modifyOnMatch(
  cWiseBinaryTests,
  '%% .+(Vector|Matrix|Array.+) numericScalar',
  'knownFailure', '.*compiles'
)
modifyOnMatch(
  cWiseBinaryTests,
  '%% numeric(Vector|Matrix|Array.+) logicalScalar',
  'knownFailure', '.*compiles'
)

##               ##
## tests to skip ##
##               ##

## vector is not recycled properly, but test doesn't always fail
modifyOnMatch(
  cWiseBinaryTests,
  '(==|!=|<=|>=|<|>|&|\\||/) .+(Matrix|Array.+) .+Vector',
  'skip', TRUE
)
modifyOnMatch(cWiseBinaryTests,
              '(\\+|-|pmin|pmax|/|\\*) numeric(Matrix|Array.+) (integer|logical)Vector',
              'skip', TRUE)
modifyOnMatch(cWiseBinaryTests,
              '(\\+|-|pmin|pmax|/|\\*) (integer|logical)(Matrix|Array.+) .+Vector',
              'skip', TRUE)
modifyOnMatch(cWiseBinaryTests, '(pmin|pmax) .+Array.+ (integer|logical)Matrix',
              'skip', TRUE)

## scalar division/moldulo by 0 in C++ leads to R floating point exception
modifyOnMatch(cWiseBinaryTests, '/ .+Scalar (integer|logical)Scalar', 'skip', TRUE)
modifyOnMatch(cWiseBinaryTests, '% .+ (integer|logical)Scalar', 'skip', TRUE)

test_batch(test_math, cWiseBinaryTests, verbose = TRUE)
