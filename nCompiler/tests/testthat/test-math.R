context("Testing of math functions in nCompiler code\n")

source('./testing_utils.R')

test_math <- function(test_batch, info, size = 3,
                      dir = file.path(tempdir(), "nCompiler_generatedCode"),
                      control = list(), verbose = nOptions('verbose'),
                      compile_all_funs = FALSE) {
  compile_error <- sapply(
    test_batch, function(param)
      !is.null(param$knownFailure) && grepl('compiles', param$knownFailure)
  )

  ## these tests should fail during compilation
  nFuns_error <- lapply(test_batch[compile_error], gen_nFunction)
  if (length(nFuns_error) > 0) {
    names(nFuns_error) <- paste0('nFun_error', 1:length(nFuns_error))
    nC_error <- gen_nClass(list(Cpublic = nFuns_error))
    expect_error(nCompile_nClass(nC_error), info = info)
  }

  ## these tests should compile
  compiles <- test_batch[!compile_error]
  nFuns <- lapply(compiles, gen_nFunction)

  if (length(nFuns) > 0) {
    
    if (compile_all_funs) {
      ## useful for debugging an nClass compilation failure
      ## e.g. test_math(binaryOpTests[['+']], compile_all_funs = TRUE)
      for (name in names(nFuns)) {
        if (verbose)
          cat(paste('### Compiling function for test of:', name, '###\n'))
        nCompile_nFunction(nFuns[[name]])
      }
    }
    
    names(nFuns) <- paste0('nFun', 1:length(nFuns))
    nC <- gen_nClass(list(Cpublic = nFuns))

    nC_compiled <- nCompile_nClass(nC)
    obj <- nC_compiled$new()

    for (i in seq_along(compiles)) {
      param <- compiles[[i]]
      nFun_i <- paste0('nFun', i)

      if (!is.null(param$skip) && param$skip) {
        if (verbose) cat(paste('### Skipping test of', param$name, '###\n'))
      } else {
        if (verbose) cat(paste('### Testing', param$name, '###\n'))
        if (!is.null(param$argChecks)) {
          args <- mapply(
            make_input, param$argTypes, param$argChecks, size, SIMPLIFY = FALSE
          )
        } else args <- lapply(param$argTypes, make_input, size = size)

        if (verbose) cat("## Calling R version of nFunction ##\n")
        ansR <- try(
          do.call(nC$public_methods[[nFun_i]], args),
          silent = TRUE
        )

        if (inherits(ansR, 'try-error')) {
          warning(
            paste(
              'Calling R version of test', param$name,
              'resulted in an error:', ansR[1]
            ),
            immediate. = TRUE
          )
          if (verbose) cat('## Skipping to next test ##\n')
          next
        }

        if (verbose) cat("## Calling compiled nFunction ##\n")
        wrap_if_matches(param$knownFailure, 'runs', expect_error, {
          ansC <- do.call(obj[[nFun_i]], args)
          if(verbose) cat("## Testing equality ##\n")
          if(is.array(ansC)) {
            expect_equal(as.array(ansR),
                         ansC,
                         info = paste("Test:", param$name))
          } else {
            expect_equal(ansR,
                         ansC,
                         info = paste("Test:", param$name))
          }
        })
      }
    }
  }
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

## TODO: move this info to compile_aaa_operatorLists.R
cWiseUnaryOps <- c(
  '-', 'min', 'max', 'mean', 'prod', 'squaredNorm', 'exp', 'inverse',
  'log', 'rsqrt', 'sqrt', 'square', 'tanh', 'abs', 'cube', 'atan'
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
  '^(abs|cube|exp|log|prod|rsqrt|sqrt|square|tanh',
  '|atan|inverse|logit|min|max|mean|squaredNorm)'), '.+Scalar',
  'knownFailure', '.*compiles'
)

set.seed(0)
test_batch(test_math, unaryOpTests, 'unaryOps')

###################
## binaryOpTests
###################

makeBinaryOpTestBatch <- function(op, argTypes) {
  o <- mapply(function(argTuple) makeOperatorParam(op, argTuple),
              argTypes, SIMPLIFY = FALSE)
  names(o) <- sapply(o, `[[`, 'name')
  return(o)
}

## TODO: move this info to compile_aaa_operatorLists.R
binaryOps <- c(
  '+', '-', 'pmin', 'pmax', '==', '!=', '<=', '>=', '<', '>', '&', '|', '/', '*',
  '^', '%%'
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

test_batch(test_math, binaryOpTests)
