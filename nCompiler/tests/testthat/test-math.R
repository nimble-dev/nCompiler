## not working
context("Testing of math functions in nCompiler code")

## As in NIMBLE's BUGS_mathCompatability.R. Where to put this?
square <- function(x) x*x
cube <- function(x) x*x*x
logit <- function(x) log(x/(1-x))
rsqrt <- function(x) 1/sqrt(x)

make_input <- function(argType, argCheck = NULL, size = 3) {
  arg <- switch(
    argType,
    "numericScalar" = rnorm(1),
    "integerScalar" = rgeom(1, 0.5),
    "logicalScalar" = sample(c(TRUE, FALSE), size = 1),
    "numericVector" = rnorm(size),
    "integerVector" = rgeom(size, 0.5),
    "logicalVector" = sample(c(TRUE, FALSE), size = size, replace = TRUE),
    ## Make different sized dimensions to avoid bugs that might be hidden by
    ## symmetry.
    "numericMatrix" = matrix(rnorm(size*(size+1)), nrow = size, ncol = size+1),
    "integerMatrix" = matrix(
      rgeom(size*(size+1), 0.5), nrow = size, ncol = size+1
    ),
    "logicalMatrix" = matrix(
      sample(c(TRUE, FALSE), size*(size+1), replace = TRUE),
      nrow = size, ncol = size+1
    ),
    "numericArray(nDim=3)" = array(rnorm(size*(size+1)*(size+2)), dim = size + 0:2),
    "integerArray(nDim=3)" = array(
      rgeom(size*(size+1)*(size+2), 0.5), dim = size + 0:2
    ),
    "logicalArray(nDim=3)" = array(
      sample(c(TRUE, FALSE), size*(size+1)*(size+2), replace = TRUE),
      dim = size + 0:2
    )
  )
  ## try again if argCheck returns FALSE
  if (!is.null(argCheck) && !argCheck(arg))
    return(make_input(argType, argCheck, size))
  else
    return(arg)
}

gen_nFunction <- function(param) {
  fun <- function() {}
  formals(fun) <- lapply(param$argTypes, function(x) quote(expr=))
  tmp <- quote({})
  tmp[[2]] <- param$expr
  tmp[[3]] <- quote(return(ans))
  body(fun) <- tmp
  return(
    nFunction(
      fun, argTypes = param$argTypes, returnType = param$returnType
    )
  )
}

## Runs test_math on a list of params.
test_math_batch <- function(batch, caseName = deparse(substitute(batch)), size = 3,
                            dir = file.path(tempdir(), "nCompiler_generatedCode"),
                            control = list(),
                            verbose = nOptions('verbose'),
                            skip = c()) {
  indices <- seq_along(batch)
  if (is.numeric(skip) && all(skip > 0 & skip %% 1 == 0))
    indices <- indices[-skip] ## skip is indices in batch to skip
  if (is.character(skip))
    skip <- names(batch) %in% skip ## convert to logical
  if (is.logical(skip) && length(skip) == length(indices))
    indices <- indices[!skip] ## skip[i] is TRUE if we should skip the corresponding test

  for (i in indices) {
    test_math(batch[[i]], paste0(caseName, ' #', i), size, dir, control, verbose)
  }
}

## This is a parametrized test, where `param` is a list with names:
##   param$name - A descriptive test name.
##   param$expr - A quoted expression `quote(out <- some_function_of(arg1, arg2, ...))`.
##   param$argTypes - A list of the input types.
##   param$returnType - The output type character string.
test_math <- function(param, caseName = 'test case', size = 3,
                      dir = file.path(tempdir(), "nCompiler_generatedCode"),
                      control = list(), verbose = nOptions('verbose')) {
  if (!is.list(param)) return(invisible(NULL))
  info <- paste0(caseName, ': ', param$name)

  ## in some cases, expect_error does not suppress error messages (I believe
  ## this has to do with how we trap errors in compilation), so make sure user
  ## realizes expectation
  if('knownFailureReport' %in% names(param) && param$knownFailureReport)
    cat("\nBegin expected error message:\n")

  test_that(info, {
    test_math_internal(param, info, size, dir, control, verbose)
  })

  invisible(NULL)
}

wrap_if_matches <- function(pattern, string, wrapper, expr) {
  if (!is.null(pattern) && any(grepl(paste0('^', pattern, '$'), string))) {
    wrapper(expr)
  } else {
    expr
  }
}

test_math_internal <- function(param, info, size = 3,
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

inverseCallReplacements <- as.list(
  names(nCompiler:::specificCallReplacements)
)
names(inverseCallReplacements) <- unlist(
  nCompiler:::specificCallReplacements
)
inverseReplace <- function(x) {
    replacement <- inverseCallReplacements[[x]]
    if(is.null(replacement)) x else replacement
}

modifyOnMatch <- function(x, pattern, key, value, negMatch = FALSE, env = parent.frame(), ...) {
  ## Modify any elements of a named list that match pattern.
  ##
  ## @param x A named list.
  ## @param pattern A regex pattern to compare with `names(x)`.
  ## @param key The key to modify in any lists whose names match `pattern`.
  ## @param value The new value for `key`.
  ## @param env The environment in which to modify `x`.
  ## @param ... Additional arguments for `grepl`.
  for (name in names(x)) {
    if (negMatch) isMatch <- !grepl(pattern, name, ...)
    else          isMatch <-  grepl(pattern, name, ...)
    if (isMatch)
      eval(substitute(x[[name]][[key]] <- value), env)
  }
}

getMatchingOps <- function(field, key, value) {
  ## Returns vector of operator names where a given field has
  ## a key with a given value, or an empty vector if no matches.
  ops <- ls(nCompiler:::operatorDefEnv)
  values <- unlist(
    sapply(ops, nCompiler:::getOperatorDef, field, key)
  )
  names(values)[values == value]
}

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
  outputHandling <- nCompiler:::getOperatorDef(
                                  op, 'labelAbstractTypes', 'returnTypeCode'
                                )
  if (is.null(outputHandling)) outputType <- argType
  else {
    argSym <- nCompiler:::argType2symbol(argType)
    type <- switch(
      outputHandling,
      'numeric', ## 1
      'integer', ## 2
      'logical', ## 3
      argSym$type,   ## 4
      if (grepl('logical', argType)) 'integer' else argSym$type ## 5
    )
    if (type == 'double') type <- 'numeric'
    handler <- nCompiler:::getOperatorDef(
                             op, 'labelAbstractTypes', 'handler'
                           )
    nDim <- if (!is.null(handler) && handler == 'UnaryReduction') 0
            else argSym$nDim
    dimString <- switch(
      nDim + 1,
      'Scalar', ## nDim is 0
      'Vector', ## nDim is 1
      'Matrix', ## nDim is 2
      'Array(nDim=3)' ## nDim is 3
    )
    outputType <- paste0(type, dimString)
  }

  list(
    name = paste(name, argType),
    expr = substitute(
      ans <- FOO(arg1), list(FOO = as.name(inverseReplace(op)))
    ),
    argTypes = list(arg1 = argType),
    returnType = outputType
  )
}

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
test_math_batch(cWiseUnaryTests, 'cWiseUnaryOps', verbose = TRUE)

###################
## cWiseBinaryTests
###################

getBinaryArgChecks <- function(op) {
  switch(
    op,
    #'/' = list(NULL, function(x) {
    #  if (length(x) == 1) x != 0
    #  else TRUE
    #},
    list(NULL, NULL)
  )
}

makeBinaryCwiseTest <- function(name, op, argTypes) {
  arg1 <- nCompiler:::argType2symbol(argTypes[1])
  arg2 <- nCompiler:::argType2symbol(argTypes[2])

  ## if args have non-matching nDim > 1 then they are non-conformable
  ## if (arg1$nDim > 1 && arg2$nDim > 1 && arg1$nDim != arg2$nDim)
  ##   return(NULL)

  outputHandling <-
    nCompiler:::getOperatorDef(op, 'labelAbstractTypes', 'returnTypeCode')
  if (is.null(outputHandling)) outputType <- argTypes[1]
  else {
    type <- switch(
      outputHandling,
      'numeric', ## 1
      'integer', ## 2
      'logical', ## 3
      nCompiler:::arithmeticOutputType(arg1$type, arg2$type), ## 4
      if (all(grepl('logical', argTypes))) 'integer' ## 5a
      else nCompiler:::arithmeticOutputType(arg1$type, arg2$type) ## 5b
    )

    ## arithmeticOutputType might return 'double'
    if (type == 'double') type <- 'numeric'

    nDim <- max(arg1$nDim, arg2$nDim)
    dimString <- switch(
      nDim + 1,
      'Scalar', ## nDim is 0
      'Vector', ## nDim is 1
      'Matrix', ## nDim is 2
      'Array(nDim=3)' ## nDim is 3
    )
    outputType <- paste0(type, dimString)
  }

  list(
    name = paste(name, argTypes[1], argTypes[2]),
    expr = substitute(
      ans <- FOO(arg1, arg2), list(FOO = as.name(inverseReplace(op)))
    ),
    argTypes = list(arg1 = argTypes[1], arg2 = argTypes[2]),
    argChecks = getBinaryArgChecks(op),
    returnType = outputType
  )
}

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

test_math_batch(cWiseBinaryTests, verbose = TRUE)
