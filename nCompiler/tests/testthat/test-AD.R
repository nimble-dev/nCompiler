context("Testing Automatic Differentiation")

source('testing_utils.R')

test_that("compileNimbleClass works (with AD for a scalar)", {
  library(nCompiler)
  nc1 <- nClass(
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cfoo = nFunction(
        fun = function(x) {
          return(1.5*x+1)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar')
    ),
    enableDerivs = 'Cfoo'
  )
  set_nOption('automaticDerivatives', TRUE)
  ans <- try(nCompiler:::nCompile_nClass(nc1, interface = "generic"))
  expect_true(is.function(ans)) ## compilation succeeded
  obj <- ans()
  expect_true(nCompiler:::is.loadedObjectEnv(obj))
  expect_equal(method(obj, "Cfoo")(1.32), 1.5*1.32 + 1)
  derivs <- nDerivs(method(obj, "Cfoo")(1.32), NC = nc1)
  expect_true(nCompiler:::is.loadedObjectEnv(derivs))
  expect_equal(value(derivs, "gradient"), array(1.5, dim = c(1, 1)))
})

test_that("compileNimbleClass works (with AD for a vector)", {
  library(nCompiler)
  nc1 <- nClass(
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cfoo = nFunction(
        fun = function(x) {
          ans <- 1.5 *x
          return(ans)
        },
        argTypes = list(x = 'numericVector(length = 2)'),
        returnType = 'numericVector(length = 2)')
    ),
    enableDerivs = 'Cfoo'
  )
  set_nOption('automaticDerivatives', TRUE)
  ans <- try(nCompiler:::nCompile_nClass(nc1, interface = "generic"))
  expect_true(is.function(ans)) ## compilation succeeded
  obj <- ans()
  expect_true(nCompiler:::is.loadedObjectEnv(obj))
  expect_equal(method(obj, "Cfoo")(c(1.48, 1.52)), array(1.5*c(1.48, 1.52), dim = 2))
  derivs <- nDerivs(method(obj, "Cfoo")(c(1.48, 1.52)), NC = nc1)
  expect_true(nCompiler:::is.loadedObjectEnv(derivs))
  expect_equal(value(derivs, "gradient"), diag(c(1.5, 1.5)))
})

test_that("AD with indexing works (with AD for a matrix)", {
  library(nCompiler)
  nc1 <- nClass(
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cfoo = nFunction(
        fun = function(x) {
          ans <- 1.5 * exp(x[2:5])^2
          return(ans)
        },
        argTypes = list(x = 'numericVector(length = 7)'),
        returnType = 'numericVector(length = 4)')
    ),
    enableDerivs = 'Cfoo'
  )
  set_nOption('automaticDerivatives', TRUE)
  ans <- try(nCompiler:::nCompile_nClass(nc1, interface = "generic"))
  expect_true(is.function(ans)) ## compilation succeeded
  obj <- ans()
  expect_true(nCompiler:::is.loadedObjectEnv(obj))
  input <- c(0.9861515, 0.3756302, 1.0109309, 0.5898856, 0.3941390, 1.1837561, -2.2858289)
  expect_equal(method(obj, "Cfoo")(input), array(1.5*exp(input[2:5])^2, dim = 4))
  derivs <- nDerivs(method(obj, "Cfoo")(input), NC = nc1)
  expect_true(nCompiler:::is.loadedObjectEnv(derivs))
  gradient <- matrix(0, nrow = 4, ncol = 7)
  gradient[, 2:5] <- diag(3*exp(input[2:5])^2)
  expect_equal(value(derivs, "gradient"), gradient)
})

test_AD <- function(param, info = '', size = 3,
                    dir = file.path(tempdir(), "nCompiler_generatedCode"),
                    control = list(), verbose = nOptions('verbose'),
                    debug = nOptions('compilerOptions')[['debug']],
                    compile_all_funs = FALSE, ...) {
  if (!is.null(param$debug) && param$debug || debug) browser()

  nC <- gen_nClass(param)
  set_nOption('automaticDerivatives', TRUE)

  if (compile_all_funs) {
    nFuns <- param$Cpublic
    ## useful for debugging an nClass compilation failure
    ## e.g. test_AD(ADopTests[['- numericVector']], compile_all_funs = TRUE)
    for (name in names(nFuns)) {
      if (verbose)
        cat(paste('### Compiling function for test of:', name, '###\n'))
      nCompile_nFunction(nFuns[[name]])
    }
  }

  info <- paste0(info, ": compiles")
  ## need expect_error not expect_failure(expect_something()) because otherwise
  ## R error will stop execution
  wrap_if_matches(param$knownFailure, info, expect_error, {
    args <- sapply(param$argTypes, make_input, size = size, USE.NAMES = FALSE)
    if (verbose) cat("## Compiling nClass ##\n")
    if (!is.null(param$dir)) dir <- param$dir
    compiled_nC <- nCompile_nClass(nC, dir = dir, control = control, interface = "generic")
    expect_true(is.function(compiled_nC)) ## compilation succeeded

    info <- paste0(info, ": runs")
    wrap_if_matches(param$knownFailure, info, expect_failure, {
      obj <- compiled_nC()
      expect_true(nCompiler:::is.loadedObjectEnv(obj))
      for (i in seq_along(param$enableDerivs)) {
        fun_i <- param$enableDerivs[i]
        nF_i <- nC$public_methods[[fun_i]]
        expect_equal(method(obj, fun_i)(args[i]), nF_i(args[i]))
        derivs <- nDerivs(method(obj, paste0(fun_i, "_derivs_"))(args[i]), NC = nC)
        expect_true(nCompiler:::is.loadedObjectEnv(derivs))
        value(derivs, "gradient")
        ## expect_equal(value(derivs, "gradient"), )
      }
      if (verbose) cat("### -------------------------------------------- ###\n")

    })
  })
  invisible(NULL)
}

## derivatives only available for scalar and vector inputs
argTypes <- c('numericScalar', 'numericVector')

makeADtest <- function(op, argType, size = 3) {
  opParam <- makeOperatorParam(op, argType)
  name <- opParam$name
  isBinary <- nCompiler:::getOperatorDef(op, 'testthat', 'isBinary')
  isUnary <- nCompiler:::getOperatorDef(op, 'testthat', 'isUnary')
  if (!is.null(isBinary) && isBinary) {
    binaryOpParam1 <- binaryOpParam2 <- opParam
    tmp <- binaryOpParam2$expr[[3]][[2]]
    ## put in a constant as the other arg
    binaryOpParam1$expr[[3]][[3]] <- binaryOpParam2$expr[[3]][[2]] <- 1.5
    ## put back in variable arg as second arg in binaryOpParam2
    binaryOpParam2$expr[[3]][[3]] <- tmp
    name1 <- paste(name, 1.5)
    binaryOpParam1$name <- name1
    name2 <- paste(op, 1.5, argType)
    binaryOpParam2$name <- name2
    opParams <- list(binaryOpParam1, binaryOpParam2)
  }
  if (!is.null(isUnary) && isUnary) {
    if (exists('opParams', inherits = FALSE)) opParams[[3]] <- opParam
    else opParams <- list(opParam)
  }
  if (!exists('opParams', inherits = FALSE))
    stop(
      paste0('Something\'s wrong... Operator ', op,
             ' is neither unary nor binary.'),
      call. = FALSE
    )
  nFuns <- lapply(opParams, function(param) {
    if (param$argTypes$arg1 == 'numericVector') {
      typeString <- paste0('numericVector(length = ', size, ')')
      param$argTypes$arg1 <- typeString
      param$returnType <- typeString
    }
    nFun <- gen_nFunction(param)
    nFun
  })
  names(nFuns) <- paste0('nFun', 1:length(nFuns))
  list(
    name = name,
    argTypes = rep(argType, length(nFuns)),
    Rpublic = list(),
    Cpublic = nFuns,
    enableDerivs = names(nFuns)
  )
}

ADops <- getMatchingOps('testthat', 'testAD', TRUE)

ADopTests <- unlist(
  recursive = FALSE,
  x = lapply(
    ADops,
    function(x) {
      mapply(
        makeADtest,
        argType = argTypes,
        MoreArgs = list(op = x),
        SIMPLIFY = FALSE
      )
    })
)
names(ADopTests) <- sapply(ADopTests, `[[`, 'name')

## tests with numericVector fail to compile for an unknown reason
modifyOnMatch(ADopTests, '.+ numericVector', 'knownFailure', '.* compiles')

## .method operators don't work with scalar input
modifyOnMatch(
  ADopTests,
  '(\\^|abs|atan|cube|exp|inverse|lgamma|log|logit|rsqrt|sqrt|square|tanh) numericScalar',
  'knownFailure', '.* compiles'
)

## test_batch(test_AD, ADopTests)
