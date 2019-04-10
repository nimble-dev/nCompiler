## not working
context("Testing Automatic Differentiation")

source('testing_utils.R')

test_that("compileNimbleClass works (with AD for a scalar)",
          {
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
            derivs <- method(obj, "Cfoo_derivs_")(1.32, c(0, 1))
            expect_true(nCompiler:::is.loadedObjectEnv(derivs))
            expect_equal(value(derivs, "gradient"), array(1.5, dim = c(1, 1)))
          })

test_that("compileNimbleClass works (with AD for a vector)",
          {
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
            derivs <- method(obj, "Cfoo_derivs_")(c(1.48, 1.52), c(0, 1))
            expect_true(nCompiler:::is.loadedObjectEnv(derivs))
            expect_equal(value(derivs, "gradient"), diag(c(1.5, 1.5)))
          })

test_AD <- function(param, info, size = 3,
                    dir = file.path(tempdir(), "nCompiler_generatedCode"),
                    control = list(), verbose = nOptions('verbose')) {
  if (!is.null(param$debug) && param$debug) browser()

  nC <- gen_nClass(param)
  set_nOption('automaticDerivatives', TRUE)

  info <- paste0(info, ": compiles")
  ## need expect_error not expect_failure(expect_something()) because otherwise
  ## R error will stop execution
  wrap_if_matches(param$knownFailure, info, expect_error, {
    argsR <- lapply(param$argType, make_input, size = size)
    argsC <- argsR
    names(argsC) <- nCompiler:::mangleArgumentNames(names(argsC))

    if (verbose) cat("## Compiling nClass ##\n")
    if (!is.null(param$dir)) dir <- param$dir
    compiled_nC <- nCompile_nClass(nC, dir = dir, control = control, interface = "generic")
    expect_true(is.function(compiled_nC)) ## compilation succeeded

    info <- paste0(info, ": runs")
    wrap_if_matches(param$knownFailure, info, expect_failure, {
      ## TODO: find source of the following error
      ## "Cannot convert object to an environment: [type=character; target=ENVSXP]."
      ## obj <- compiled_nC()
      ## expect_true(nCompiler:::is.loadedObjectEnv(obj))
      if (verbose) cat("### -------------------------------------------- ###\n")

    })
  })
  invisible(NULL)
}

## derivatives only available for scalar and vector inputs
argTypes <- c('numericScalar', 'numericVector')

makeADtest <- function(op, argType) {
  opParam <- makeOperatorParam(op, argType)
  name <- opParam$name
  isBinary <- nCompiler:::getOperatorDef(op, 'testthat', 'isBinary')
  if (!is.null(isBinary) && isBinary) {
    ## put in a constant as second arg
    opParam$expr[[3]][[3]] <- 1.5
    name <- paste(name, 1.5)
  }
  nFun <- gen_nFunction(opParam)
  if (slot(nFun, 'internals')$returnSym$nDim == 1) {
    slot(nFun, 'internals')$argSymTab$setSymbolField('arg1', 'size', 3)
    slot(nFun, 'internals')$returnSym$size <- 3
  }
  list(
    name = name,
    argTypes = argType,
    Rpublic = list(),
    Cpublic = list(
      Cfun = nFun
    ),
    enableDerivs = 'Cfun'
  )
}

## TODO: move this info to compile_aaa_operatorLists.R
ADops <- c(
  '/', '*', '^', 'exp', 'log', 'sqrt', 'square', 'cube', 'tanh',
  'atan'
)
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

test_batch(test_AD, ADopTests, verbose = TRUE)
