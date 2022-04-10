# First steps towards a bridge from nimbleFunction to nFunction and nClass
#
# This file contains initial sketches of how we can maintain backward
# compatibility for nimbleFunctions and divert their compilation pathway
# to nFunctions and nClasses.  It also contains comments to try to explain
# some nimble internals.

library(nimble)

# What we call a "simple" nimbleFunction is one with no setup code.
# This is converted into a C++ function.
# In nimble internals, a simple nimbleFunction is called an RCfunction.
# Toy example:
nimFoo <- nimbleFunction(
  run = function(x = double(1)) {
    y = sum(x)
    return(y)
    returnType(double())
  }
)

# I see two pathways to convert this to an nCompiler::nFunction.
# Option 1: Extract the run function from nimFoo without using
# any of nimble's compilation steps at all.  Put that into a
# nFunction.  This should work because the basic type declarations
# are already backward(i.e. nimble)-compatible.
# This will need to be updated for predefined types.

# Prototype of what this will turn out to be:
nCfoo <- nFunction(
  fun = function(x = double(1)) {
    y = sum(x)
    return(y)
    returnType(double())
  }
)

# Demo that it compiles
CnCfoo <- nCompile(nCfoo)

# Example of extracting the code for this from nimFoo.
# The internals are stored in an "nfMethodRC" 
# that is in the closure of nimFoo
environment(nimFoo)$nfMethodRCobject
# The information maps easily into an nFunction
nfMethodRCobj <- environment(nimFoo)$nfMethodRCobject
fun <- function() {}
body(fun) <- nfMethodRCobj$code
formals(fun) <- nfMethodRCobj$argInfo
nCfoo2 <- nFunction(
  name = nfMethodRCobj$uniqueName, # This might be helpful
  fun = fun,
  returnType = nfMethodRCobj$returnType
)
CnCfoo2 <- nCompile(nCfoo2)
CnCfoo2(1:10)

RCfun_2_nFun <- function(RCfun) {
  nfMethodRCobj <- environment(RCfun)$nfMethodRCobject
  fun <- function() {}
  body(fun) <- nfMethodRCobj$code
  formals(fun) <- nfMethodRCobj$argInfo
  nFun <- nFunction(
    name = nfMethodRCobj$uniqueName, # This might be helpful
    fun = fun,
    returnType = nfMethodRCobj$returnType
  )
  nFun
}

compile_RCfun_as_nFun <- function(RCfun, dirName = tempdir()) {
  nFun <- RCfun_2_nFun(RCfun)
  CnFun <- nCompile(nFun) # Something is broken about the dir argument.
  CnFun
}

CnFoo3 <- compile_RCfun_as_nFun(nimFoo)

# Option 2: Use the first steps of nimble's compilation
# process, stopping before generating C++ code.

nimProj <- nimble:::nimbleProjectClass( tempdir(), "my_nimProj")
RCfunProc <- nimProj$compileRCfun(nimFoo, initialTypeInference = TRUE)                               
ls(RCfunProc)

# I am not going to develop this just now, waiting to see 
# how necessary might become.

###############################


test_math_via_nCompiler <- function(param, caseName, verbose = nimbleOptions('verbose'), size = 3, dirName = NULL) {
  info <- paste0(caseName, ': ', param$name)
  ## in some cases, expect_error does not suppress error messages (I believe this has
  ## to do with how we trap errors in compilation), so make sure user realizes expectation
  if('knownFailureReport' %in% names(param) && param$knownFailureReport)
    cat("\nBegin expected error message:\n")
  test_that(info, {
    ## wrap_if_matches(param$xfail, paste0(info, ': compiles and runs'), expect_error, {
    test_math_internal_via_nCompiler(param, info, verbose, size, dirName)
    ## })
  })
  if('knownFailureReport' %in% names(param) && param$knownFailureReport)
    cat("End expected error message.\n")
  invisible(NULL)
}

test_math_internal_via_nCompiler <- function(param, info, verbose = nimbleOptions('verbose'), size = 3, dirName = NULL) {
  if(verbose) cat("### Testing", param$name, "###\n")
  nArgs <- length(param$inputDim)
  logicalArgs <- rep(FALSE, nArgs)
  if("logicalArgs" %in% names(param))
    logicalArgs <- param$logicalArgs
  returnType <- "double"
  if("returnType" %in% names(param))
    returnType <- param$returnType
  
  runFun <- gen_runFun(param, logicalArgs, returnType)
  wrap_if_matches(param$expectWarnings, "builds", expect_warning, {
    nfR <- nimbleFunction(  
      run = runFun)
  })
  
  info <- paste0(info, ": compiles")
  ## need expect_error not expect_failure(expect_something()) because otherwise
  ## R error will stop execution
  wrap_if_matches(param$knownFailure, info, expect_error, {
    nfC <- compile_RCfun_as_nFun(nfR, dirName = dirName)
    
    arg1 <- make_input(param$inputDim[1], size = size, logicalArgs[1])
    if(nArgs > 1)
      arg2 <- make_input(param$inputDim[2], size = size, logicalArgs[2])
    if(nArgs > 2)
      arg3 <- make_input(param$inputDim[3], size = size, logicalArgs[3])
    if(nArgs > 3)
      stop("test_math not set up for >3 args yet")
    
    if("Rcode" %in% names(param)) {      
      eval(param$Rcode)
    } else {
      eval(param$expr)
    }
    info <- paste0(info, ": runs")
    wrap_if_matches(param$knownFailure, info, expect_failure, {
      if(nArgs == 3) {
        expect_silent(out_nfR <- nfR(arg1, arg2, arg3))
        expect_silent(out_nfC <- nfC(arg1, arg2, arg3))
      }  
      if(nArgs == 2) {
        expect_silent(out_nfR <- nfR(arg1, arg2))
        expect_silent(out_nfC <- nfC(arg1, arg2))
      }
      if(nArgs == 1) {
        expect_silent(out_nfR <- nfR(arg1))
        expect_silent(out_nfC <- nfC(arg1))
      }
      
      attributes(out) <- attributes(out_nfR) <- attributes(out_nfC) <- NULL
      
      infoR <- paste0(info, ": R vs Nimble DSL")
      wrap_if_matches(param$knownFailure, infoR, expect_failure, {
        expect_equal(out, out_nfR, info = infoR)
      })
      infoC <- paste0(info, ": R vs Nimble Cpp")
      wrap_if_matches(param$knownFailure, infoC, expect_failure, {
        expect_equal(out, out_nfC, info = infoC)
      })
    })
  })
  invisible(NULL)
}

source(system.file(file.path('tests', 'testthat', 'test_utils.R'), package = 'nimble'))
options(warn = 0)
nimbleOptions(verbose = FALSE)
source(system.file(file.path('tests', 'testthat', 'mathTestLists.R'), package = 'nimble'))

set.seed(0)
debug(test_math_internal_via_nCompiler)
ans1 <- sapply(testsVaried, test_math, 'math')    ## 12 # All pass!
ans2 <- sapply(testsBasicMath, test_math_via_nCompiler, 'math') ## 70 # nimStep is first failure
ans3 <- sapply(testsMoreMath, test_math, 'math')  ## 41 # All pass!
ans4 <- sapply(testsReduction, test_math, 'math') ## 13 # All pass!
ans5 <- sapply(testsComparison, test_math, 'math')## 12 # All pass!
ans6 <- sapply(testsMatrix, test_math, 'math')    ## 19 # All pass!
