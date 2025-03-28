## Support for bridging nimble's compileNimble to nCompile
## Only basic tests will be here.
## The real tests will be running nimble's test suite.

library(nimble)
library(nCompiler)
library(testthat)

test_that("compileNimble bridge works for simple nimbleFunction (RC function)",{
  RCF1 <- nimbleFunction(
    run = function(x = double(1)) {
      ans <- sum(x)
      return(ans)
      returnType(double())
    }
  )
  CRCF1 <- nCompiler:::compileNimble(RCF1)
  expect_equal(CRCF1(1:3), 6)
})

test_that("compileNimble bridge works for one nimbleFunction object", {
  nf <- nimbleFunction(
    setup = function() {x <- 1:2},
    run = function() {return(x[1]); returnType(double())}
  )
  nf1 <- nf()
  Cnf1 <- compileNimble(nf1)
  expect_identical(Cnf1$x, 1:2)
})
## NEXT STEPS:
## get a custom handler working
## try a test file from nimble using nClass
## add nClass to nCompiler:::compileNimble
##
## document, document, document

test_that("registering a user-defined operator definition (opDef) works", {
  ## first version: provide a function
  nimArrayHandler <- function(code,...) {
    code$name <- 'nArray'
    NULL
  }
  nCompiler:::registerOpDef(
    list(nimArray =
           list(
             matchDef = function(value=0, dim=c(1,1), init=TRUE,
                                 fillZeros=TRUE, recycle=TRUE, nDim,
                                 type="double") {},
             #    normalizeCalls=list(handler='skip'),
             simpleTransformations=list(handler = nimArrayHandler))))
  expect_equal(ls(nCompiler:::operatorDefUserEnv), "nimArray")

  nCompiler:::registerOpDef(
    list(nimArray2 =
           list(
             matchDef = function(value=0, dim=c(1,1), init=TRUE,
                                 fillZeros=TRUE, recycle=TRUE, nDim,
                                 type="double") {},
             simpleTransformations=list(handler = 'replace',
                                        replacement = 'nArray'))))
  expect_equal(ls(nCompiler:::operatorDefUserEnv), c("nimArray", "nimArray2"))

  nc <- nClass(
    Cpublic = list(
      foo = nFunction(
        function() {
          ans <- nimArray( 6, dim = 2)
          ans2 <- nArray(value = 5, dim = 2)
          return(ans)
          returnType('double(1)')
        }
      ),
      foo2 = nFunction(
        function() {
          ans <- nimArray2(3,dim = 2)
          return(ans)
          returnType('double(1)')
        })
      ))
  Cnc <- nCompile(nc)
  obj <- Cnc$new()
  expect_identical(obj$foo(), c(6, 6))
  expect_identical(obj$foo2(), c(3, 3))
  rm(obj); gc()
  #
  nCompiler:::deregisterOpDef("nimArray")
  nCompiler:::deregisterOpDef("nimArray2")
  expect_equal(length(ls(nCompiler:::operatorDefUserEnv)), 0)
})
