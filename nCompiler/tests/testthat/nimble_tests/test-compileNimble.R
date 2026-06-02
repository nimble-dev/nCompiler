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
  Cnf1 <- nCompiler:::compileNimble(nf1)
  expect_identical(Cnf1$x, 1:2)

  nf <- nimbleFunction(
      setup = function() {x <- 2:3},
      run = function(mult = double(0)) {
          return(myfun(mult)); returnType(double())
      },
      methods = list(
          myfun = function(mult = double(0)) {
              return(x[1]*mult); returnType(double())
          }
      )
  )
  nf1 <- nf()
  Cnf1 <- nCompiler:::compileNimble(nf1)
  expect_identical(Cnf1$run(3), 6)
})
  
test_that("compileNimble bridge works for two nimbleFunction objects", {
  nf1 <- nimbleFunction(
      run = function(x = double(0)) {
          return(nf2(x))
          returnType(double(0))
      })
  nf2 <- nimbleFunction(
      run = function(x = double(0)) {
          return(3*x)
          returnType(double(0))
      })
  Cnfs <- nCompiler:::compileNimble(nf1,nf2)
  expect_identical(Cnfs[[1]](5), 15)
 
})

## NEXT STEPS:
## get a custom handler working
## try a test file from nimble using nClass
## add nClass to nCompiler:::compileNimble
##
## document, document, document


test <- nClass(
  Cpublic = list(
    x = nTypeBasic(name = "x", scalarType = "integer", nDim = 1)
  )
)
ctest <- nCompile(test)
