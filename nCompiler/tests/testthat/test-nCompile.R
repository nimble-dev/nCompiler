context("Testing nCompile")

test_that("Compile one nFunction via nCompile, returning a list.",
          { 
            addScalars <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- x + y
                return(ans)
              }
            )
            test <- nCompile(addScalars, returnList = TRUE)
            expect_equal(test$addScalars(2, 3), 5)
          })

test_that("Compile one nFunction via nCompile, returning a list.",
          { 
            addScalars <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- x + y
                return(ans)
              }
            )
            test <- nCompile(addScalars, returnList = FALSE)
            expect_equal(test(2, 3), 5)
          })

test_that("Compile two nFunctions via nCompile, returning a list.",
          { 
            addScalars <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- x + y
                return(ans)
              }
            )
            multScalars <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- x * y
                return(ans)
              }
            )
            test <- nCompile(addScalars, multScalars)
            expect_equal(test$addScalars(2, 3), 5)
            expect_equal(test$multScalars(2, 3), 6)
          })

test_that("Compile two nFunctions via nCompile provided as a list, returning a list.",
          { 
            addScalars <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- x + y
                return(ans)
              }
            )
            multScalars <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- x * y
                return(ans)
              }
            )
            debug(nCompiler:::nCompile)
            test <- nCompile(list(f1 = addScalars, f2 = multScalars))
            expect_equal(test$f1(2, 3), 5)
            expect_equal(test$f2(2, 3), 6)
          })

