context("Calling from one nFunction to another")

test_that("One nFunction can call another and be compiled.",
          { 
            f1 <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- x + y
                return(ans)
              }
            )
            f2 <- nFunction(
              fun = function(x = double(0),
                             y = double(0)) {
                returnType(double(0))
                ans <- f1(x, y)
                return(ans)
              }
            )
            ## debug(nCompiler:::compile_labelAbstractTypes)
            test <- nCompile(f2, f1)
          })

test_that("One nFunction can call another with non-scalar and be compiled.",
          { 
            f1 <- nFunction(
              fun = function(x = double(1),
                             y = double(1)) {
                returnType(double(1))
                ans <- x + y
                return(ans)
              }
            )
            f2 <- nFunction(
              fun = function(x = double(1),
                             y = double(1)) {
                returnType(double(1))
                ans <- f1(x, y)
                return(ans)
              }
            )
            ## debug(nCompiler:::compile_labelAbstractTypes)
            test <- nCompile(f2, f1)
          })

