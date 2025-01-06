## flex_(logical_scalar) = ... does not seem to work.
message("test-eigenShapeFlex of flex_() assignment system works but has only one case. More coverage is needed.")
test_that("flex_ system works for mean()",
          {
            #set_nOption("use_flexible_assignment", TRUE)
            meanX <- nFunction(
              fun = function(x) {
                ans <- mean(x)
                return(ans)
              },
              argTypes = list(x = 'numericVector()'),
              returnType = 'numericScalar()')
            
            x <- rnorm(10)
            expect_equal(meanX(x), mean(x), info = "uncompiled meanX works")
            CmeanX <- nCompile_nFunction(meanX)
            expect_equal(CmeanX(x), meanX(x), info = "uncompiled meanX works")
          }
)
