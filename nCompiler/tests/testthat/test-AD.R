context("Testing Automatic Differentiation")

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

