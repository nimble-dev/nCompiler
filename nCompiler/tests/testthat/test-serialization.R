# not working
# Works when run directly, but not through test_package.
context("Test serialization with cereal package")

test_that("serialization works",
          {
            nc1 <- nClass(
              Rpublic = list(
                Rv = NULL,
                Rfoo = function(x) x+1
              ),
              Cpublic = list(
                Cv = 'numericScalar',
                Cx = 'integerScalar',
                Cfoo = nFunction(
                  fun = function(x) {
                    return(x+1)
                  },
                  argTypes = list(x = 'numericScalar'),
                  returnType = 'numericScalar'),
                Cbar = nFunction(
                  fun = function(x, y) {
                    return(x + y)
                  },
                  argTypes = list(x = 'numericMatrix',
                                  y = 'numericMatrix'),
                  returnType = 'numericMatrix')
              )
            )
            set_nOption('serialize', TRUE)
            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans[[1]])) ## compilation succeeded
            obj <- ans[[1]]()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)
            
            serialized <- nCompiler:::serialize_nComp_object(obj) 
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))
            
            deserialized <- nCompiler:::deserialize_nComp_object(serialized)
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))
            expect_equal(value(deserialized, "Cv"), 1.23)
            x <- matrix(as.numeric(1:6), nrow = 2)
            y <- matrix(as.numeric(101:106), nrow = 2)
            
            expect_equal(
              method(deserialized, "Cbar")(
                x, y
              ),
              x+y)
          })
