library(nCompiler)
library(testthat)

# tests below will not work for automated use yet
# because they go into browser()s
# I am putting a quit here so that if this
# file is called for automated testing, it will pass.
# We can remove this when we build tests that work.
q('no')

testthat("Simple Rbrowser call works", {
  # Check that we get into a browser
  # with view list containing x
  foo <- nFunction(
    fun = function() {
      x <- 1:5
      Rbrowser(view = "x")
    }
  )
  foo()
  cfoo <- nCompile(foo)
  cfoo()

  # Check that we get into a browser
  # with view list containing x and y
  foo <- nFunction(
    fun = function() {
      x <- 1:5
      y <- 6:10
      Rbrowser(view = c("x", "y"))
    }
  )
  foo()
  cfoo <- nCompile(foo)
  cfoo()

  # Check that we get into a browser
  # with view list containing input arguments
  foo <- nFunction(
    fun = function(y) {
      x <- 1:5
      Rbrowser(view = c("x", "y"))
    },
    argTypes = list(y = 'numericScalar')
  )
  foo()
  cfoo <- nCompile(foo)
  cfoo()
  
  # Check that we get into a browser
  # with view list containing vars of various types
  foo <- nFunction(
    fun = function(z = integerVector) {
      # w <- "hello"
      x <- nMatrix(type = 'integer', nrow = 2, ncol = 12, value = z)
      # y <- nArray(type = 'integer', dim = c(2,3,4), value = z)
      Rbrowser(view = c("x"))
    }
  )
  foo()
  cfoo <- nCompile(foo)
  cfoo(1:24)

  # check that can access member data in nClass object (awkwardly at the moment)
  nc1 <- nClass(
      classname = "nc1",
      Rpublic = list(
          Rv = 1,
          Rfoo = function(x) x+1
      ),
      Cpublic = list(
          Cv = 'numericScalar',
          Cfoo = nFunction(
              fun = function(x) {
                  Cv <- Cv + 100
                  Rbrowser(view = 'x') 
                  return(x+1)
              },
              argTypes = list(x = 'numericScalar'),
              returnType = 'numericScalar')
      )
  )
  cnc1 <- nCompile(nc1)
  obj <- cnc1$new()
  obj$Cv <- 7
  obj$Cfoo(5)
  ## Use `get('obj')$Cv` to see member data.
  ## Use `local_obj <- get('obj'); local_obj$Cv <- 1000` to modify.

  
  # to do:
  # - add a "modify" list of inputs whose modified
  # values will be copied back into C++
  # - check that various types work (in progress above)
  # - work to make "self" work (will take some care)

})
