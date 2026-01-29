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
  # with update list containing x (an argument)
  foo <- nFunction(
      fun = function(x = double(1)) {
        Rbrowser(update = c("x"))
        return(x)
        returnType(double(1))
      }
  )
  foo(1:5)
  cfoo <- nCompile(foo)
  cfoo(1:5)

  # Check that we get into a browser
  # with update list containing x (an ref argument)
  foo <- nFunction(
    fun = function(x = double(1)) {
      Rbrowser(update = c("x"))
      return(x)
      returnType(double(1))
    },
    refArgs = "x"
  )
  message("currently a ref arg in Rbrowser works in compiled but not uncompiled. The name x_Ref__ is not used")
  local_x <- 1:5
  foo(local_x)
  cfoo <- nCompile(foo)
  local_x <- 1:5
  cfoo(local_x)

  foo <- nFunction(
    fun = function(x = double(1)) {
      Rbrowser(update = c("x"))
      return(x)
      returnType(double(1))
    },
    blockRefArgs = "x"
  )
  message("currently a blockRefArg in Rbrowser fails in both uncompiled and compiled (does not compile in C++)")
  local_x <- 1:5
  foo(local_x[2:4])
  cfoo <- nCompile(foo)
  local_x <- 1:5
  cfoo(local_x)


  # Check that we get into a browser
  # with update list containing x (an argument)
  # and y (a local variable)
  foo <- nFunction(
    fun = function(x = double(1)) {
      y <- 1:5
      Rbrowser(update = c("x", "y"))
      return(c(x, y))
      returnType(double(1))
    }
  )
  foo(1:5)
  cfoo <- nCompile(foo)
  cfoo(1:5)


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
  # with view list containing x and y
  # after a nested call
  # Note that the "Called from" message from
  # compiled case looks like it will be the
  # last call from R itself, regardness of
  # calls among C++ functions.
  bar <- nFunction(
    fun = function() {
      foo()
    }
  )
  foo <- nFunction(
    fun = function() {
      x <- 1:5
      y <- 6:10
      Rbrowser(view = c("x", "y"))
    }
  )
  bar()
  comp <- nCompile(foo, bar)
  comp$bar()


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

  # Check that we can mix view and update variables
  foo <- nFunction(
    fun = function(y) {
      x <- 1:5
      Rbrowser(view = "x", update = "y")
      return(y)
      returnType('numericScalar')
    },
    argTypes = list(y = 'numericScalar')
  )
  foo(101)
  cfoo <- nCompile(foo)
  cfoo(101)

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



  # Check that we get into a browser
  # with view list containing x, y, and an nClass object
  nc1 <- nClass(
    Cpublic = list(
      z = 'numericMatrix'
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      my_nc1 = 'nc1',
      foo = nFunction(
        fun = function() {
          x <- 1:5
          y <- 6:10
          Rbrowser(view = c("x", "y", "my_nc1"))
        }
      )
    )
  )
  nc2obj <- nc2$new()
  nc2obj$my_nc1 <- nc1$new()
  nc2obj$foo() # check for x, y, my_nc1
  comp <- nCompile(nc1, nc2)
  Cnc2obj <- comp$nc2$new()
  Cnc2obj$my_nc1 <- comp$nc1$new()
  Cnc2obj$my_nc1$z <- matrix(1:4, nrow = 2)
  Cnc2obj$foo()


  # to do:
  # - add a "modify" list of inputs whose modified
  # values will be copied back into C++
  # - check that various types work (in progress above)
  # - work to make "self" work (will take some care)

})
