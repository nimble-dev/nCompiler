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

  # to do:
  # - add a "modify" list of inputs whose modified
  # values will be copied back into C++
  # - check that various types work,
  #  includes variables that were arguments
  # - work to make "self" work (will take some care)

})
