# Tests of nCompile's handling of interacting compilation units:
#
# nFunction calling nFunction
# nFunction creating nClass internally
# nFunction taking nClass argument
# nFunction returning nClass object
# nClass with methods doing each of the above
# nClass with member variable of another nClass
# nClass inheriting from another nClass (tested in test-nClass_inherit)
#
# There are three modes by which one compilation unit may correctly see another:
#  Have one .h include the other .h
#  Have one .h include a forward declaration for another class (but don't include its .h)
#  Have a .cpp include the other .h.
#
# When an nFunction is needed: have "my" .h include the "other" .h
# When an nClass is needed as argument or return type: Forward declare the class in my .h.
# When an nClass is needed for inheritance: Include the other .h
# When an nClass is my member variable: Forward declare the class in my .h (because we really represent
#   member variables using shared_ptr<>s, so a forward declaration should be sufficient).
# When an nClass is used internally to an nFunction (including an nClass method): Include the other .h in my .cpp.
#
# Here are some potential future challenges beyond the current work:
#  - if an nClass is manually included directly as a member variable (not via shared_ptr or basic ptr), we would
#    need to include the .h
#  - if a function or method is a template<>, the definition should appear in the .h, not the .cpp. (And
#    if it uses another nClass, the other .h would need to be included in the .h, not the .cpp).
#
# These tests also attempt to cover mixes of needing Eigen or not in one or the other part of
# the dependency.

library(nCompiler)
library(testthat)
#nOptions(showCompilerOutput=TRUE)
#nOptions(showCompilerOutput=FALSE)

test_that("#includes work when one nFunction calls another", {
  f1 <- nFunction(
    function(x = 'numericScalar') {
      return(x+1); returnType('numericScalar')
    }
  )
  f2 <- nFunction(
    function(x = 'numericVector') {
      x[1] <- f1(x[1])
      return(x[1]); returnType('numericScalar')
    }
  )
  comp1 <- nCompile(f1, f2)
  expect_equal(comp1$f1(2), 3)
  expect_equal(comp1$f2(2:4), 3)

  comp1 <- nCompile(f2, f1)
  expect_equal(comp1$f1(2), 3)
  expect_equal(comp1$f2(2:4), 3)

  comp1 <- nCompile(f1, f2, package=TRUE)
  expect_equal(comp1$f1(2), 3)
  expect_equal(comp1$f2(2:4), 3)

  comp1 <- nCompile(f2, f1, package=TRUE)
  expect_equal(comp1$f1(2), 3)
  expect_equal(comp1$f2(2:4), 3)

  # Now make f1 use Eigen while f2 doesn't
  f1 <- nFunction(
    function(x = 'numericVector') {
      return(x[1]+1); returnType('numericScalar')
    }
  )
  f2 <- nFunction(
    function() {
      x <- numeric(length = 3, value = 2)
      x[1] <- f1(x)
      return(x[1]); returnType('numericScalar')
    }
  )
  comp1 <- nCompile(f1, f2)
  expect_equal(comp1$f1(2:4), 3)
  expect_equal(comp1$f2(), 3)

  comp1 <- nCompile(f2, f1)
  expect_equal(comp1$f1(2:4), 3)
  expect_equal(comp1$f2(), 3)

  comp1 <- nCompile(f1, f2, package=TRUE)
  expect_equal(comp1$f1(2:4), 3)
  expect_equal(comp1$f2(), 3)

  comp1 <- nCompile(f2, f1, package=TRUE)
  expect_equal(comp1$f1(2:4), 3)
  expect_equal(comp1$f2(), 3)
})

test_that("#includes work when an nFunction creates an nClass internally (only)", {
  nc1 <- nClass(
    Cpublic = list(
      x = 'numericVector',
      foo = nFunction(
        function(v = 'numericScalar') {
        return(v + x[1]); returnType('numericScalar')
      })
    )
  )
  f1 <- nFunction(
    function(z = 'numericScalar') {
      my_nc1 <- nc1$new()
      my_nc1$x <- numeric(length = 3, value = 2)
      ans <- my_nc1$foo(z)
      return(ans); returnType('numericScalar')
    }
  )
  comp1 <- nCompile(nc1, f1)
  expect_equal(comp1$f1(3), 5)

  comp1 <- nCompile(f1, nc1)
  expect_equal(comp1$f1(3), 5)

  comp1 <- nCompile(nc1, f1, package = TRUE)
  expect_equal(comp1$f1(3), 5)

  comp1 <- nCompile(f1, nc1, package = TRUE)
  expect_equal(comp1$f1(3), 5)
})
