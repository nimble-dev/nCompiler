# Test providing separate R_fun and C_fun in an nFunction
# to fully separate compiled and uncompiled behavior.

test_that("C_fun works in an nFunction", {
  # Key points
  # 1. argTypes and returnType refer to the C_fun is provided.
  # 2. The args of fun are untyped and may even differ from C_fun, if C_fun is provided.
  # 3. By default, changeKeywords does not happen, so "nStep" instead of "step"
  foo <- nFunction(
    fun = function(x) {
      x+1
    },
    compileInfo = list(
      C_fun = function(x, y, z) {
        ans <- x + y + nStep(z)
        return(ans)
      }
    ),
    argTypes = list(x='numericVector',y='numericScalar', z='numericScalar'),
    returnType = 'numericVector'
  )

  expect_equal(foo(3), 4)
  cfoo <- nCompile(foo)
  expect_equal(cfoo(3:4, 100, 1.2), (103:104) + 1)
})

test_that("C_fun errors out in an nFunction from not changing keywords ", {
  # Now expect and error if "step" was used
  foo <- nFunction(
    fun = function(x) {
      x+1
    },
    compileInfo = list(
      C_fun = function(x, y, z) {
        ans <- x + y + step(z)
        return(ans)
      }
    ),
    argTypes = list(x='numericVector',y='numericScalar', z='numericScalar'),
    returnType = 'numericVector'
  )

  cat("Expected error message about no op def for step.\n")
  expect_error(cfoo <- nCompile(foo))
})

test_that("C_fun works in an nFunction with changeKeywords=TRUE", {
  # Now step is ok b/c control$changeKeywords is TRUE
  foo <- nFunction(
    fun = function(x) {
      x+1
    },
    compileInfo = list(
      C_fun = function(x, y, z) {
        ans <- x + y + step(z)
        return(ans)
      }),
    control = list(
      changeKeywords=TRUE
    ),
    argTypes = list(x='numericVector',y='numericScalar', z='numericScalar'),
    returnType = 'numericVector'
  )
  expect_equal(foo(3), 4)
  cfoo <- nCompile(foo)
  expect_equal(cfoo(3:4, 100, 1.2), (103:104) + 1)
})

test_that("C_fun works in an nClass method", {
  foo <- nFunction(
    fun = function(x) {
      x+1
    },
    compileInfo = list(
      C_fun = function(x, y, z) {
        ans <- x + y + step(z)
        return(ans)
      }),
    control = list(
      changeKeywords=TRUE
    ),
    argTypes = list(x='numericVector',y='numericScalar', z='numericScalar'),
    returnType = 'numericVector'
  )
  fooClass <- nClass(
    Cpublic=list(foo=foo)
  )
  obj <- fooClass$new()
  expect_equal(obj$foo(3), 4)
  cfooClass <- nCompile(fooClass)
  cobj <- cfooClass$new()
  expect_equal(cobj$foo(3:4, 100, 1.2), (103:104) + 1)
})
