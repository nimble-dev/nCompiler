# Tests of passing by copy, reference, and block reference,
# from R-R, R-C, and C-C,
# with nFunctions and nClass methods,
# with different argument ordering,
# for different kinds of numeric objects

library(testthat)
#library(nCompiler)

message("More test coverage of argument passing cases is needed. See comments.")
# need cases of multiple function call layers
message("How does/should ref or blockRef work for scalars?")
message("doing scalar = vector + scalar does not error out if the vector in length>1.")
message("blockRef error trapping can be more involved -- using dims.")
message("blockRef cannot cross between scalar types")

cat("starting test-argumentPassing\n")

# This is a workaround to pkg_name::var.
# This is necessary because on GitHub Actions for testing, we use
# `setup-r-dependencies`. This **aggressively** checks **all** directories
# in the package structure and identifies **all** pkg::var code
# and then attempts to install a package called "pkg".
# Here we are dynamically generating packages, e.g. nc1Package,
# and then checking that they work. So we use access_dynamic_package,
# which internally uses get(), to avoid the `::` syntax.
access_dynamic_package <- function(pkg_name, var) {
  if (!isNamespaceLoaded(pkg_name)) {
    stop(paste("Dynamic package", pkg_name, "is not loaded"))
  }
  ns <- asNamespace(pkg_name)
  get(var, envir = ns, inherits = FALSE)
}

# Same rationale as above:
load_dynamic_namespace <- function(pkg_name) {
  eval(call("loadNamespace", pkg_name))
}

# compiled and uncompiled 1D by copy
# A lot of this is tested elsewhere, so here it is not thorough
# and is more of a warm-up test.
test_that("pass 1D by copy works (compiled & uncompiled)", {
  foo <- nFunction(
    function(x = numericVector()) {
      x <- x + 1
      return(x)
    },
    returnType = 'numericVector'
  )
  x <- 1:3
  y <- foo(x)
  expect_equal(x+1, y)
  cfoo <- nCompile(foo)
  x <- 1:3
  y <- cfoo(x)
  expect_equal(x+1, y)
})

####################
## 1D tests

# compiled and uncompiled 1D by ref
test_that("pass 1D by ref and blockRef works and error-traps (compiled & uncompiled) in nFunction", {
  message("This test has many trapped errors.")
  foo <- nFunction(
    function(x = numericVector(), xRef = numericVector(), xBlockRef = numericVector()) {
      xRef <- x[1:2] + 10
      x <- xRef + 100
      xBlockRef <- xRef + 1000
      return(x)
    },
    refArgs = 'xRef',
    blockRefArgs = 'xBlockRef',
    returnType = 'numericVector'
  )
  test_foo <- function(fn) {
    x <- as.numeric(1:3)
    xRef <- as.numeric(11:13)
    xBlockRef <- as.numeric(21:23)
    expect_error(fn(x, 11:13, xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, 11:13))      # Can't pass literal to blockRef
    expect_error(fn(x, xRef[1:3], blockRef)) # Can't pass block to ref
    y <- fn(x, xRef, xBlockRef[2:3])
    expect_equal(y, x[1:2] + 10 + 100)
    expect_equal(xRef, x[1:2] + 10)
    expect_equal(xBlockRef, c(21, 1:2 + 10 + 1000))
  }
  cfoo <- nCompile(foo)
  test_foo(foo)
  test_foo(cfoo)
  test_foo(cfoo) # Do twice to invoke byte-compiled version.
  # At one stage of development we were trying to be tricky
  # and grab information on the C++ side by reaching up into
  # the calling environment. This worked except only on the first call.
  # On the second call, R had jit-byte-compiled the function
  # and our then our tricky code failed. So we stopped being tricky in C++
  # and took another approach. However we still have
  # second-calls as regression tests on this problem.
  cfoo <- nCompile(foo, package=TRUE)
  test_foo(cfoo)
  test_foo(cfoo)
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing")
  test <- writePackage(foo, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, action = "prefix", code = load_dynamic_namespace("testpackage"))
  test_foo(access_dynamic_package("testpackage", "foo"))
  test_foo(access_dynamic_package("testpackage", "foo"))
})

test_that("pass 1D by ref and blockRef works and error-traps via nClass method (compiled & uncompiled)", {
  message("This test has four trapped errors.")

  #debug(nCompiler:::make_nClass_code)
  #undebug(nCompiler:::build_Cmethod_code_for_nClass)
  nc1 <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x = numericVector(), xRef = numericVector(), xBlockRef = numericVector()) {
          xRef <- x[1:2] + 10
          x <- xRef + 100
          xBlockRef <- xRef + 1000
          return(x)
        },
        refArgs = 'xRef',
        blockRefArgs = 'xBlockRef',
        returnType = 'numericVector'
      )))

  test_foo <- function(fn) {
    x <- as.numeric(1:3)
    xRef <- as.numeric(11:13)
    xBlockRef <- as.numeric(21:23)
    expect_error(fn(x, 11:13, xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, 11:13))      # Can't pass literal to blockRef
    expect_error(fn(x, xRef[1:3], blockRef)) # Can't pass block to ref
    y <- fn(x, xRef, xBlockRef[2:3])
    expect_equal(y, x[1:2] + 10 + 100)
    expect_equal(xRef, x[1:2] + 10)
    expect_equal(xBlockRef, c(21, 1:2 + 10 + 1000))
  }
  # uncompiled
  obj <- nc1$new()
  test_foo(obj$foo)
  # compiled directly
  Cnc1 <- nCompile(nc1, package = FALSE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()
  # Compiled via package
  Cnc1 <- nCompile(nc1, package = TRUE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()
  # Compiled via package via writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing")
  test <- writePackage(nc1, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, action = "prefix", code = load_dynamic_namespace("testpackage"))
  Cobj <- access_dynamic_package("testpackage", "nc1")$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()
})

####################
## 2D tests

# compiled and uncompiled 2D by ref
test_that("pass 2D by ref and blockRef works and error-traps (compiled & uncompiled) in nFunction", {
  message("This test has many trapped errors.")
  foo <- nFunction(
    function(x = numericMatrix(), xRef = numericMatrix(), xBlockRef = numericMatrix()) {
      xRef <- x[1:2, 1:3] + 10
      x <- xRef + 100
      xBlockRef <- xRef + 1000
      return(x)
    },
    refArgs = 'xRef',
    blockRefArgs = 'xBlockRef',
    returnType = 'numericMatrix'
  )
  test_foo <- function(fn) {
    x <- matrix(as.numeric(1:12), nrow = 3)
    xRef <- matrix(as.numeric(11:22), nrow = 3)
    xBlockRef <- matrix(as.numeric(21:32), nrow = 3)
    expect_error(fn(x, matrix(as.numeric(11:22), nrow = 3), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, matrix(as.numeric(21:32)), nrow = 3))      # Can't pass literal to blockRef
    expect_error(fn(x, xRef[1:3, 1:4], blockRef)) # Can't pass block to ref
    y <- fn(x, xRef, xBlockRef[2:3, 2:4])
    expect_equal(y, x[1:2, 1:3] + 10 + 100)
    expect_equal(xRef, x[1:2, 1:3] + 10)
    xBRans <- xBlockRef
    xBRans[2:3, 2:4] <- x[1:2, 1:3] + 10 + 1000
    expect_equal(xBlockRef, xBRans)
  }
  cfoo <- nCompile(foo)
  test_foo(foo)
  test_foo(cfoo)
  test_foo(cfoo) # Do twice to invoke byte-compiled version.

  cfoo <- nCompile(foo, package=TRUE)
  test_foo(cfoo)
  test_foo(cfoo)

  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing")
  test <- writePackage(foo, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, action = "prefix", code = load_dynamic_namespace("testpackage"))
  test_foo(testpackage::foo)
  test_foo(testpackage::foo)
})

test_that("pass 2D by ref and blockRef works and error-traps via nClass method (compiled & uncompiled)", {
  message("This test has four trapped errors.")
  nc1 <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x = numericMatrix(), xRef = numericMatrix(), xBlockRef = numericMatrix()) {
          xRef <- x[1:2, 1:3] + 10
          x <- xRef + 100
          xBlockRef <- xRef + 1000
          return(x)
        },
        refArgs = 'xRef',
        blockRefArgs = 'xBlockRef',
        returnType = 'numericMatrix'
      )
    ))
  test_foo <- function(fn) {
    x <- matrix(as.numeric(1:12), nrow = 3)
    xRef <- matrix(as.numeric(11:22), nrow = 3)
    xBlockRef <- matrix(as.numeric(21:32), nrow = 3)
    expect_error(fn(x, matrix(as.numeric(11:22), nrow = 3), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, matrix(as.numeric(21:32)), nrow = 3))      # Can't pass literal to blockRef
    expect_error(fn(x, xRef[1:3, 1:4], blockRef)) # Can't pass block to ref
    y <- fn(x, xRef, xBlockRef[2:3, 2:4])
    expect_equal(y, x[1:2, 1:3] + 10 + 100)
    expect_equal(xRef, x[1:2, 1:3] + 10)
    xBRans <- xBlockRef
    xBRans[2:3, 2:4] <- x[1:2, 1:3] + 10 + 1000
    expect_equal(xBlockRef, xBRans)
  }
  # uncompiled
  obj <- nc1$new()
  test_foo(obj$foo)

  # compiled directly
  Cnc1 <- nCompile(nc1, package = FALSE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()

  # Compiled via package
  Cnc1 <- nCompile(nc1, package = TRUE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()

  # Compiled via package via writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing")
  test <- writePackage(nc1, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, action = "prefix", code = load_dynamic_namespace("testpackage"))
  Cobj <- testpackage::nc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()
})

#####################
## 3D

# compiled and uncompiled 2D by ref
test_that("pass 3D by ref and blockRef works and error-traps (compiled & uncompiled) in nFunction", {
  message("This test has many trapped errors.")
  foo <- nFunction(
    function(x = numericArray(nDim=3), xRef = numericArray(nDim=3), xBlockRef = numericArray(nDim=3)) {
      xRef <- x[1:2, 1:3, 1:4] + 10
      x <- xRef + 100
      xBlockRef <- xRef + 1000
      return(x)
    },
    refArgs = 'xRef',
    blockRefArgs = 'xBlockRef',
    returnType = 'numericArray(nDim=3)'
  )
  test_foo <- function(fn) {
    x <- array(as.numeric(1:60), dim = c(3, 4, 5))
    xRef <- x + 10
    xBlockRef <- x + 20
    expect_error(fn(x, array(as.numeric(1:60), dim = c(3, 4, 5)), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, array(as.numeric(1:60), dim = c(3, 4, 5)), nrow = 3))      # Can't pass literal to blockRef
    expect_error(fn(x, xRef[1:3, 1:4, 1:5], blockRef)) # Can't pass block to ref
    y <- fn(x, xRef, xBlockRef[2:3, 2:4, 2:5])
    expect_equal(y, x[1:2, 1:3, 1:4] + 10 + 100)
    expect_equal(xRef, x[1:2, 1:3, 1:4] + 10)
    xBRans <- xBlockRef
    xBRans[2:3, 2:4, 2:5] <- x[1:2, 1:3, 1:4] + 10 + 1000
    expect_equal(xBlockRef, xBRans)
  }
  cfoo <- nCompile(foo)
  test_foo(foo)
  test_foo(cfoo)
  test_foo(cfoo) # Do twice to invoke byte-compiled version.

  cfoo <- nCompile(foo, package=TRUE)
  test_foo(cfoo)
  test_foo(cfoo)

  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing")
  test <- writePackage(foo, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, action = "prefix", code = load_dynamic_namespace("testpackage"))
  test_foo(testpackage::foo)
  test_foo(testpackage::foo)
})

test_that("pass 2D by ref and blockRef works and error-traps via nClass method (compiled & uncompiled)", {
  message("This test has four trapped errors.")
  nc1 <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x = numericArray(nDim=3), xRef = numericArray(nDim=3), xBlockRef = numericArray(nDim=3)) {
          xRef <- x[1:2, 1:3, 1:4] + 10
          x <- xRef + 100
          xBlockRef <- xRef + 1000
          return(x)
        },
        refArgs = 'xRef',
        blockRefArgs = 'xBlockRef',
        returnType = 'numericArray(nDim=3)'
      )
    ))
  test_foo <- function(fn) {
    x <- array(as.numeric(1:60), dim = c(3, 4, 5))
    xRef <- x + 10
    xBlockRef <- x + 20
    expect_error(fn(x, array(as.numeric(1:60), dim = c(3, 4, 5)), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, array(as.numeric(1:60), dim = c(3, 4, 5)), nrow = 3))      # Can't pass literal to blockRef
    expect_error(fn(x, xRef[1:3, 1:4, 1:5], blockRef)) # Can't pass block to ref
    y <- fn(x, xRef, xBlockRef[2:3, 2:4, 2:5])
    expect_equal(y, x[1:2, 1:3, 1:4] + 10 + 100)
    expect_equal(xRef, x[1:2, 1:3, 1:4] + 10)
    xBRans <- xBlockRef
    xBRans[2:3, 2:4, 2:5] <- x[1:2, 1:3, 1:4] + 10 + 1000
    expect_equal(xBlockRef, xBRans)
  }

  # uncompiled
  obj <- nc1$new()
  test_foo(obj$foo)

  # compiled directly
  Cnc1 <- nCompile(nc1, package = FALSE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()

  # Compiled via package
  Cnc1 <- nCompile(nc1, package = TRUE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()

  # Compiled via package via writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing")
  test <- writePackage(nc1, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, action = "prefix", code = load_dynamic_namespace("testpackage"))
  Cobj <- testpackage::nc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj); gc()
})

## It seems like this test should give an error because we are assigning
## from a vector to a scalar and the vector is not length 1.
## This seems to occur from flex_(y) assignment.
## The first element alone is used.
## test_that("scalar and vector pass by copy, basic test (compiled and uncompiled)", {
##   # 0D and 1D by copy in nClass
##   nc1 <- nClass(
##     Cpublic = list(
##       Cfoo = nFunction(
##         fun = function(x, z) {
##           y <- x + 1
##           return(y)
##         },
##         argTypes = list(x = 'numericScalar', z = 'numericVector'),
##         returnType = 'numericScalar')
##     )
##   )
##   Cnc1 <- nCompile(nc1)
##   Cobj <- Cnc1$new()
##   expect_equal(Cobj$Cfoo(10, 1:3), 10+(1:3))
## })
