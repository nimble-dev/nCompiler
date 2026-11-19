# Tests of passing by copy, reference, and block reference,
# from R-R, R-C, and C-C,
# with nFunctions and nClass methods,
# with different argument ordering,
# for different kinds of numeric objects

library(testthat)
library(nCompiler)

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
    returnType = "numericVector"
  )
  x <- 1:3
  y <- foo(x)
  expect_equal(x + 1, y)
  cfoo <- nCompile(foo)
  x <- 1:3
  y <- cfoo(x)
  expect_equal(x + 1, y)
})

####################
## 1D tests

# Shows bug of modifying a lazy copy by blockRef
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
    refArgs = "xRef",
    blockRefArgs = "xBlockRef",
    returnType = "numericVector"
  )
  test_foo <- function(fn) {
    x <- as.numeric(1:3)
    xRef <- as.numeric(11:13)
    xRef2 <- xRef
    xBlockRef <- as.numeric(21:23)
    xBlockRef2 <- xBlockRef
    expect_error(fn(x, 11:13, xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, 11:13)) # Can't pass literal to blockRef
    expect_error(fn(x, xRef[1:3], blockRef)) # Can't pass block to ref
    y <- fn(x, xRef, xBlockRef[2:3])
    expect_equal(y, x[1:2] + 10 + 100)
    expect_equal(xRef, x[1:2] + 10)
    expect_equal(xBlockRef, c(21, 1:2 + 10 + 1000))
    expect_equal(xRef2, 11:13) # copies should be unmodified
    expect_equal(xBlockRef2, 21:23)
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
  cfoo <- nCompile(foo, package = TRUE)
  test_foo(cfoo)
  test_foo(cfoo)
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing")
  test <- writePackage(foo, pkgName = "testpackage", dir = dir, modify = "clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing")
  dir.create(lib, showWarnings = FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage"),
    upgrade = FALSE, dependencies = FALSE, quick = TRUE, quiet = TRUE
  ))
  withr::with_libpaths(lib, action = "prefix", code = {
    load_dynamic_namespace("testpackage")
    test_foo(access_dynamic_package("testpackage", "foo"))
    test_foo(access_dynamic_package("testpackage", "foo"))
  })
})

test_that("pass 1D by ref and blockRef works and error-traps via nClass method (compiled & uncompiled)", {
  message("This test has four trapped errors.")

  # debug(nCompiler:::make_nClass_code)
  # undebug(nCompiler:::build_Cmethod_code_for_nClass)
  nc1 <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x = numericVector(), xRef = numericVector(), xBlockRef = numericVector()) {
          xRef <- x[1:2] + 10
          x <- xRef + 100
          xBlockRef <- xRef + 1000
          return(x)
        },
        refArgs = "xRef",
        blockRefArgs = "xBlockRef",
        returnType = "numericVector"
      )
    )
  )

  test_foo <- function(fn) {
    x <- as.numeric(1:3)
    xRef <- as.numeric(11:13)
    xBlockRef <- as.numeric(21:23)
    expect_error(fn(x, 11:13, xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, 11:13)) # Can't pass literal to blockRef
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
  rm(Cobj, CppObj)
  gc()
  # Compiled via package
  Cnc1 <- nCompile(nc1, package = TRUE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj)
  gc()
  # Compiled via package via writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassingb")
  test <- writePackage(nc1, pkgName = "testpackageb", dir = dir, modify = "clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassingb")
  dir.create(lib, showWarnings = FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackageb"),
    upgrade = FALSE, dependencies = FALSE, quick = TRUE, quiet = TRUE
  ))
  withr::with_libpaths(lib, action = "prefix", code = {
    load_dynamic_namespace("testpackageb")
    Cobj <- access_dynamic_package("testpackageb", "nc1")$new()
    test_foo(Cobj$foo)
    CppObj <- to_generic_interface(Cobj)
    test_foo(method(CppObj, "foo"))
    rm(Cobj, CppObj)
    gc()
  })
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
    refArgs = "xRef",
    blockRefArgs = "xBlockRef",
    returnType = "numericMatrix"
  )
  test_foo <- function(fn) {
    x <- matrix(as.numeric(1:12), nrow = 3)
    xRef <- matrix(as.numeric(11:22), nrow = 3)
    xBlockRef <- matrix(as.numeric(21:32), nrow = 3)
    expect_error(fn(x, matrix(as.numeric(11:22), nrow = 3), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, matrix(as.numeric(21:32)), nrow = 3)) # Can't pass literal to blockRef
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

  cfoo <- nCompile(foo, package = TRUE)
  test_foo(cfoo)
  test_foo(cfoo)

  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing2")
  test <- writePackage(foo, pkgName = "testpackage2", dir = dir, modify = "clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing2")
  dir.create(lib, showWarnings = FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage2"),
    upgrade = FALSE, dependencies = FALSE, quick = TRUE, quiet = TRUE
  ))
  withr::with_libpaths(lib, action = "prefix", code = {
    load_dynamic_namespace("testpackage2")
    test_foo(testpackage2::foo)
    test_foo(testpackage2::foo)
  })
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
        refArgs = "xRef",
        blockRefArgs = "xBlockRef",
        returnType = "numericMatrix"
      )
    )
  )
  test_foo <- function(fn) {
    x <- matrix(as.numeric(1:12), nrow = 3)
    xRef <- matrix(as.numeric(11:22), nrow = 3)
    xBlockRef <- matrix(as.numeric(21:32), nrow = 3)
    expect_error(fn(x, matrix(as.numeric(11:22), nrow = 3), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, matrix(as.numeric(21:32)), nrow = 3)) # Can't pass literal to blockRef
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
  rm(Cobj, CppObj)
  gc()

  # Compiled via package
  Cnc1 <- nCompile(nc1, package = TRUE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj)
  gc()

  # Compiled via package via writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing2b")
  test <- writePackage(nc1, pkgName = "testpackage2b", dir = dir, modify = "clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing2b")
  dir.create(lib, showWarnings = FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage2b"),
    upgrade = FALSE, dependencies = FALSE, quick = TRUE, quiet = TRUE
  ))
  withr::with_libpaths(lib, action = "prefix", code = {
    load_dynamic_namespace("testpackage2b")
    Cobj <- testpackage2b::nc1$new()
    test_foo(Cobj$foo)
    CppObj <- to_generic_interface(Cobj)
    test_foo(method(CppObj, "foo"))
    rm(Cobj, CppObj)
    gc()
  })
})

#####################
## 3D

# compiled and uncompiled 2D by ref
test_that("pass 3D by ref and blockRef works and error-traps (compiled & uncompiled) in nFunction", {
  message("This test has many trapped errors.")
  foo <- nFunction(
    function(x = numericArray(nDim = 3), xRef = numericArray(nDim = 3), xBlockRef = numericArray(nDim = 3)) {
      xRef <- x[1:2, 1:3, 1:4] + 10
      x <- xRef + 100
      xBlockRef <- xRef + 1000
      return(x)
    },
    refArgs = "xRef",
    blockRefArgs = "xBlockRef",
    returnType = "numericArray(nDim=3)"
  )
  test_foo <- function(fn) {
    x <- array(as.numeric(1:60), dim = c(3, 4, 5))
    xRef <- x + 10
    xBlockRef <- x + 20
    expect_error(fn(x, array(as.numeric(1:60), dim = c(3, 4, 5)), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, array(as.numeric(1:60), dim = c(3, 4, 5)), nrow = 3)) # Can't pass literal to blockRef
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

  cfoo <- nCompile(foo, package = TRUE)
  test_foo(cfoo)
  test_foo(cfoo)

  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing3")
  test <- writePackage(foo, pkgName = "testpackage3", dir = dir, modify = "clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing3")
  dir.create(lib, showWarnings = FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage3"),
    upgrade = FALSE, dependencies = FALSE, quick = TRUE, quiet = TRUE
  ))
  withr::with_libpaths(lib, action = "prefix", code = {
    load_dynamic_namespace("testpackage3")
    test_foo(testpackage3::foo)
    test_foo(testpackage3::foo)
  })
})

test_that("pass 2D by ref and blockRef works and error-traps via nClass method (compiled & uncompiled)", {
  message("This test has four trapped errors.")
  nc1 <- nClass(
    Cpublic = list(
      foo = nFunction(
        function(x = numericArray(nDim = 3), xRef = numericArray(nDim = 3), xBlockRef = numericArray(nDim = 3)) {
          xRef <- x[1:2, 1:3, 1:4] + 10
          x <- xRef + 100
          xBlockRef <- xRef + 1000
          return(x)
        },
        refArgs = "xRef",
        blockRefArgs = "xBlockRef",
        returnType = "numericArray(nDim=3)"
      )
    )
  )
  test_foo <- function(fn) {
    x <- array(as.numeric(1:60), dim = c(3, 4, 5))
    xRef <- x + 10
    xBlockRef <- x + 20
    expect_error(fn(x, array(as.numeric(1:60), dim = c(3, 4, 5)), xBlockRef)) # Can't pass literal to ref
    expect_error(fn(x, xRef, array(as.numeric(1:60), dim = c(3, 4, 5)), nrow = 3)) # Can't pass literal to blockRef
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
  rm(Cobj, CppObj)
  gc()

  # Compiled via package
  Cnc1 <- nCompile(nc1, package = TRUE)
  Cobj <- Cnc1$new()
  test_foo(Cobj$foo)
  CppObj <- to_generic_interface(Cobj)
  test_foo(method(CppObj, "foo"))
  rm(Cobj, CppObj)
  gc()

  # Compiled via package via writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage_argPassing3b")
  test <- writePackage(nc1, pkgName = "testpackage3b", dir = dir, modify = "clear")
  lib <- file.path(tempdir(), "test_nComp_lib_argPassing3b")
  dir.create(lib, showWarnings = FALSE)
  withr::with_libpaths(lib, action = "prefix", code = devtools::install(file.path(dir, "testpackage3b"),
    upgrade = FALSE, dependencies = FALSE, quick = TRUE, quiet = TRUE
  ))
  withr::with_libpaths(lib, action = "prefix", code = {
    load_dynamic_namespace("testpackage3b")
    Cobj <- testpackage3b::nc1$new()
    test_foo(Cobj$foo)
    CppObj <- to_generic_interface(Cobj)
    test_foo(method(CppObj, "foo"))
    rm(Cobj, CppObj)
    gc()
  })
})

## Tests added while re-designing these schemes as part of redesigning nClasses
## Jan 2026

test_that("nested pass by ref works", {
  foo1 <- nFunction(
    fun = function(x = double(1)) {
      x <- x + 1
    },
    refArgs = "x"
  )
  foo2 <- nFunction(
    fun = function(x = double(1)) {
      foo1(x)
    },
    refArgs = "x"
  )
  foo3 <- nFunction(
    fun = function(x = double(1)) {
      x <- x + 1
      foo1(x)
    },
    refArgs = "x"
  )
  x <- as.numeric(1:2)
  foo1(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  foo2(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  foo3(x)
  expect_equal(x, 3:4)

  C <- nCompile(foo1, foo2, foo3)
  x <- as.numeric(1:2)
  C$foo1(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  C$foo2(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  C$foo3(x)
  expect_equal(x, 3:4)

  # Nest from R to C++
  foo4 <- nFunction(
    fun = function(x = double(1)) {
      C$foo3(x)
    },
    refArgs = "x"
  )
  x <- as.numeric(1:2)
  foo4(x)
  expect_equal(x, 3:4)
})

test_that("nested pass by blockRef case 1 works", {
  foo1 <- nFunction(
    fun = function(x = double(1)) {
      x <- x + 1
    },
    blockRefArgs = "x"
  )
  foo2 <- nFunction(
    fun = function(x = double(1)) {
      foo1(x)
    },
    blockRefArgs = "x"
  )
  foo3 <- nFunction(
    fun = function(x = double(1)) {
      x <- x + 1
      foo1(x)
    },
    blockRefArgs = "x"
  )
  x <- as.numeric(1:2)
  foo1(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  foo2(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  foo3(x)
  expect_equal(x, 3:4)

  Cfoo1 <- nCompile(foo1)

  C <- nCompile(foo1, foo2, foo3)
  x <- as.numeric(1:2)
  C$foo1(x[1:2])
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  C$foo2(x[1:2])
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  C$foo3(x[1:2])
  expect_equal(x, 3:4)

  # Nest from R to C++
  foo4 <- nFunction(
    fun = function(x = double(1)) {
      C$foo3(x)
    },
    blockRefArgs = "x"
  )
  x <- as.numeric(1:2)
  foo4(x[1:2])
  expect_equal(x, 3:4)
})

# KNOWN FAILURES aka things that will not work.
# This will NEVER (in any forseeable way) be made to work
# in the case from R to C++
test_that("pass by ref with same ref in multiple args works except R->C++", {
  foo1 <- nFunction(
    fun = function(x = double(1), y = double(1)) {
      z <- x
      x <- x + 1
      y <- z + 2
      # foo1(v, v) should set v = v+2
    },
    refArgs = c("x", "y")
  )
  v <- as.numeric(1:2)
  foo1(v, v)
  expect_equal(v, 3:4)

  foo2 <- nFunction(
    fun = function(x = double(1), y = double(1)) {
      z <- x
      y <- z + 2
      x <- x + 1
      # foo1(v, v) should set v = v + 2
    },
    refArgs = c("x", "y")
  )
  v <- as.numeric(1:2)
  foo2(v, v)
  expect_equal(v, (1:2) + 3)

  C <- nCompile(foo1, foo2)

  v <- as.numeric(1:2)
  C$foo1(v, v)
  expect_false(identical(v, 3:4)) # WILL NOT WORK

  v <- as.numeric(1:2)
  C$foo2(v, v)
  expect_false(identical(v, (1:2) + 3)) # WILL NOT WORK
  # Nest from R to C++
})

# This will NEVER be made to work in the case from R to C++
test_that("pass by blockRef with same ref in multiple args works except R->C++", {
  foo1 <- nFunction(
    fun = function(x = double(1), y = double(1)) {
      z <- x
      x <- x + 1
      y <- z + 2
      # foo1(v, v) should set v = v+2
    },
    blockRefArgs = c("x", "y")
  )
  v <- as.numeric(1:3)
  foo1(v[2:3], v[2:3])
  expect_equal(v, c(1, (2:3) + 2))

  foo2 <- nFunction(
    fun = function(x = double(1), y = double(1)) {
      z <- x
      y <- z + 2
      x <- x + 1
      # foo1(v, v) should set v = v + 2
    },
    blockRefArgs = c("x", "y")
  )
  v <- as.numeric(1:3)
  foo2(v[2:3], v[2:3])
  expect_equal(v, c(1, (2:3) + 3))

  C <- nCompile(foo1, foo2)

  v <- as.numeric(1:3)
  C$foo1(v[2:3], v[2:3])
  expect_false(identical(v, c(1, (2:3) + 2))) # FAIL

  v <- as.numeric(1:3)
  C$foo2(v[2:3], v[2:3])
  expect_false(identical(v, c(1, (2:3) + 3)))
})

test_that("nested pass by blockRef works", {
  foo1 <- nFunction(
    fun = function(x = double(1)) {
      x <- x + 1
    },
    blockRefArgs = "x"
  )
  foo2 <- nFunction(
    fun = function(x = double(1)) {
      foo1(x[1:2])
    },
    blockRefArgs = "x"
  )
  foo3 <- nFunction(
    fun = function(x = double(1)) {
      x <- x + 1
      foo1(x[1:2])
    },
    blockRefArgs = "x"
  )
  x <- as.numeric(1:2)
  foo1(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  foo2(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  foo3(x)
  expect_equal(x, 3:4)

  C <- nCompile(foo1, foo2, foo3)
  x <- as.numeric(1:2)
  C$foo1(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  C$foo2(x)
  expect_equal(x, 2:3)
  x <- as.numeric(1:2)
  C$foo3(x)
  expect_equal(x, 3:4)
  # Inner block is a strict subset of the outer object
  x <- as.numeric(1:3)
  C$foo2(x)
  expect_equal(x, c(2, 3, 3))
  x <- as.numeric(1:3)
  C$foo3(x)
  expect_equal(x, c(3, 4, 4))
  # Outer block has an offset, so the inner block is a block of a block
  x <- as.numeric(1:4)
  C$foo2(x[2:4])
  expect_equal(x, c(1, 3, 4, 4))

  foo4 <- nFunction(
    fun = function(x = double(1)) {
      C$foo3(x)
    },
    blockRefArgs = "x"
  )
  x <- as.numeric(1:2)
  foo4(x)
  expect_equal(x, 3:4)
})

test_that("nested blockRef passing keeps offsets and strides (blocks of blocks, C++ to C++)", {
  foo1 <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100
    },
    blockRefArgs = "x"
  )
  inner_idx <- nFunction(
    fun = function(x = double(1)) {
      foo1(x[2:3]) # block of a blockRef argument
    },
    blockRefArgs = "x"
  )
  inner_whole <- nFunction(
    fun = function(x = double(1)) {
      foo1(x) # blockRef argument passed on whole
    },
    blockRefArgs = "x"
  )
  inner_2D <- nFunction(
    fun = function(x = double(2)) {
      foo1(x[2, 2:3])
    },
    blockRefArgs = "x"
  )
  level2 <- nFunction(
    fun = function(x = double(1)) {
      inner_idx(x[2:4])
    },
    blockRefArgs = "x"
  )
  level1 <- nFunction(
    fun = function(x = double(2)) {
      level2(x[, 3])
    },
    blockRefArgs = "x"
  )
  outer <- nFunction(
    fun = function(v = double(1), M = double(2), A = double(3), case = integer()) {
      if (case == 1) inner_idx(v[2:4]) # v[3]
      if (case == 2) inner_whole(v[2:4]) # v[2]
      if (case == 3) inner_idx(M[2, ]) # M[2, 2] (row has stride 3)
      if (case == 4) inner_whole(M[2, ]) # M[2, 1]
      if (case == 5) inner_2D(M[2:3, 2:4]) # M[3, 3]
      if (case == 6) level1(A[2, , ]) # A[2, , ][, 3][2:4][2:3][1] is A[2, 3, 3]
    },
    blockRefArgs = c("v", "M", "A")
  )
  C <- nCompile(foo1, inner_idx, inner_whole, inner_2D, level2, level1, outer)
  expected <- list(
    list(v = 3), list(v = 2), list(M = c(2, 2)), list(M = c(2, 1)),
    list(M = c(3, 3)), list(A = c(2, 3, 3))
  )
  for (cs in 1:6) {
    v <- as.numeric(1:5)
    M <- matrix(as.numeric(1:12), nrow = 3)
    A <- array(as.numeric(1:60), dim = c(3, 4, 5))
    C$outer(v, M, A, cs)
    v_exp <- as.numeric(1:5)
    M_exp <- matrix(as.numeric(1:12), nrow = 3)
    A_exp <- array(as.numeric(1:60), dim = c(3, 4, 5))
    e <- expected[[cs]]
    if (!is.null(e$v)) v_exp[e$v] <- v_exp[e$v] + 100
    if (!is.null(e$M)) M_exp[e$M[1], e$M[2]] <- M_exp[e$M[1], e$M[2]] + 100
    if (!is.null(e$A)) A_exp[e$A[1], e$A[2], e$A[3]] <- A_exp[e$A[1], e$A[2], e$A[3]] + 100
    expect_identical(v, v_exp, info = paste("case", cs))
    expect_identical(M, M_exp, info = paste("case", cs))
    expect_identical(A, A_exp, info = paste("case", cs))
  }
})

test_that("pass indexed and dimension-changing arguments by blockRef (C++ to C++)", {
  foo1 <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100
      return(sum(x))
      returnType(double())
    },
    blockRefArgs = "x"
  )
  # Indexed arguments become StridedTensorMaps
  foo2 <- nFunction(
    fun = function(v = double(1), M = double(2)) {
      s1 <- foo1(v[2:3])
      s2 <- foo1(M[, 2])
      s3 <- foo1(M[1:2, 1])
      return(c(s1, s2, s3))
      returnType(double(1))
    },
    blockRefArgs = c("v", "M")
  )
  # Whole objects with extra singleton dimensions
  foo3 <- nFunction(
    fun = function(M = double(2), A = double(3)) {
      s1 <- foo1(M)
      s2 <- foo1(A)
      return(c(s1, s2))
      returnType(double(1))
    },
    blockRefArgs = c("M", "A")
  )
  C <- nCompile(foo1, foo2, foo3)
  v <- as.numeric(1:3)
  M <- matrix(as.numeric(1:6), nrow = 3)
  ans <- C$foo2(v, M)
  expect_equal(ans, c(102 + 3, 104 + 5 + 6, 101 + 2))
  expect_equal(v, c(1, 102, 3))
  expect_equal(M, matrix(c(101, 2, 3, 104, 5, 6), nrow = 3))

  M <- matrix(as.numeric(1:3), nrow = 3)
  A <- array(as.numeric(1:3), dim = c(1, 3, 1))
  ans <- C$foo3(M, A)
  expect_equal(ans, c(101 + 2 + 3, 101 + 2 + 3))
  expect_equal(M, matrix(c(101, 2, 3), nrow = 3))
  expect_equal(A, array(c(101, 2, 3), dim = c(1, 3, 1)))
})

test_that("blockRef views follow the flex rule for dimensions (C++ to C++)", {
  # Mark element [i, j] (or [i]) and return the sum
  m2 <- nFunction(
    fun = function(x = double(2), i = integer(), j = integer()) {
      x[i, j] <- x[i, j] + 100
      return(sum(x))
      returnType(double())
    },
    blockRefArgs = "x"
  )
  v1 <- nFunction(
    fun = function(x = double(1), i = integer()) {
      x[i] <- x[i] + 100
      return(sum(x))
      returnType(double())
    },
    blockRefArgs = "x"
  )
  foo <- nFunction(
    fun = function(v = double(1), A = double(3), M = double(2), case = integer()) {
      s <- 0
      if (case == 1) s <- m2(v, 2L, 1L) # padding: (3) -> (3, 1)
      if (case == 2) s <- m2(A, 1L, 2L) # leading 1 kept: (1, 3, 1) -> (1, 3)
      if (case == 3) s <- v1(M[2, ], 3L) # row of a matrix (strided)
      if (case == 4) s <- v1(M, 2L) # 3x4 -> vector: no flattening, error
      return(s)
      returnType(double())
    },
    blockRefArgs = c("v", "A", "M")
  )
  foo2 <- nFunction(
    fun = function(A = double(3)) {
      return(m2(A[, , 2], 2L, 3L)) # offset from a scalar-indexed higher dimension
      returnType(double())
    },
    blockRefArgs = "A"
  )
  C <- nCompile(m2, v1, foo, foo2)

  v <- as.numeric(1:3)
  A <- array(as.numeric(1:3), c(1, 3, 1))
  M <- matrix(as.numeric(1:12), 3)
  expect_equal(C$foo(v, A, M, 1L), 106)
  expect_equal(v, c(1, 102, 3))
  expect_equal(C$foo(v, A, M, 2L), 106)
  expect_equal(as.numeric(A), c(1, 102, 3))
  expect_equal(C$foo(v, A, M, 3L), 2 + 5 + 108 + 11)
  expect_equal(M[2, 3], 108)
  M <- matrix(as.numeric(1:12), 3)
  expect_error(C$foo(v, A, M, 4L), "Cannot create requested variable block")
  expect_equal(M, matrix(as.numeric(1:12), 3))
  A <- array(as.numeric(1:12), c(2, 3, 2))
  expect_equal(C$foo2(A), sum(7:12) + 100)
  expect_equal(A[2, 3, 2], 112)
  expect_equal(A[-12], (1:12)[-12])
})

test_that("pass by copy adapts scalar type, nDim, expressions, and scalars (C++ to C++)", {
  # Copy arguments: modifying x does not affect the caller's object.
  cp <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100
      return(sum(x))
      returnType(double())
    }
  )
  cpi <- nFunction(
    fun = function(x = integer(1)) {
      return(sum(x))
      returnType(double())
    }
  )
  cpm <- nFunction(
    fun = function(x = double(2)) {
      return(x[3, 1])
      returnType(double())
    }
  )
  sc <- nFunction(
    fun = function(x = double()) {
      return(2 * x)
      returnType(double())
    }
  )
  foo <- nFunction(
    fun = function(v = double(1), iv = integer(1), M = double(2), a = double()) {
      ans <- c(
        cp(iv), # integer -> double
        cp(M), # 3x1 -> vector
        cp(iv + 1L), # integer expression
        cp(a), # scalar -> vector (length 1)
        cp(v), # same type (passed as-is)
        cp(v + 1), # same-type expression (passed as-is)
        cpi(v * 1.5), # double expression -> integer (truncates)
        cpm(v), # vector -> 3x1 matrix
        sc(v[2:2]) # length-1 vector -> scalar
      )
      return(ans)
      returnType(double(1))
    }
  )
  foo2 <- nFunction(
    fun = function(v = double(1)) {
      return(sc(v)) # run-time error unless v has length 1
      returnType(double())
    }
  )
  C <- nCompile(cp, cpi, cpm, sc, foo, foo2)
  v <- as.numeric(1:3)
  iv <- 1:3
  M <- matrix(as.numeric(1:3), ncol = 1)
  ans <- C$foo(v, iv, M, 5)
  expect_equal(ans, c(106, 106, 109, 105, 106, 109, 1 + 3 + 4, 3, 4))
  expect_identical(v, as.numeric(1:3))
  expect_identical(iv, 1:3)
  expect_identical(M, matrix(as.numeric(1:3), ncol = 1))
  expect_equal(C$foo2(7), 14)
  expect_error(C$foo2(v))
})

test_that("pass by blockRef adapts scalar type, expressions, and scalars (C++ to C++)", {
  br <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100.5
      return(sum(x))
      returnType(double())
    },
    blockRefArgs = "x"
  )
  foo <- nFunction(
    fun = function(v = double(1), iv = integer(1), a = double(), ia = integer(), case = integer()) {
      s <- 0
      if (case == 1) s <- br(iv) # integer -> double: cast copy, written back (truncated)
      if (case == 2) s <- br(iv[2:3]) # indexed integer: cast copy of the block, written back
      if (case == 3) s <- br(v + 1) # expression: temporary copy, changes lost
      if (case == 4) s <- br(a) # scalar variable: viewed as a length-1 vector
      if (case == 5) s <- br(ia) # integer scalar variable: cast copy, written back
      if (case == 6) s <- br(v[2]) # scalar element: viewed as a length-1 vector
      if (case == 7) s <- br(5) # literal: temporary copy
      return(c(s, a, ia))
      returnType(double(1))
    },
    blockRefArgs = c("v", "iv")
  )
  C <- nCompile(br, foo)
  run <- function(cs) {
    v <- as.numeric(1:3)
    iv <- 1:3
    ans <- C$foo(v, iv, 5, 7L, cs)
    list(s = ans[1], a = ans[2], ia = ans[3], v = v, iv = iv)
  }
  r <- run(1)
  expect_equal(r$s, 101.5 + 2 + 3)
  expect_identical(r$iv, c(101L, 2L, 3L))
  r <- run(2)
  expect_equal(r$s, 102.5 + 3)
  expect_identical(r$iv, c(1L, 102L, 3L))
  r <- run(3)
  expect_equal(r$s, 102.5 + 3 + 4)
  expect_identical(r$v, as.numeric(1:3))
  r <- run(4)
  expect_equal(r$s, 105.5)
  expect_equal(r$a, 105.5)
  r <- run(5)
  expect_equal(r$s, 107.5)
  expect_equal(r$ia, 107)
  r <- run(6)
  expect_equal(r$s, 102.5)
  expect_identical(r$v, c(1, 102.5, 3))
  r <- run(7)
  expect_equal(r$s, 105.5)

  # Optional code-generation warnings for copies
  old <- get_nOption("warn_arg_passing_copies")
  set_nOption("warn_arg_passing_copies", TRUE)
  on.exit(set_nOption("warn_arg_passing_copies", old), add = TRUE)
  foo2 <- nFunction(
    fun = function(iv = integer(1)) {
      return(br(iv))
      returnType(double())
    }
  )
  expect_warning(nCompile(br, foo2), "cast copy is passed and written back")
  foo3 <- nFunction(
    fun = function(v = double(1)) {
      return(br(v + 1))
      returnType(double())
    }
  )
  expect_warning(nCompile(br, foo3), "changes to it are lost")
})

test_that("pass by ref adapts scalar type, nDim, blocks, expressions, and scalars (C++ to C++)", {
  r1 <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100.5
      return(sum(x))
      returnType(double())
    },
    refArgs = "x"
  )
  # resizes its ref argument
  rz <- nFunction(
    fun = function(x = double(1), n = integer()) {
      x <- numeric(value = 9, length = n)
      return(sum(x))
      returnType(double())
    },
    refArgs = "x"
  )
  foo <- nFunction(
    fun = function(v = double(1), iv = integer(1), M = double(2), a = double(), ia = integer(), case = integer()) {
      s <- 0
      if (case == 1) s <- r1(v) # exact type: passed as-is
      if (case == 2) s <- r1(iv) # integer -> double: cast copy, written back (truncated)
      if (case == 3) s <- r1(M) # 3x1 matrix -> vector: copy, written back
      if (case == 4) s <- r1(v[2:3]) # block: copy, written back
      if (case == 5) s <- r1(M[1:2, 1]) # block of a matrix: copy, written back
      if (case == 6) s <- r1(v + 1) # expression: temporary copy, changes lost
      if (case == 7) s <- r1(a) # scalar variable: copy, written back
      if (case == 8) s <- r1(ia) # integer scalar variable: cast copy, written back
      if (case == 9) s <- r1(v[2]) # scalar element: copy, written back
      if (case == 10) s <- r1(5) # literal: temporary copy
      if (case == 11) s <- rz(iv, 5L) # whole object of another type: resized by write-back
      if (case == 12) s <- rz(M, 4L) # whole object of another nDim: resized by write-back
      if (case == 13) s <- rz(v[2:3], 4L) # block can't be resized: error
      if (case == 14) s <- rz(a, 2L) # scalar can't take 2 elements: error
      return(c(s, a, ia))
      returnType(double(1))
    },
    refArgs = c("v", "iv", "M")
  )
  C <- nCompile(r1, rz, foo)
  run <- function(cs) {
    v <- as.numeric(1:3)
    iv <- 1:3
    M <- matrix(as.numeric(1:3), 3, 1)
    ans <- C$foo(v, iv, M, 5, 7L, cs)
    list(s = ans[1], a = ans[2], ia = ans[3], v = v, iv = iv, M = M)
  }
  r <- run(1)
  expect_equal(r$s, 101.5 + 2 + 3)
  expect_identical(r$v, c(101.5, 2, 3))
  r <- run(2)
  expect_equal(r$s, 101.5 + 2 + 3)
  expect_identical(r$iv, c(101L, 2L, 3L))
  r <- run(3)
  expect_equal(r$s, 101.5 + 2 + 3)
  expect_identical(r$M, matrix(c(101.5, 2, 3), 3, 1))
  r <- run(4)
  expect_equal(r$s, 102.5 + 3)
  expect_identical(r$v, c(1, 102.5, 3))
  r <- run(5)
  expect_equal(r$s, 101.5 + 2)
  expect_identical(r$M, matrix(c(101.5, 2, 3), 3, 1))
  r <- run(6)
  expect_equal(r$s, 102.5 + 3 + 4)
  expect_identical(r$v, as.numeric(1:3))
  r <- run(7)
  expect_equal(r$s, 105.5)
  expect_equal(r$a, 105.5)
  r <- run(8)
  expect_equal(r$s, 107.5)
  expect_equal(r$ia, 107)
  r <- run(9)
  expect_equal(r$s, 102.5)
  expect_identical(r$v, c(1, 102.5, 3))
  r <- run(10)
  expect_equal(r$s, 105.5)
  r <- run(11)
  expect_equal(r$s, 45)
  expect_identical(r$iv, rep(9L, 5))
  r <- run(12)
  expect_equal(r$s, 36)
  expect_identical(r$M, matrix(9, 4, 1))
  expect_error(run(13))
  expect_error(run(14))

  # Optional code-generation warnings for copies
  old <- get_nOption("warn_arg_passing_copies")
  set_nOption("warn_arg_passing_copies", TRUE)
  on.exit(set_nOption("warn_arg_passing_copies", old), add = TRUE)
  foo2 <- nFunction(
    fun = function(iv = integer(1)) {
      return(r1(iv))
      returnType(double())
    }
  )
  expect_warning(nCompile(r1, foo2), "cast copy is passed and written back")
  foo3 <- nFunction(
    fun = function(v = double(1)) {
      return(r1(v + 1))
      returnType(double())
    }
  )
  expect_warning(nCompile(r1, foo3), "changes to it are lost")
  foo4 <- nFunction(
    fun = function(v = double(1)) {
      return(r1(v[2:3]))
      returnType(double())
    }
  )
  expect_warning(nCompile(r1, foo4), "not a whole variable")
})

test_that("pass ETaccessor arguments by copy, ref, and blockRef (C++ to C++)", {
  cp <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100.5
      return(sum(x))
      returnType(double())
    }
  )
  rf <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100.5
      return(sum(x))
      returnType(double())
    },
    refArgs = "x"
  )
  br <- nFunction(
    fun = function(x = double(1)) {
      x[1] <- x[1] + 100.5
      return(sum(x))
      returnType(double())
    },
    blockRefArgs = "x"
  )
  # resizes its ref argument
  rz <- nFunction(
    fun = function(x = double(1), n = integer()) {
      x <- numeric(value = 9, length = n)
      return(sum(x))
      returnType(double())
    },
    refArgs = "x"
  )
  nc <- nClass(
    Cpublic = list(
      dv = "numericVector", iv = "integerVector", M = "numericMatrix", s = "numericScalar",
      run = nFunction(
        function(kind = integer(), vn = "string") {
          ans <- 0
          if (kind == 1) ans <- cp(self[[vn]])
          if (kind == 2) ans <- rf(self[[vn]])
          if (kind == 3) ans <- br(self[[vn]])
          if (kind == 4) ans <- rz(self[[vn]], 4L)
          if (kind == 5) {
            acc <- self[[vn]]
            ans <- rf(acc)
          }
          return(ans)
          returnType(double())
        }
      )
    )
  )
  C <- nCompile(cp, rf, br, rz, nc)
  # dv: same type (passed directly for ref); iv: cast copy; M: 3x1 matrix, copy for ref;
  # s: scalar, viewed or copied as a length-1 vector
  newObj <- function() {
    o <- C$nc$new()
    o$dv <- c(1, 2, 3)
    o$iv <- 1:3
    o$M <- matrix(c(1, 2, 3), 3, 1)
    o$s <- 5
    o
  }
  changed <- list(
    dv = c(101.5, 2, 3), iv = c(101L, 2L, 3L), # integer write-back is truncated
    M = matrix(c(101.5, 2, 3), 3, 1), s = 105.5
  )
  for (vn in c("dv", "iv", "M", "s")) {
    sumExpected <- if (vn == "s") 105.5 else 106.5
    # copy: changes are not seen
    o <- newObj()
    orig <- o[[vn]]
    expect_equal(o$run(1, vn), sumExpected)
    expect_identical(o[[vn]], orig)
    # ref, blockRef, and ref through an ETaccessor variable: changes are written back
    for (kind in c(2, 3, 5)) {
      o <- newObj()
      expect_equal(o$run(kind, vn), sumExpected)
      expect_identical(o[[vn]], changed[[vn]])
    }
  }
  # Resizing through a ref
  o <- newObj()
  expect_equal(o$run(4, "dv"), 36) # same type: resized directly
  expect_identical(o$dv, rep(9, 4))
  expect_equal(o$run(4, "iv"), 36) # cast copy: source resized on write-back
  expect_identical(o$iv, rep(9L, 4))
  expect_equal(o$run(4, "M"), 36) # copy: source resized by the flex rule
  expect_identical(o$M, matrix(9, 4, 1))
  expect_error(o$run(4, "s"), "not all equal to 1") # a scalar can't hold 4 elements
  expect_identical(o$s, 5)
  rm(o)
  gc()
})

test_that("pass to scalar copy, ref, and blockRef parameters adapts arguments (C++ to C++)", {
  cp <- nFunction(
    fun = function(x = double()) {
      x <- x + 100.5
      return(x)
      returnType(double())
    }
  )
  rf <- nFunction(
    fun = function(x = double()) {
      x <- x + 100.5
      return(x)
      returnType(double())
    },
    refArgs = "x"
  )
  br <- nFunction(
    fun = function(x = double()) {
      x <- x + 100.5
      return(x)
      returnType(double())
    },
    blockRefArgs = "x" # a scalar blockRef is a reference, as for ref
  )
  foo <- nFunction(
    fun = function(v = double(1), iv = integer(1), M = double(2), v1 = double(1),
                   a = double(), ia = integer(), kind = integer(), case = integer()) {
      s <- 0
      if (kind == 1) {
        if (case == 1) s <- cp(a)
        if (case == 2) s <- cp(ia)
        if (case == 3) s <- cp(v[2])
        if (case == 4) s <- cp(iv[2])
        if (case == 5) s <- cp(M[2, 1])
        if (case == 6) s <- cp(v1)
        if (case == 7) s <- cp(v[2:2])
        if (case == 8) s <- cp(v)
        if (case == 9) s <- cp(a + 1)
        if (case == 10) s <- cp(5)
      }
      if (kind == 2) {
        if (case == 1) s <- rf(a)
        if (case == 2) s <- rf(ia)
        if (case == 3) s <- rf(v[2])
        if (case == 4) s <- rf(iv[2])
        if (case == 5) s <- rf(M[2, 1])
        if (case == 6) s <- rf(v1)
        if (case == 7) s <- rf(v[2:2])
        if (case == 8) s <- rf(v)
        if (case == 9) s <- rf(a + 1)
        if (case == 10) s <- rf(5)
      }
      if (kind == 3) {
        if (case == 1) s <- br(a)
        if (case == 2) s <- br(ia)
        if (case == 3) s <- br(v[2])
        if (case == 4) s <- br(iv[2])
        if (case == 5) s <- br(M[2, 1])
        if (case == 6) s <- br(v1)
        if (case == 7) s <- br(v[2:2])
        if (case == 8) s <- br(v)
        if (case == 9) s <- br(a + 1)
        if (case == 10) s <- br(5)
      }
      return(c(s, a, ia))
      returnType(double(1))
    },
    refArgs = c("v", "iv", "M", "v1")
  )
  C <- nCompile(cp, rf, br, foo)
  run <- function(kind, cs) {
    v <- c(1, 2, 3)
    iv <- 1:3
    M <- matrix(c(1, 2, 3), 3, 1)
    v1 <- 7
    ans <- C$foo(v, iv, M, v1, 5, 6L, kind, cs)
    list(s = ans[1], a = ans[2], ia = ans[3], v = v, iv = iv, M = M, v1 = v1)
  }
  sums <- c(105.5, 106.5, 102.5, 102.5, 102.5, 107.5, 102.5, NA, 106.5, 105.5)
  for (kind in 1:3) {
    for (cs in c(1:7, 9:10)) {
      r <- run(kind, cs)
      expect_equal(r$s, sums[cs])
      written <- kind > 1 # ref and blockRef write back; copy does not
      expect_equal(r$a, if (written && cs == 1) 105.5 else 5)
      expect_equal(r$ia, if (written && cs == 2) 106 else 6) # integer write-back is truncated
      expect_identical(r$v, if (written && cs %in% c(3, 7)) c(1, 102.5, 3) else c(1, 2, 3))
      expect_identical(r$iv, if (written && cs == 4) c(1L, 102L, 3L) else 1:3)
      expect_identical(r$M, matrix(if (written && cs == 5) c(1, 102.5, 3) else c(1, 2, 3), 3, 1))
      expect_identical(r$v1, if (written && cs == 6) 107.5 else 7)
    }
    expect_error(run(kind, 8), "to a scalar") # a length-3 vector can't be a scalar
  }

  # Optional code-generation warnings for copies
  old <- get_nOption("warn_arg_passing_copies")
  set_nOption("warn_arg_passing_copies", TRUE)
  on.exit(set_nOption("warn_arg_passing_copies", old), add = TRUE)
  foo2 <- nFunction(
    fun = function(v1 = double(1)) {
      return(rf(v1))
      returnType(double())
    }
  )
  expect_warning(nCompile(rf, foo2), "not a scalar variable or element")
  foo3 <- nFunction(
    fun = function(ia = integer()) {
      return(br(ia))
      returnType(double())
    }
  )
  expect_warning(nCompile(br, foo3), "cast copy is passed and written back")
})

test_that("pass ETaccessor arguments to scalar copy, ref, and blockRef parameters (C++ to C++)", {
  cp <- nFunction(
    fun = function(x = double()) {
      x <- x + 100.5
      return(x)
      returnType(double())
    }
  )
  rf <- nFunction(
    fun = function(x = double()) {
      x <- x + 100.5
      return(x)
      returnType(double())
    },
    refArgs = "x"
  )
  br <- nFunction(
    fun = function(x = double()) {
      x <- x + 100.5
      return(x)
      returnType(double())
    },
    blockRefArgs = "x"
  )
  nc <- nClass(
    Cpublic = list(
      s = "numericScalar", is = "integerScalar", v1 = "numericVector", v3 = "numericVector",
      run = nFunction(
        function(kind = integer(), vn = "string") {
          ans <- 0
          if (kind == 1) ans <- cp(self[[vn]])
          if (kind == 2) ans <- rf(self[[vn]])
          if (kind == 3) ans <- br(self[[vn]])
          return(ans)
          returnType(double())
        }
      )
    )
  )
  C <- nCompile(cp, rf, br, nc)
  newObj <- function() {
    o <- C$nc$new()
    o$s <- 5
    o$is <- 6L
    o$v1 <- 7
    o$v3 <- c(1, 2, 3)
    o
  }
  orig <- list(s = 5, is = 6L, v1 = 7)
  changed <- list(s = 105.5, is = 106L, v1 = 107.5) # integer write-back is truncated
  sums <- c(s = 105.5, is = 106.5, v1 = 107.5)
  for (kind in 1:3) {
    for (vn in c("s", "is", "v1")) {
      o <- newObj()
      expect_equal(o$run(kind, vn), sums[[vn]])
      expect_identical(o[[vn]], if (kind == 1) orig[[vn]] else changed[[vn]])
    }
    o <- newObj()
    expect_error(o$run(kind, "v3"), "not all equal to 1")
    expect_identical(o$v3, c(1, 2, 3))
  }
  rm(o)
  gc()
})

test_that("unsupported argument passing cases give compile-time errors", {
  foo1bref <- nFunction(
    fun = function(x = double(1)) {
      return(sum(x))
      returnType(double())
    },
    blockRefArgs = "x"
  )
  # A non-strided index can't be passed by blockRef
  foo3 <- nFunction(
    fun = function(x = double(1)) {
      return(foo1bref(x[c(1, 3)]))
      returnType(double())
    }
  )
  expect_error(nCompile(foo1bref, foo3), "only use empty, scalar, or `:` indices")
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
