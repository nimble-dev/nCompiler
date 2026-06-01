# Tests for the `self` keyword in nClass method bodies.
#
# `self` compiles to nC_shared_from_this(), which returns
# std::shared_ptr<ClassName>. This covers three uses:
#   1. self$method(x)  -- method call on the current object
#   2. return(self)    -- return the current object by shared_ptr
#   3. f(self)         -- pass the current object as an argument
#
# All three are tested for a plain (root) nClass and for a derived nClass.
# Uncompiled behaviour uses R6's built-in `self` keyword, so it matches
# compiled behaviour for all three cases.

# ---------------------------------------------------------------------------
# Plain (root) nClass
# ---------------------------------------------------------------------------

test_that("self$method() works in a plain nClass", {
  nc <- nClass(
    classname = "nc_self_method",
    Cpublic = list(
      x = 'numericScalar',
      double_x = nFunction(
        function() { return(x * 2); returnType('numericScalar') }
      ),
      set_x_via_self = nFunction(
        function(x_ = 'numericScalar') {self$x <- x_}
      ),
      get_double_x_via_self = nFunction(
        function() { return(self$double_x()); returnType('numericScalar') }
      )
    )
  )

  obj <- nc$new()
  obj$x <- 3
  obj$set_x_via_self(5)
  expect_equal(obj$get_double_x_via_self(), 10)

  for(package in c(FALSE, TRUE)) {
    comp <- nCompile(nc, package = package)
    Cobj <- comp$new()
    Cobj$x <- 3
    Cobj$set_x_via_self(5)
    expect_equal(Cobj$get_double_x_via_self(), 10)
    rm(Cobj); gc()
  }
})

test_that("self as return value works in a plain nClass", {
  nc <- nClass(
    classname = "nc_self_return",
    Cpublic = list(
      x = 'numericScalar',
      get_self = nFunction(
        function() { return(self); returnType('nc') }
      )
    )
  )

  obj <- nc$new()
  obj$x <- 42
  s <- obj$get_self()
  expect_equal(s$x, 42)

  for(package in c(FALSE, TRUE)) {
    comp <- nCompile(nc, package = package)
    Cobj <- comp$new()
    Cobj$x <- 42
    Cs <- Cobj$get_self()
    expect_true(inherits(Cs, "nc_self_return"))
    expect_equal(Cs$x, 42)
    rm(Cobj, Cs); gc()
  }
})

test_that("self as argument works in a plain nClass", {
  nc <- nClass(
    classname = "nc_self_arg",
    Cpublic = list(
      x = 'numericScalar',
      get_x_from = nFunction(
        function(other = 'nc') {
          return(other$x); returnType('numericScalar')
        }
      ),
      get_my_x_via_self = nFunction(
        function() { return(self$get_x_from(self)); returnType('numericScalar') }
      )
    )
  )

  obj <- nc$new()
  obj$x <- 7
  expect_equal(obj$get_my_x_via_self(), 7)

  for(package in c(FALSE, TRUE)) {
    comp <- nCompile(nc, package = package)
    Cobj <- comp$new()
    Cobj$x <- 7
    expect_equal(Cobj$get_my_x_via_self(), 7)
    rm(Cobj); gc()
  }
})

# ---------------------------------------------------------------------------
# Derived nClass
# ---------------------------------------------------------------------------

test_that("self$method() works in a derived nClass", {
  ncBase <- nClass(
    classname = "nc_self_hier_base1",
    Cpublic = list(
      x = 'numericScalar',
      double_x = nFunction(
        function() { return(x * 2); returnType('numericScalar') },
        compileInfo = list(virtual = TRUE)
      )
    ),
    compileInfo = list(interface = "generic", createFromR = FALSE,
                       exportName = "ncBase_export",
                       packageNames = c(uncompiled = "ncBase"))
  )

  ncDer <- nClass(
    classname = "nc_self_hier_der1",
    inherit = ncBase,
    Cpublic = list(
      get_double_x_via_self = nFunction(
        function() { return(self$double_x()); returnType('numericScalar') }
      ),
      triple_x = nFunction(
        function() {return(x*3); returnType('numericScalar')}
      ),
      get_triple_x_via_self = nFunction(
        function() {return(self$triple_x()); returnType('numericScalar')}
      )
    )
  )

  obj <- ncDer$new()
  obj$x <- 5
  expect_equal(obj$get_double_x_via_self(), 10)
  expect_equal(obj$get_triple_x_via_self(), 15)

  for(package in c(FALSE, TRUE)) {
    comp <- nCompile(ncBase, ncDer, package = package)
    Cobj <- comp$ncDer$new()
    Cobj$x <- 5
    expect_equal(Cobj$get_double_x_via_self(), 10)
    expect_equal(Cobj$get_triple_x_via_self(), 15)
    rm(Cobj); gc()
  }
})

test_that("self as return value works in a derived nClass", {
  ncBase <- nClass(
    classname = "nc_self_hier_base2",
    Cpublic = list(
      x = 'numericScalar'
    ),
    compileInfo = list(interface = "generic", createFromR = FALSE,
                       exportName = "ncBase_export",
                       packageNames = c(uncompiled = "ncBase"))
  )

  ncDer <- nClass(
    classname = "nc_self_hier_der2",
    inherit = ncBase,
    Cpublic = list(
      y = 'numericScalar',
      get_self = nFunction(
        function() { return(self); returnType('ncDer') }
      )
    )
  )

  obj <- ncDer$new()
  obj$x <- 10
  obj$y <- 20
  s <- obj$get_self()
  expect_equal(s$x, 10)
  expect_equal(s$y, 20)

  for(package in c(FALSE, TRUE)) {
    comp <- nCompile(ncBase, ncDer, package = package)
    Cobj <- comp$ncDer$new()
    Cobj$x <- 10
    Cobj$y <- 20
    Cs <- Cobj$get_self()
    expect_equal(Cs$x, 10)
    expect_equal(Cs$y, 20)
    rm(Cobj, Cs); gc()
  }
})

test_that("self as argument works in a derived nClass", {
  ncBase <- nClass(
    classname = "nc_self_hier_base3",
    Cpublic = list(
      x = 'numericScalar'
    ),
    compileInfo = list(interface = "generic", createFromR = FALSE,
                       exportName = "ncBase_export",
                       packageNames = c(uncompiled = "ncBase"))
  )

  ncDer <- nClass(
    classname = "nc_self_hier_der3",
    inherit = ncBase,
    Cpublic = list(
      get_x_from = nFunction(
        function(other = 'ncDer') {
          return(other$x); returnType('numericScalar')
        }
      ),
      get_my_x_via_self = nFunction(
        function() { return(self$get_x_from(self)); returnType('numericScalar') }
      )
    )
  )

  obj <- ncDer$new()
  obj$x <- 99
  expect_equal(obj$get_my_x_via_self(), 99)
  for(package in c(FALSE, TRUE)) {
    comp <- nCompile(ncBase, ncDer, package = package)
    Cobj <- comp$ncDer$new()
    Cobj$x <- 99
    expect_equal(Cobj$get_my_x_via_self(), 99)
    rm(Cobj); gc()
  }
})
