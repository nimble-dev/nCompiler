# Tests of nClasses holding, passing and use each other.

#library(nCompiler); library(testthat)
test_that("One nClass holds another and uses it", {
  nc_inner <- nClass(
    classname = "nc_inner",
    Cpublic = list(
      x = 'numericScalar',
      get_x = nFunction(function() {return(x)}, returnType = 'numericScalar')
    )
  )
  nc_outer <- nClass(
    classname = "nc_outer",
    Cpublic = list(
      my_inner = 'nc_inner',
      init = nFunction(function() {my_inner = nc_inner$new()}),
      get_inner = nFunction(function() {return(my_inner)}, returnType = 'nc_inner'),
      inner_x_p1 = nFunction(function() {return(my_inner$x+1)}, returnType='numericScalar')
    )
  )
  comp <- nCompile(nc_inner, nc_outer, package = TRUE)
  obj <- comp$nc_outer$new()
  inner_obj <- obj$my_inner
  expect_true(is.null(inner_obj))
  obj$init()
  inner_obj <- obj$my_inner
  expect_true(obj$isCompiled())
  expect_true(inherits(inner_obj, "nClass"))
  expect_true(inner_obj$isCompiled())

  inner_obj$x <- 10
  expect_equal(obj$get_inner()$x, 10)
  expect_equal(obj$inner_x_p1(), 11)
  rm(obj, inner_obj)
  gc()
})

test_that("One nClass holds another by a base class and uses it", {

  ncA <- nClass(
    Cpublic = list(
      classname = "ncA",
      v.A = 'numericVector',
      wA = 'numericScalar',
      add.wA = nFunction(
        function(x.1 = 'numericVector') {
          return(wA + x.1); returnType('numericVector')
        }
      )
    ),
    compileInfo = list(interface="generic", createFromR=FALSE)
  )
  nc_inner <- nClass(
    inherit = ncA,
    classname = "nc_inner",
    Cpublic = list(
      x = 'numericScalar',
      get_x = nFunction(function() {return(x)}, returnType = 'numericScalar')
    )
  )
  nc_outer <- nClass(
    classname = "nc_outer",
    Cpublic = list(
      my_inner = 'nc_inner',
      my_A = 'ncA',
      init = nFunction(function() {my_inner = nc_inner$new()}),
      initA = nFunction(function() {my_A = nc_inner$new() }),
      useA = nFunction(function() {my_A$wA <- 10; return(my_A$wA + 3)}, returnType='numericScalar'),
      get_inner = nFunction(function() {return(my_inner)}, returnType = 'nc_inner'),
      inner_x_p1 = nFunction(function() {return(my_inner$x+1)}, returnType='numericScalar'),
      inner_add_wA_p2 = nFunction(function(v='numericVector') {return(my_inner$add.wA(v)+2)}, returnType='numericVector'),
      inner_wA_p3 = nFunction(function() {return(my_inner$wA + 3)}, returnType='numericScalar')
    )
  )
  for(package in c(TRUE, FALSE)) {
    comp <- nCompile(nc_inner, nc_outer, ncA, package = package)
    obj <- comp$nc_outer$new()
    inner_obj <- obj$my_inner
    expect_true(is.null(inner_obj))
    obj$init()
    inner_obj <- obj$my_inner
    expect_true(inner_obj$isCompiled())
    expect_true(inherits(inner_obj, "nClass"))

    obj$my_A
    obj$initA()
    # since my_A is of type ncA, which has interface="none", we get back only a raw externalptr,
    expect_true(inherits(obj$my_A, "loadedObjectEnv"))
    obj$useA()

    inner_obj$x <- 10
    expect_equal(obj$get_inner()$x, 10)
    expect_equal(obj$inner_x_p1(), 11)

    inner_obj$wA <- 20
    inner_obj$v.A <- 1:3
    expect_equal(obj$get_inner()$v.A, 1:3)
    expect_equal(inner_obj$add.wA(1:3), 1:3 + 20)

    expect_equal(obj$inner_add_wA_p2(1:3), 1:3 + 20 + 2)
    expect_equal(obj$inner_wA_p3(), 20 + 3)

    rm(obj, inner_obj)
    gc()
  }
})
