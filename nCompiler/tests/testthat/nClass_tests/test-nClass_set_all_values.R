# Tests of assigning multiple fields of an nClass from a list or environment

library(nCompiler)
library(testthat)

test_that("assigning multiple fields of an nClass from a list works", {

  # A class that LACKS a default constructor and so will
  # lead to trapped error below
  nc0 <- nClass(
    classname = "nc0",
    Cpublic = list(
      w = 'numericVector',
      nc0 = nFunction(
        function(x = 'numericVector') {
        },
        compileInfo = list(constructor=TRUE)
      )
    ),
    compileInfo=list(omit_automatic_Cpp_construction=TRUE,
                     createFromR = FALSE)
  )

  nc1 <- nClass(
    Cpublic = list(
      x = 'numericVector',
      y = 'logicalVector'
    )
  )

  nc2 = nClass(
    classname = "nc2",
    Cpublic = list(
      my_nc1 = 'nc1',
      my_null_nc1 = 'nc1',
      my_nc0 = 'nc0', # chance to check catching error below
      z = 'numericScalar',
      nc2 = nFunction(
        function() {
          my_nc1 = nc1$new()
        },
        compileInfo = list(constructor=TRUE)
      )
    )
  )

  comp <- nCompile(nc0, nc1, nc2, interfaces = "generic") #list(nc1 ="generic", nc2 = "generic"))

  obj1a <- comp$nc1()
  value(obj1a, "x") <- 1:3
  value(obj1a, "x")
  value(obj1a) <- list(x = 1:3, y = TRUE)
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- list(x = 4:6)
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- list(y = c(1, 0, 1))
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- list(not_there = 100)
  value(obj1a, "x")
  value(obj1a, "y")


  value(obj1a) <- as.environment(list(x = 7:9, y = c(0, 1)))
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- as.environment(list(y = FALSE))
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- as.environment(list(not_there = 100))
  value(obj1a, "x")
  value(obj1a, "y")

  obj2a <- comp$nc2()
  value(obj2a, "my_nc1") |> value("x")
  value(obj2a) <- list(z = 42, my_nc1 = obj1a)
  value(obj2a, "z")
  value(obj2a, "my_nc1") |> value("x")
  value(obj2a, "my_nc1") |> value("y")

  value( value(obj2a, "my_nc1"), "x") <- 101:103
  value(obj1a, "x")

  value( value(obj2a, "my_nc1") ) <- list(x = 104:106, y = c(1,1,1))
  value(obj1a, "x")
  value(obj1a, "y")

  value(obj2a, "my_nc1") <- list(x = 201:203, y = c(0,0,0)) # makes new object
  expect_equal(value(obj1a, "x"), 104:106) # old values, because obj2a$my_nc1 is new
  expect_equal(value(obj1a, "y"), c(T, T, T))
  obj1b <- value(obj2a, "my_nc1") # new object
  expect_equal(value(obj1b, "x"), 201:203) # new values
  expect_equal(value(obj1b, "y"), c(F, F, F))

  value(obj2a, "my_nc1") <- as.environment(list(x = 301:303)) # makes new object
  expect_equal(value(obj1b, "x"), 201:203)
  expect_equal(value(obj1b, "y"), c(F, F, F))
  obj1c <- value(obj2a, "my_nc1")
  expect_equal(value(obj1c, "x"), 301:303)
  expect_equal(value(obj1c, "y"), logical())

  value(obj2a, "my_null_nc1") <- list(x = 1:3, y = TRUE)
  value(obj2a, "my_null_nc1") |> value("x")
  value(obj2a, "my_null_nc1") <- as.environment(list(x = 4:6, y = TRUE))
  value(obj2a, "my_null_nc1") |> value("x")

  expect_error(value(obj2a, "my_nc0") <- list(w = 1:3))
})


test_that("assigning multiple fields of an nClass from a list works", {

  nc0 <- nClass(
    classname = "nc0",
    Cpublic = list(
      w = 'numericVector',
      nc0 = nFunction(
        function(x = 'numericVector') {
        },
        compileInfo = list(constructor=TRUE)
      )
    ),
    compileInfo=list(omit_automatic_Cpp_construction=TRUE,
                     createFromR = FALSE)
  )

  nc1 <- nClass(
    Cpublic = list(
      x = 'numericVector',
      y = 'logicalVector'
    )
  )

  nc2 = nClass(
    classname = "nc2",
    Cpublic = list(
      my_nc1 = 'nc1',
      my_null_nc1 = 'nc1',
      my_nc0 = 'nc0', # chance to check catching error below
      z = 'numericScalar',
      nc2 = nFunction(
        function() {
          my_nc1 = nc1$new()
        },
        compileInfo = list(constructor=TRUE)
      )
    )
  )

  comp <- nCompile(nc0, nc1, nc2, interfaces = "full")

  obj1a <- comp$nc1$new()
  value(obj1a, "x") <- 1:3
  value(obj1a, "x")
  value(obj1a) <- list(x = 1:3, y = TRUE)
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- list(x = 4:6)
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- list(y = c(1, 0, 1))
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- list(not_there = 100)
  value(obj1a, "x")
  value(obj1a, "y")


  value(obj1a) <- as.environment(list(x = 7:9, y = c(0, 1)))
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- as.environment(list(y = FALSE))
  value(obj1a, "x")
  value(obj1a, "y")
  value(obj1a) <- as.environment(list(not_there = 100))
  value(obj1a, "x")
  value(obj1a, "y")

  obj2a <- comp$nc2$new()
  value(obj2a, "my_nc1") |> value("x")
  value(obj2a) <- list(z = 42, my_nc1 = obj1a)
  value(obj2a, "z")
  value(obj2a, "my_nc1") |> value("x")
  value(obj2a, "my_nc1") |> value("y")

  value( value(obj2a, "my_nc1"), "x") <- 101:103
  value(obj1a, "x")

  value( value(obj2a, "my_nc1") ) <- list(x = 104:106, y = c(1,1,1))
  value(obj1a, "x")
  value(obj1a, "y")

  value(obj2a, "my_nc1") <- list(x = 201:203, y = c(0,0,0)) # makes new object
  expect_equal(value(obj1a, "x"), 104:106) # old values, because obj2a$my_nc1 is new
  expect_equal(value(obj1a, "y"), c(T, T, T))
  obj1b <- value(obj2a, "my_nc1") # new object
  expect_equal(value(obj1b, "x"), 201:203) # new values
  expect_equal(value(obj1b, "y"), c(F, F, F))

  value(obj2a, "my_nc1") <- as.environment(list(x = 301:303)) # makes new object
  expect_equal(value(obj1b, "x"), 201:203)
  expect_equal(value(obj1b, "y"), c(F, F, F))
  obj1c <- value(obj2a, "my_nc1")
  expect_equal(value(obj1c, "x"), 301:303)
  expect_equal(value(obj1c, "y"), logical())

  value(obj2a, "my_null_nc1") <- list(x = 1:3, y = TRUE)
  value(obj2a, "my_null_nc1") |> value("x")
  value(obj2a, "my_null_nc1") <- as.environment(list(x = 4:6, y = TRUE))
  value(obj2a, "my_null_nc1") |> value("x")

  expect_error(value(obj2a, "my_nc0") <- list(w = 1:3))

  #####

  obj2a <- comp$nc2$new()
  obj2a$my_nc1$x
  obj2a$my_nc1$x <- 101:103
  obj2a$my_nc1$x
  obj2a$my_nc1 <- list(x = 104:106, y = c(1,1,1))
  obj2a$my_nc1$x
  obj1a <- obj2a$my_nc1
  obj1a$x
  obj1a$y

  obj2a$my_nc1 <- as.environment(list(x = 201:203, y = c(0,0,0)))
  obj1a$x
  obj1a$y

  obj2a$my_null_nc1 <- list(x = 1:3, y = TRUE)
  obj2a$my_null_nc1$x
  obj2a$my_null_nc1$y
  obj2a$my_null_nc1 <- as.environment(list(x = 4:6, y = FALSE))
  obj2a$my_null_nc1$x
  obj2a$my_null_nc1$y

  obj2a$my_nc0 <- list(w = 1:3)
})
