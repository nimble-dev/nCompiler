# Tests of assigning multiple fields of an nClass from a list or environment

# library(nCompiler)
# library(testthat)

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
  expect_equal(value(obj1a, "x"), 1:3)
  expect_equal(value(obj1a, "y"), logical())
  value(obj1a) <- list(x = 2:4, y = TRUE)
  expect_equal(value(obj1a, "x"), 2:4)
  expect_equal(value(obj1a, "y"), TRUE)
  value(obj1a) <- list(x = 4:6)
  expect_equal(value(obj1a, "x"), 4:6)
  expect_equal(value(obj1a, "y"), TRUE)
  value(obj1a) <- list(y = c(1, 0, 1))
  expect_equal(value(obj1a, "x"), 4:6)
  expect_equal(value(obj1a, "y"), as.logical(c(1,0,1)))
  value(obj1a) <- list(not_there = 100)
  expect_equal(value(obj1a, "x"), 4:6)
  expect_equal(value(obj1a, "y"), as.logical(c(1,0,1)))

  value(obj1a) <- as.environment(list(x = 7:9, y = c(0, 1)))
  expect_equal(value(obj1a, "x"), 7:9)
  expect_equal(value(obj1a, "y"), as.logical(c(0,1)))
  value(obj1a) <- as.environment(list(y = FALSE))
  expect_equal(value(obj1a, "x"), 7:9)
  expect_equal(value(obj1a, "y"), FALSE)
  value(obj1a) <- as.environment(list(not_there = 100))
  expect_equal(value(obj1a, "x"), 7:9)
  expect_equal(value(obj1a, "y"), FALSE)

  obj2a <- comp$nc2()
  expect_identical(value(obj2a, "my_nc1") |> value("x"), numeric())
  value(obj2a) <- list(z = 42, my_nc1 = obj1a)
  expect_equal(value(obj2a, "z"), 42)
  expect_equal(value(obj2a, "my_nc1") |> value("x"), 7:9)
  expect_equal(value(obj2a, "my_nc1") |> value("y"), FALSE)

  value( value(obj2a, "my_nc1"), "x") <- 101:103
  expect_equal(value(obj1a, "x"), 101:103)

  value( value(obj2a, "my_nc1") ) <- list(x = 104:106, y = c(1,1,1))
  expect_equal(value(obj1a, "x"), 104:106)
  expect_equal(value(obj1a, "y"), as.logical(c(1,1,1)))

  value(obj2a, "my_nc1") <- list(x = 201:203, y = c(0,0,0))
  expect_equal(value(obj1a, "x"), 201:203)
  expect_equal(value(obj1a, "y"), as.logical(c(0,0,0)))
  obj1b <- value(obj2a, "my_nc1")
  expect_equal(value(obj1b, "x"), 201:203)
  expect_equal(value(obj1b, "y"), c(F, F, F))

  value(obj2a, "my_nc1") <- as.environment(list(x = 301:303))
  expect_equal(value(obj1b, "x"), 301:303)
  expect_equal(value(obj1b, "y"), c(F, F, F))
  obj1c <- value(obj2a, "my_nc1")
  expect_equal(value(obj1c, "x"), 301:303)
  expect_equal(value(obj1c, "y"), c(F,F,F))

  value(obj2a, "my_null_nc1") <- list(x = 1:3, y = TRUE) # makes new object
  expect_equal(value(obj2a, "my_null_nc1") |> value("x"), 1:3)
  value(obj2a, "my_null_nc1") <- as.environment(list(x = 4:6, y = TRUE))
  expect_equal(value(obj2a, "my_null_nc1") |> value("x"), 4:6)
  expect_equal(value(obj2a, "my_null_nc1") |> value("y"), TRUE)

  expect_error(value(obj2a, "my_nc0") <- list(w = 1:3))
  rm(obj1a, obj1b, obj2a); gc()
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

  # Use the tests above because generic interface
  # should also work with full interface objects
  obj1a <- comp$nc1$new()
  value(obj1a, "x") <- 1:3
  expect_equal(value(obj1a, "x"), 1:3)
  expect_equal(value(obj1a, "y"), logical())
  value(obj1a) <- list(x = 2:4, y = TRUE)
  expect_equal(value(obj1a, "x"), 2:4)
  expect_equal(value(obj1a, "y"), TRUE)
  value(obj1a) <- list(x = 4:6)
  expect_equal(value(obj1a, "x"), 4:6)
  expect_equal(value(obj1a, "y"), TRUE)
  value(obj1a) <- list(y = c(1, 0, 1))
  expect_equal(value(obj1a, "x"), 4:6)
  expect_equal(value(obj1a, "y"), as.logical(c(1,0,1)))
  value(obj1a) <- list(not_there = 100)
  expect_equal(value(obj1a, "x"), 4:6)
  expect_equal(value(obj1a, "y"), as.logical(c(1,0,1)))

  value(obj1a) <- as.environment(list(x = 7:9, y = c(0, 1)))
  expect_equal(value(obj1a, "x"), 7:9)
  expect_equal(value(obj1a, "y"), as.logical(c(0,1)))
  value(obj1a) <- as.environment(list(y = FALSE))
  expect_equal(value(obj1a, "x"), 7:9)
  expect_equal(value(obj1a, "y"), FALSE)
  value(obj1a) <- as.environment(list(not_there = 100))
  expect_equal(value(obj1a, "x"), 7:9)
  expect_equal(value(obj1a, "y"), FALSE)

  obj2a <- comp$nc2$new()
  expect_identical(value(obj2a, "my_nc1") |> value("x"), numeric())
  value(obj2a) <- list(z = 42, my_nc1 = obj1a)
  expect_equal(value(obj2a, "z"), 42)
  expect_equal(value(obj2a, "my_nc1") |> value("x"), 7:9)
  expect_equal(value(obj2a, "my_nc1") |> value("y"), FALSE)

  value( value(obj2a, "my_nc1"), "x") <- 101:103
  expect_equal(value(obj1a, "x"), 101:103)

  value( value(obj2a, "my_nc1") ) <- list(x = 104:106, y = c(1,1,1))
  expect_equal(value(obj1a, "x"), 104:106)
  expect_equal(value(obj1a, "y"), as.logical(c(1,1,1)))

  value(obj2a, "my_nc1") <- list(x = 201:203, y = c(0,0,0))
  expect_equal(value(obj1a, "x"), 201:203)
  expect_equal(value(obj1a, "y"), as.logical(c(0,0,0)))
  obj1b <- value(obj2a, "my_nc1")
  expect_equal(value(obj1b, "x"), 201:203)
  expect_equal(value(obj1b, "y"), c(F, F, F))

  value(obj2a, "my_nc1") <- as.environment(list(x = 301:303))
  expect_equal(value(obj1b, "x"), 301:303)
  expect_equal(value(obj1b, "y"), c(F, F, F))
  obj1c <- value(obj2a, "my_nc1")
  expect_equal(value(obj1c, "x"), 301:303)
  expect_equal(value(obj1c, "y"), c(F,F,F))

  value(obj2a, "my_null_nc1") <- list(x = 1:3, y = TRUE) # makes new object
  expect_equal(value(obj2a, "my_null_nc1") |> value("x"), 1:3)
  value(obj2a, "my_null_nc1") <- as.environment(list(x = 4:6, y = TRUE))
  expect_equal(value(obj2a, "my_null_nc1") |> value("x"), 4:6)
  expect_equal(value(obj2a, "my_null_nc1") |> value("y"), TRUE)

  expect_error(value(obj2a, "my_nc0") <- list(w = 1:3))
  rm(obj1a, obj1b, obj2a); gc()

  ###
  ## Add some tests using the actual full interface

  obj2a <- comp$nc2$new()
  expect_equal(obj2a$my_nc1$x, numeric())
  obj2a$my_nc1$x <- 101:103
  expect_equal(obj2a$my_nc1$x, 101:103)
  obj2a$my_nc1 <- list(x = 104:106, y = c(1,1,1))
  expect_equal(obj2a$my_nc1$x, 104:106)
  obj1a <- obj2a$my_nc1
  expect_equal(obj1a$x, 104:106)
  expect_equal(obj1a$y, as.logical(c(1,1,1)))

  obj2a$my_nc1 <- as.environment(list(x = 201:203, y = c(0,0,0)))
  expect_equal(obj1a$x, 201:203)
  expect_equal(obj1a$y, as.logical(c(0,0,0)))

  obj2a$my_null_nc1 <- list(x = 1:3, y = TRUE)
  expect_equal(obj2a$my_null_nc1$x, 1:3)
  expect_equal(obj2a$my_null_nc1$y, TRUE)
  obj2a$my_null_nc1 <- as.environment(list(x = 4:6, y = FALSE))
  expect_equal(obj2a$my_null_nc1$x, 4:6)
  expect_equal(obj2a$my_null_nc1$y, FALSE)

  # Could add more but stopping. I'm not sure there's a purpose
  # in further exercising the full interface. At this point I have
  # ended up testing that the generic interface for a full object
  # does the same thing internally and there is no further point
  # to pursue here.
  rm(obj2a, obj1a); gc()
})
