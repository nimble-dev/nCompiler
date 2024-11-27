library(nCompiler)
library(testthat)
## debug(nCompile)
## debug(nCompile_nFunction)
## debug(nCompile_nClass)

test_that("Compile one nFunction via nCompile, returning a list (and testing external R name invalid for C++).",
{ 
  add.Scalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  test <- nCompile(add.Scalars, returnList = TRUE)
  test <- nCompile2(add.Scalars, returnList = TRUE)
  expect_equal(test$add.Scalars(2, 3), 5)
}
)

test_that("Compile one nFunction via nCompile, not returning a list (and testing internal name invalid for C++).",
{ 
  addScalars <- nFunction(
    name = "add.Scalars",
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  test <- nCompile(addScalars, returnList = FALSE)
  expect_equal(test(2, 3), 5)
})

test_that("Compile two nFunctions via nCompile, returning a list.",
{ 
  addScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  multScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x * y
      return(ans)
    }
  )
  test <- nCompile(addScalars, multScalars)
  expect_equal(test$addScalars(2, 3), 5)
  expect_equal(test$multScalars(2, 3), 6)
}
)

test_that("Compile two nFunctions via nCompile provided as a list, returning a list.",
{ 
  addScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  multScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x * y
      return(ans)
    }
  )
  test <- nCompile(list(f1 = addScalars, f2 = multScalars))
  expect_equal(test$f1(2, 3), 5)
  expect_equal(test$f2(2, 3), 6)
})

test_that("Compile one nClass via nCompile provided as a list, returning not as list (and checking R to C++ conversions of class, method, and member names).",
{
  # Some other variants are in the "...various ways..." test below
  nc <- nClass(
    classname = "nc.1",
    Cpublic = list(
      v.1 = 'numericVector',
      go.1 = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v.1)
        },
        returnType = 'numericVector'
      )
    )
  )
  Cnc <- nCompile(list(nc = nc))
  Cnc <- nCompile2(list(nc = nc))
  Cnc1 <- Cnc$new()
  Cnc1$v.1 <- 1:3
  expect_equal(Cnc1$go.1(2), 2*(1:3))
})

test_that("nCompile naming and interface choices work in various ways",
{
  nc1 <- nClass(
    classname = "nc1_",
    Cpublic = list(
      x = 'numericScalar'
    )
  )
  
  nc2 <- nClass(
    Cpublic = list(
      y = 'numericScalar'
    )
  )
  
  nc3 <- nClass(
    Cpublic = list(
      z = 'numericScalar'
    )
  )

  # Basic use
  #nOptions(showCompilerOutput = TRUE)
  comp <- nCompile(nc1, nc2)
  expect_identical(names(comp), c("nc1", "nc2"))
  expect_true(inherits(comp$nc1$new(), "nClass"))
  expect_true(inherits(comp$nc2$new(), "nClass"))
 
  # One named element in the ..., and generic interface for ALL
  comp <- nCompile(nc1x = nc1, nc2,
                   interfaces = "generic")
  expect_identical(names(comp), c("nc1x", "nc2"))
  expect_true(class(comp$nc1())=="loadedObjectEnv")
  expect_true(class(comp$nc2())=="loadedObjectEnv")

  # One named element in the ..., and different interface choices
  comp <- nCompile(nc1x = nc1, nc2,
                   interfaces = c(nc1x = "full", nc2 = "generic"))
  expect_identical(names(comp), c("nc1x", "nc2"))
  expect_true(inherits(comp$nc1x$new(), "CnClass"))
  expect_true(class(comp$nc2())=="loadedObjectEnv")

  # Call with singleton does not return a list
  comp <- nCompile(nc1)
  expect_true(inherits(comp$new(), "CnClass"))

  # Option to return a list with a singleton
  comp <- nCompile(nc1, returnList = TRUE)
  expect_true(inherits(comp$nc1$new(), "CnClass"))

  # Provide compilation units as a named list
  comp <- nCompile(list(nc1 = nc1, nc2 = nc2), interfaces = "generic")
  expect_identical(names(comp), c("nc1", "nc2"))
  expect_true(class(comp$nc1())=="loadedObjectEnv")
  expect_true(class(comp$nc2())=="loadedObjectEnv")

  # Error if a list is not completely named
  expect_error(comp <- nCompile(list(nc1 = nc1, nc2))) ## expect error due to only partial naming in list

  # Mix of named list and individual unit, both in ...
  comp <- nCompile(list(nc1 = nc1, nc3 = nc3), nc2, interfaces = "generic")
  expect_identical(names(comp), c("nc1", "nc3", "nc2"))
  expect_true(class(comp$nc1())=="loadedObjectEnv")
  expect_true(class(comp$nc2())=="loadedObjectEnv")
  expect_true(class(comp$nc3())=="loadedObjectEnv")
  
  # Move on to nFunctions  
  nfA <- nFunction(
    name = "nfA_",
    fun = function() {
      return(2)
      returnType('integerScalar')
    })
  
  nfB <- nFunction(
    fun = function() {
      return(nfA())
      returnType('integerScalar')
    })
  
  nfC <- nFunction(
    fun = function() {
      return(nfB())
      returnType('integerScalar')
    })

  # Basic use
  #debug(nCompile)
  comp <- nCompile(nfB, nfA)
  expect_identical(names(comp), c("nfB", "nfA"))
  expect_true(is.function(comp$nfB))
  expect_true(is.function(comp$nfA))
  expect_equal(comp$nfB(), 2)
  # Singleton
  comp <- nCompile(nfA)
  expect_true(is.function(comp))

  # Singleton returned as list
  comp <- nCompile(nfA, returnList = TRUE)
  expect_identical(names(comp), c("nfA"))
  expect_true(is.function(comp$nfA))

  # One item named, the other not, in ...
  comp <- nCompile(f2 = nfB, nfA)
  expect_identical(names(comp), c("f2", "nfA"))
  expect_true(is.function(comp$f2))
  expect_true(is.function(comp$nfA))

  # Error from incompletely named list
  expect_error(comp <- nCompile(list(f2 = nfB, nfA))) # expected error due to incompletely named list 

  # Fully named list
  comp <- nCompile(list(f2 = nfB, f1 = nfA)) 
  expect_identical(names(comp), c("f2", "f1"))
  expect_true(is.function(comp$f2))
  expect_true(is.function(comp$f1))

  # Mix of list and individual item, both in ...
  comp <- nCompile(list(f2 = nfB, f3 = nfC), nfA) 
  expect_identical(names(comp), c("f2", "f3", "nfA"))
  expect_true(is.function(comp$f2))
  expect_true(is.function(comp$f3))
  expect_true(is.function(comp$nfA))

  # Mix of nFunction and nClass
  comp <- nCompile(nfA, nc1)
  expect_identical(names(comp), c("nfA", "nc1"))
  expect_true(is.function(comp$nfA))
  expect_true(inherits(comp$nc1$new(), "nClass"))
})
