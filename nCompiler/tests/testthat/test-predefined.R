# See generatePredefinedCpp.R for instructions on adding predefined nClasses to the package.
# This file is not in the R source directory.  It is in the GitHub repository
# above the package directory.

test_that("test_predefined class works",
{
  # Generate new code.
  ctest <- nCompile(test_predefined, control = list(generate_predefined = TRUE))
  obj <- ctest$new()
  obj$a <- 1.2
  expect_equal(obj$a, 1.2)
  # uncompiled
  obj <- test_predefined$new()
  obj$a <- 1.2
  expect_equal(obj$a, 1.2)
  
  # Use existing (predefined) code
  ctest <- nCompile(test_predefined, control = list(generate_predefined = FALSE))
  obj <- ctest$new()
  obj$a <- 1.2
  expect_equal(obj$a, 1.2)
  
  # Default to existing (predefined) code
  # We could add a test to confirm that it is actually the predefined code that is used.
  ctest <- nCompile(test_predefined)
  obj <- ctest$new()
  obj$a <- 1.2
  expect_equal(obj$a, 1.2)

  # Use in a class as member data
  nc1 <- nClass(
    classname = "nc1",
    Cpublic = list(
      x = 'test_predefined'
    ))
  Cnc1 <- nCompile(nc1, test_predefined)
  obj <- Cnc1$nc1$new()
  obj$x <- Cnc1$test_predefined$new()
  obj$x$a <- 1.2
  expect_equal(obj$x$a, 1.2)
  #uncompiled
  obj <- nc1$new()
  obj$x <- test_predefined$new()
  obj$x$a <- 1.2
  expect_equal(obj$x$a, 1.2)

  # Use in a class as return object
  nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    foo = nFunction(
      fun = function() {
        return(test_predefined$new())
      },
      returnType = 'test_predefined'
    )
  ))
  Cnc1 <- nCompile(nc1, test_predefined)
  obj <- Cnc1$nc1$new()
  x <- obj$foo()
  x$a <- 1.2
  expect_equal(x$a, 1.2)
  #uncompiled
  obj <- nc1$new()
  x <- obj$foo()
  x$a <- 1.2
  expect_equal(x$a, 1.2)

  # Use in a class as input object
  nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    foo = nFunction(
      fun = function(x) {
        x$a <- x$a + 1
        return(x)
      },
      argTypes = list(x = 'test_predefined'),
      returnType = 'test_predefined'
    )
  ))
  Cnc1 <- nCompile(nc1, test_predefined)
  obj <- Cnc1$nc1$new()
  x <- Cnc1$test_predefined$new()
  x$a <- 1.2
  x2 <- obj$foo(x)
  expect_equal(x2$a, 2.2)
  #uncompiled
  obj <- nc1$new()
  x <- test_predefined$new()
  x$a <- 1.2
  x2 <- obj$foo(x)
  expect_equal(x2$a, 2.2)

  # Use in a function
  f1 <- nFunction(
    fun = function(x) {
      x$a <- x$a + 1
      return(x)
    },
    argTypes = list(x = 'test_predefined'),
    returnType = 'test_predefined'
  )
  Cf1 <- nCompile(f1, test_predefined)
  x <- Cnc1$test_predefined$new()
  x$a <- 1.2
  x2 <- Cf1$f1(x)
  expect_equal(x2$a, 2.2)
  # uncompiled
  x <- test_predefined$new()
  x$a <- 1.2
  x2 <- f1(x)
  expect_equal(x2$a, 2.2)
  
  # Call C++ test function that relies on correctly reading the header  
  get_test_predefined <- nFunction(
    fun = function() {
      cppLiteral("std::shared_ptr<test_predefined> A = make_test_predefined();")
      cppLiteral("return(A);")
    },
    returnType = 'test_predefined')  
  C_gtp <- nCompile(get_test_predefined, test_predefined)
  x <- C_gtp$get_test_predefined()
  x$a <- 1.2
  expect_equal(x$a, 1.2)
})

test_that("predefined derivClass class works",
{
  # Generate new code.
  ctest <- nCompile(derivClass, control = list(generate_predefined = TRUE))
  obj <- ctest$new()
  obj$gradient <- matrix(1:4, nrow = 2)
  expect_equal(obj$gradient, matrix(1:4, nrow = 2))
  # uncompiled
  obj <- derivClass$new()
  obj$gradient <- matrix(1:4, nrow = 2)
  expect_equal(obj$gradient, matrix(1:4, nrow = 2))
  
  # Use existing (predefined) code
  ctest <- nCompile(derivClass, control = list(generate_predefined = FALSE))
  obj <- ctest$new()
  obj$gradient <- matrix(1:4, nrow = 2)
  expect_equal(obj$gradient, matrix(1:4, nrow = 2))
 
  # Default to existing (predefined) code
  # We could add a test to confirm that it is actually the predefined code that is used.
  ctest <- nCompile(derivClass)
  obj <- ctest$new()
  obj$gradient <- matrix(1:4, nrow = 2)
  expect_equal(obj$gradient, matrix(1:4, nrow = 2))

  # Use in a class as member data
  nc1 <- nClass(
    classname = "nc1",
    Cpublic = list(
      x = 'derivClass'
    ))
  Cnc1 <- nCompile(nc1, derivClass)
  obj <- Cnc1$nc1$new()
  obj$x <- Cnc1$derivClass$new()
  obj$x$gradient <- matrix(1:4, nrow = 2)
  expect_equal(obj$x$gradient, matrix(1:4, nrow = 2))
  #uncompiled
  obj <- nc1$new()
  obj$x <- derivClass$new()
  obj$x$gradient <- matrix(1:4, nrow = 2)
  expect_equal(obj$x$gradient, matrix(1:4, nrow = 2))

  # Use in a class as return object
  nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    foo = nFunction(
      fun = function() {
        return(derivClass$new())
      },
      returnType = 'derivClass'
    )
  ))
  Cnc1 <- nCompile(nc1, derivClass)
  obj <- Cnc1$nc1$new()
  x <- obj$foo()
  x$gradient <- matrix(1:4, nrow = 2)
  expect_equal(x$gradient, matrix(1:4, nrow = 2))
  #uncompiled
  obj <- nc1$new()
  x <- obj$foo()
  x$gradient <- matrix(1:4, nrow = 2)
  expect_equal(x$gradient, matrix(1:4, nrow = 2))

  # Use in a class as input object
  nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    foo = nFunction(
      fun = function(x) {
        x$gradient <- x$gradient + 1
        return(x)
      },
      argTypes = list(x = 'derivClass'),
      returnType = 'derivClass'
    )
  ))
  Cnc1 <- nCompile(nc1, derivClass)
  obj <- Cnc1$nc1$new()
  x <- Cnc1$derivClass$new()
  x$gradient <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$gradient, matrix(2:5, nrow = 2))
  #uncompiled
  obj <- nc1$new()
  x <- derivClass$new()
  x$gradient <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$gradient, matrix(2:5, nrow = 2))

  # Use in a function
  f1 <- nFunction(
    fun = function(x) {
      x$gradient <- x$gradient + 1
      return(x)
    },
    argTypes = list(x = 'derivClass'),
    returnType = 'derivClass'
  )
  Cf1 <- nCompile(f1, derivClass)
  x <- Cnc1$derivClass$new()
  x$gradient <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$gradient, matrix(2:5, nrow = 2))
  # uncompiled
  x <- derivClass$new()
  x$gradient <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$gradient, matrix(2:5, nrow = 2))
  
  # Call C++ test function that relies on correctly reading the header  
  get_derivClass <- nFunction(
    fun = function() {
      cppLiteral("std::shared_ptr<derivClass> A = make_derivClass();")
      cppLiteral("return(A);")
    },
    returnType = 'derivClass')  
  C_get <- nCompile(get_derivClass, derivClass)
  x <- C_get$get_derivClass()
  x$gradient <- matrix(1:4, nrow = 2)
  expect_equal(x$gradient, matrix(1:4, nrow = 2))
})

test_that("predefined EigenDecomp class works",
{
  # Generate new code.
  ctest <- nCompile(EigenDecomp, control = list(generate_predefined = TRUE))
  obj <- ctest$new()
  obj$vectors <- matrix(1:4, nrow = 2)
  expect_equal(obj$vectors, matrix(1:4, nrow = 2))
  # uncompiled
  obj <- EigenDecomp$new()
  obj$vectors <- matrix(1:4, nrow = 2)
  expect_equal(obj$vectors, matrix(1:4, nrow = 2))
  
  # Use existing (predefined) code
  ctest <- nCompile(EigenDecomp, control = list(generate_predefined = FALSE))
  obj <- ctest$new()
  obj$vectors <- matrix(1:4, nrow = 2)
  expect_equal(obj$vectors, matrix(1:4, nrow = 2))
 
  # Default to existing (predefined) code
  # We could add a test to confirm that it is actually the predefined code that is used.
  ctest <- nCompile(EigenDecomp)
  obj <- ctest$new()
  obj$vectors <- matrix(1:4, nrow = 2)
  expect_equal(obj$vectors, matrix(1:4, nrow = 2))

  # Use in a class as member data
  nc1 <- nClass(
    classname = "nc1",
    Cpublic = list(
      x = 'EigenDecomp'
    ))
  Cnc1 <- nCompile(nc1, EigenDecomp)
  obj <- Cnc1$nc1$new()
  obj$x <- Cnc1$EigenDecomp$new()
  obj$x$vectors <- matrix(1:4, nrow = 2)
  expect_equal(obj$x$vectors, matrix(1:4, nrow = 2))
  #uncompiled
  obj <- nc1$new()
  obj$x <- EigenDecomp$new()
  obj$x$vectors <- matrix(1:4, nrow = 2)
  expect_equal(obj$x$vectors, matrix(1:4, nrow = 2))

  # Use in a class as return object
  nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    foo = nFunction(
      fun = function() {
        return(EigenDecomp$new())
      },
      returnType = 'EigenDecomp'
    )
  ))
  Cnc1 <- nCompile(nc1, EigenDecomp)
  obj <- Cnc1$nc1$new()
  x <- obj$foo()
  x$vectors <- matrix(1:4, nrow = 2)
  expect_equal(x$vectors, matrix(1:4, nrow = 2))
  #uncompiled
  obj <- nc1$new()
  x <- obj$foo()
  x$vectors <- matrix(1:4, nrow = 2)
  expect_equal(x$vectors, matrix(1:4, nrow = 2))

  # Use in a class as input object
  nc1 <- nClass(
  classname = "nc1",
  Cpublic = list(
    foo = nFunction(
      fun = function(x) {
        x$vectors <- x$vectors + 1
        return(x)
      },
      argTypes = list(x = 'EigenDecomp'),
      returnType = 'EigenDecomp'
    )
  ))
  Cnc1 <- nCompile(nc1, EigenDecomp)
  obj <- Cnc1$nc1$new()
  x <- Cnc1$EigenDecomp$new()
  x$vectors <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$vectors, matrix(2:5, nrow = 2))
  #uncompiled
  obj <- nc1$new()
  x <- EigenDecomp$new()
  x$vectors <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$vectors, matrix(2:5, nrow = 2))

  # Use in a function
  f1 <- nFunction(
    fun = function(x) {
      x$vectors <- x$vectors + 1
      return(x)
    },
    argTypes = list(x = 'EigenDecomp'),
    returnType = 'EigenDecomp'
  )
  Cf1 <- nCompile(f1, EigenDecomp)
  x <- Cnc1$EigenDecomp$new()
  x$vectors <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$vectors, matrix(2:5, nrow = 2))
  # uncompiled
  x <- EigenDecomp$new()
  x$vectors <- matrix(1:4, nrow = 2)
  x2 <- obj$foo(x)
  expect_equal(x2$vectors, matrix(2:5, nrow = 2))
  
  # Call C++ test function that relies on correctly reading the header  
  get_EigenDecomp <- nFunction(
    fun = function() {
      cppLiteral("std::shared_ptr<EigenDecomp> A = make_EigenDecomp();")
      cppLiteral("return(A);")
    },
    returnType = 'EigenDecomp')  
  C_get <- nCompile(get_EigenDecomp, EigenDecomp)
  x <- C_get$get_EigenDecomp()
  x$vectors <- matrix(1:4, nrow = 2)
  expect_equal(x$vectors, matrix(1:4, nrow = 2))
  
  doEigen <- nFunction(
    fun = function(x, symmetric, valuesOnly) {
      eigX <- nEigen(x, symmetric, valuesOnly)
      return(eigX)
    },
    argTypes = list(x = 'numericMatrix', symmetric = 'logicalScalar', valuesOnly = 'logicalScalar'),
    returnType = 'EigenDecomp')
  comp <- nCompile(doEigen, EigenDecomp)
  x <- matrix(as.numeric(c(2, 4, 1, 3)), nrow = 2)
  Cres <- comp$doEigen(x, FALSE, FALSE) ## Add some variants with symmetric = TRUE and valuesOnly=TRUE
  Rres <- eigen(x)
  expect_equal(as.numeric(Cres$values), as.numeric(Rres$values))
  for(i in 1:nrow(x)) {
    Cvec <- as.numeric(Cres$vectors[,i])
    Rvec <- as.numeric(Rres$vectors[,i])
    # Must work out a test that allows Cvec == -Rvec or Cvec == Rvec
    # For now:
    print(min( abs(max(Rvec-Cvec)), abs(max(Rvec+Cvec)) )) ## should be 0s or near 0s, showing agreement between R and c++ up to sign flip.
  }
})
