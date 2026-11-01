test_that("is_na and is_nan work", {

  nf <- nFunction(
    fun = function(x = 'numericVector')  {
      result <- is_na(x)
      return(result)
    }, returnType = 'logicalVector')
  cnf <- nCompile(nf)
  x <- c(1.5,NA,2,NA,NaN,1.5)
  expect_identical(is.na(x), nf(x))
  expect_identical(is.na(x), cnf(x))

  nf <- nFunction(
    fun = function(x = 'numericMatrix')  {
      result <- is_na(x)
      return(result)
    }, returnType = 'logicalMatrix')
  cnf <- nCompile(nf)
  x2 <- matrix(rnorm(4),2)
  x2[1,1] <- x2[2,2] <- NA
  expect_identical(is.na(x2), nf(x2))
  expect_identical(is.na(x2), cnf(x2))

  nf <- nFunction(
    fun = function(x = 'numericVector')  {
      result <- is_nan(x)
      return(result)
    }, returnType = 'logicalVector')
  cnf <- nCompile(nf)
  expect_identical(is.nan(x), nf(x))
  expect_identical(is.nan(x), cnf(x))

  nf <- nFunction(
    fun = function(x = 'numericVector')  {
      result <- is.na(x)
      return(result)
    }, returnType = 'logicalVector')
  cnf <- nCompile(nf)
  expect_identical(is.na(x), nf(x))
  expect_identical(is.na(x), cnf(x))

  nf <- nFunction(
    fun = function(x = 'integerVector')  {
      result <- is_na(x)
      return(result)
    }, returnType = 'logicalVector')
  cnf <- nCompile(nf)
  x <- c(1L, NA, 3L)
  expect_identical(is.na(x), nf(x))
  expect_error(expect_identical(is.na(x), cnf(x)))
  # Same error occurs in nimble, presumably because an NA_double is not the same as NA_int.

})


test_that("any_na and any_nan work", {

  nf <- nFunction(
    fun = function(x = 'numericVector')  {
      result <- any_na(x)
      return(result)
    }, returnType = 'logicalScalar')
  cnf <- nCompile(nf)
  x <- c(1.5,NA,2,NA,NaN,1.5)
  expect_identical(any(is.na(x)), nf(x))
  expect_identical(any(is.na(x)), cnf(x))
  y <- c(1.5, NaN)
  expect_identical(any(is.na(y)), nf(y))
  expect_identical(any(is.na(y)), cnf(y))
  w <- c(1.5, NA)
  expect_identical(any(is.na(w)), nf(w))
  expect_identical(any(is.na(w)), cnf(w))
  z <- c(1.5, 2.5)
  expect_identical(any(is.na(z)), nf(z))
  expect_identical(any(is.na(z)), cnf(z))
  
  nf <- nFunction(
    fun = function(x = 'numericVector')  {
      result <- any_nan(x)
      return(result)
    }, returnType = 'logicalScalar')
  cnf <- nCompile(nf)

  expect_identical(any(is.nan(x)), nf(x))
  expect_identical(any(is.nan(x)), cnf(x))
  expect_identical(any(is.nan(y)), nf(y))
  expect_identical(any(is.nan(y)), cnf(y))
  expect_identical(any(is.nan(w)), nf(w))
  expect_identical(any(is.nan(w)), cnf(w))
  expect_identical(any(is.nan(z)), nf(z))
  expect_identical(any(is.nan(z)), cnf(z))
  

})
