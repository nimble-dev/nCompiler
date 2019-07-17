context("Testing nDerivs and setup_wrt")

test_that('setup_wrt produces correct wrt indices for nFunction', {
  nf <- nFunction(function(x = numericVector(2), y = numericScalar(),
                           z = numericMatrix(size = c(3, 4))) {
    ans = 2*x*x[2] + 3*y*y
    return(ans)
    returnType(numericVector(2))
  })
  wrt1 <- setup_wrt(nf)
  expect_equal(wrt1, 1:15)
  wrt2 <- setup_wrt(nf, wrt = c('x[1:2]', 'x[1]', 'z[2:3, 2]', 'y'))
  expect_equal(wrt2, c(1:2, 1, 8:9, 3))
})

test_that('setup_wrt produces correct wrt indices for compiled nClass method from full interface', {
  nc <- nClass(
    Rpublic = list(),
    Cpublic = list(
      nf = nFunction(function(x = numericVector(2), y = numericScalar(),
                              z = numericMatrix(size = c(3, 4))) {
        ans = 2*x*x[2] + 3*y*y
        return(ans)
        returnType(numericVector(2))
      })
    )
  )
  nc <- nCompile(nc)
  nc_obj <- nc$new()
  wrt1 <- setup_wrt(nc_obj$nf)
  expect_equal(wrt1, 1:15)
  wrt2 <- setup_wrt(nc_obj$nf, wrt = c('x[1:2]', 'x[1]', 'z[2:3, 2]', 'y'))
  expect_equal(wrt2, c(1:2, 1, 8:9, 3))
})

test_that('setup_wrt produces correct wrt indices for compiled nClass method from generic interface', {
  nc <- nClass(
    Rpublic = list(),
    Cpublic = list(
      nf = nFunction(function(x = numericVector(2), y = numericScalar(),
                              z = numericMatrix(size = c(3, 4))) {
        ans = 2*x*x[2] + 3*y*y
        return(ans)
        returnType(numericVector(2))
      })
    )
  )
  nc <- nCompile_nClass(nc, interface = 'generic')
  nc_obj <- nc()
  wrt1 <- setup_wrt(method(nc_obj, 'nf'))
  expect_equal(wrt1, 1:15)
  wrt2 <- setup_wrt(method(nc_obj, 'nf'), wrt = c('x[1:2]', 'x[1]', 'z[2:3, 2]', 'y'))
  expect_equal(wrt2, c(1:2, 1, 8:9, 3))
})

test_that('nDerivs produces correct result for nFunction', {
  nf <- nFunction(function(x = numericVector(2), y = numericScalar()) {
    ans = 2*x + y
    return(ans)
    returnType(numericVector(2))
  })
  a <- 1:2
  b <- 4
  derivs <- nDerivs(nf(a, b)) ## uncompiled
  expect_equal(derivs$value, c(nf(a, b)))
  expect_equal(derivs$gradient, matrix(c(2, 0, 0, 2, 1, 1), nrow = 2))
  ## known issue: genD hessians are not very accurate
  expect_equal(derivs$hessian, array(rep(0, 3*3*2), dim = c(3, 3, 2)),
               tolerance = 1e-07)
})

test_that('nDerivs produces correct result for nClass generator method', {
  nc <- nClass(
    Rpublic = list(),
    Cpublic = list(
      nf = nFunction(function(x = numericVector(2), y = numericScalar()) {
        ans = 2*x + y
        return(ans)
        returnType(numericVector(2))
      })
    )
  )
  a <- 1:2
  b <- 4
  derivs <- nDerivs(nc$public_methods$nf(a, b)) ## uncompiled
  expect_equal(derivs$value, c(nc$public_methods$nf(a, b)))
  expect_equal(derivs$gradient, matrix(c(2, 0, 0, 2, 1, 1), nrow = 2))
  expect_equal(derivs$hessian, array(rep(0, 3*3*2), dim = c(3, 3, 2)),
               tolerance = 1e-07)
})

test_that('nDerivs produces correct result for compiled nClass method from full interface', {
  set_nOption('automaticDerivatives', TRUE)
  nc <- nClass(
    Rpublic = list(),
    Cpublic = list(
      nf = nFunction(function(x = numericVector(2), y = numericScalar()) {
        ans = 2*x + y
        return(ans)
        returnType(numericVector(2))
      })
    ),
    enableDerivs = 'nf'
  )
  a <- 1:2
  b <- 4
  derivs <- nDerivs(nc$public_methods$nf(a, b)) ## uncompiled
  nc_full <- nCompile_nClass(nc)
  nc_full_obj <- nc_full$new()
  Cderivs <- nDerivs(nc_full_obj$nf(a, b))
  expect_equivalent(derivs$value, Cderivs$value)
  expect_equal(derivs$gradient, Cderivs$gradient)
  expect_equal(derivs$hessian, Cderivs$hessian, tolerance = 1e-07)
})

test_that('nDerivs produces correct result for compiled nClass method from generic interface', {
  set_nOption('automaticDerivatives', TRUE)
  nc <- nClass(
    Rpublic = list(),
    Cpublic = list(
      nf = nFunction(function(x = numericVector(2), y = numericScalar()) {
        ans = 2*x + y
        return(ans)
        returnType(numericVector(2))
      })
    ),
    enableDerivs = 'nf'
  )
  a <- 1:2
  b <- 4
  derivs <- nDerivs(nc$public_methods$nf(a, b)) ## uncompiled
  nc_generic <- nCompile_nClass(nc, interface = 'generic')
  nc_generic_obj <- nc_generic()
  Cderivs <- nDerivs(method(nc_generic_obj, 'nf')(a, b))
  expect_equivalent(derivs$value, value(Cderivs, 'value'))
  expect_equal(derivs$gradient, value(Cderivs, 'gradient'))
  expect_equal(derivs$hessian, value(Cderivs, 'hessian'), tolerance = 1e-07)
})
