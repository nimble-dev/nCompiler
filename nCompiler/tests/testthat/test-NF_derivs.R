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
  ncc <- nCompile(nc)
  nc_obj <- ncc$new()
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
  ncc <- nCompile_nClass(nc, interface = 'generic')
  nc_obj <- ncc()
  wrt1 <- setup_wrt(method(nc_obj, 'nf'), NC = nc)
  expect_equal(wrt1, 1:15)
  wrt2 <- setup_wrt(method(nc_obj, 'nf'),
                    wrt = c('x[1:2]', 'x[1]', 'z[2:3, 2]', 'y'), NC = nc)
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
  derivs1 <- nDerivs(nf(a, b)) ## uncompiled
  derivs2 <- nDerivs(nf(a, b), wrt = 1:3) ## wrt arg is numeric vector
  expect_equal(derivs1$value, c(nf(a, b)))
  expect_equal(derivs2$value, c(nf(a, b)))
  expect_equal(derivs1$gradient, matrix(c(2, 0, 0, 2, 1, 1), nrow = 2))
  expect_equal(derivs2$gradient, matrix(c(2, 0, 0, 2, 1, 1), nrow = 2))
  ## known issue: genD hessians are not very accurate
  expect_equal(derivs1$hessian, array(rep(0, 3*3*2), dim = c(3, 3, 2)),
               tolerance = 1e-07)
  expect_equal(derivs2$hessian, array(rep(0, 3*3*2), dim = c(3, 3, 2)),
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
  Cderivs1 <- nDerivs(nc_full_obj$nf(a, b))
  Cderivs2 <- nDerivs(nc_full_obj$nf(a, b), wrt = 1:3) ## wrt is numeric vector
  expect_equivalent(derivs$value, Cderivs1$value)
  expect_equivalent(derivs$value, Cderivs2$value)
  expect_equal(derivs$gradient, Cderivs1$gradient)
  expect_equal(derivs$gradient, Cderivs2$gradient)
  expect_equal(derivs$hessian, Cderivs1$hessian, tolerance = 1e-07)
  expect_equal(derivs$hessian, Cderivs2$hessian, tolerance = 1e-07)
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
  Cderivs1 <- nDerivs(method(nc_generic_obj, 'nf')(a, b), NC = nc)
  Cderivs2 <- nDerivs(method(nc_generic_obj, 'nf')(a, b), NC = nc, wrt = 1:3)
  expect_equivalent(derivs$value, value(Cderivs1, 'value'))
  expect_equivalent(derivs$value, value(Cderivs2, 'value'))
  expect_equal(derivs$gradient, value(Cderivs1, 'gradient'))
  expect_equal(derivs$gradient, value(Cderivs2, 'gradient'))
  expect_equal(derivs$hessian, value(Cderivs1, 'hessian'), tolerance = 1e-07)
  expect_equal(derivs$hessian, value(Cderivs2, 'hessian'), tolerance = 1e-07)
})
