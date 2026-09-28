# Tests of multivariate distributions

library(nCompiler)
library(testthat)

test_that("multivariate dists work", {
  nc <- nClass(
    Cpublic = list(
      dmnc = nFunction(
        function(x = double(1), mean = double(1), cholesky = double(2),
                 prec_param = logical(), log = logical()) {
          ans <- dmnorm_chol(x, mean, cholesky, prec_param, log)
          return(ans); returnType(double())
        }
      ),
      rmnc = nFunction(
        function(mean = double(1), cholesky = double(2),
                 prec_param = logical()) {
          ans <- rmnorm_chol(n = 1, mean, cholesky, prec_param)
          return(ans); returnType(double(1))
        }
      ),
      dmvtc = nFunction(
        function(x = double(1), mu = double(1), cholesky = double(2), df = integer(),
                 prec_param = logical(), log = logical()) {
          ans <- dmvt_chol(x, mu, cholesky, df, prec_param, log)
          return(ans); returnType(double())
        }
      ),
      rmvtc = nFunction(
        function(mu = double(1), cholesky = double(2), df = integer(),
                 prec_param = logical()) {
          ans <- rmvt_chol(n = 1, mu, cholesky, df, prec_param)
          return(ans); returnType(double(1))
        }
      ),
      dwishc = nFunction(
        function(x = double(2), cholesky = double(2), df = integer(),
                 scale_param = logical(), log = logical()) {
          ans <- dwish_chol(x, cholesky, df, scale_param, log)
          return(ans); returnType(double())
        }
      ),
      rwishc = nFunction(
        function(cholesky = double(2), df = integer(),
                 scale_param = logical()) {
          ans <- rwish_chol(n = 1, cholesky, df, scale_param)
          return(ans); returnType(double(2))
        }
      ),
      ddirch_ = nFunction(
        function(x = double(1), alpha = double(1),
                 log = logical()){
          ans <- ddirch(x, alpha, log)
          return(ans); returnType(double())
        }
      ),
      rdirch_ = nFunction(
        function(alpha = double(1)){
          ans <- rdirch(n = 1, alpha)
          return(ans); returnType(double(1))
        }
      ),
      dmulti_ = nFunction(
        function(x = double(1), prob = double(1),
                 log = logical()){
          ans <- dmulti(x, size = sum(x), prob = prob, log = log)
          return(ans); returnType(double())
        }
      ),
      rmulti_ = nFunction(
        function(size = integer(), prob = double(1)){
          ans <- rmulti(n = 1, size = size, prob = prob)
          return(ans); returnType(double(1))
        }
      )
    )
  )

  comp <- nCompile(nc)
  set.seed(1)
  x <- rnorm(3)
  mean <- 1:3
  cov <- matrix(c(1, 0.5, 0.25, 0.5, 1, 0.5, 0.25, 0.5, 1), byrow = TRUE, nrow = 3)
  chol_cov <- chol(cov)
  prec <- solve(cov)
  chol_prec <- chol(prec)
  obj <- nc$new()
  Cobj <- comp$new()
  # dmnorm
  expect_equal(obj$dmnc(x, mean, chol_cov, FALSE, FALSE),
               Cobj$dmnc(x, mean, chol_cov, FALSE, FALSE))
  expect_equal(obj$dmnc(x, mean, chol_cov, FALSE, TRUE),
               Cobj$dmnc(x, mean, chol_cov, FALSE, TRUE))
  expect_equal(obj$dmnc(x, mean, chol_prec, TRUE, FALSE),
               Cobj$dmnc(x, mean, chol_prec, TRUE, FALSE))
  expect_equal(obj$dmnc(x, mean, chol_prec, TRUE, TRUE),
               Cobj$dmnc(x, mean, chol_prec, TRUE, TRUE))
  expect_equal(Cobj$dmnc(x, mean, chol_cov, FALSE, TRUE),
               Cobj$dmnc(x, mean, chol_prec, TRUE, TRUE))

  # rmnorm
  expect_equal(
  {set.seed(10); obj$rmnc(mean, chol_cov, FALSE)},
  {set.seed(10); Cobj$rmnc(mean, chol_cov, FALSE)})
  expect_equal(
  {set.seed(10); obj$rmnc(mean, chol_prec, TRUE)},
  {set.seed(10); Cobj$rmnc(mean, chol_prec, TRUE)})

  # dmvt
  expect_equal(obj$dmvtc(x, mean, chol_cov, 5, FALSE, FALSE),
               Cobj$dmvtc(x, mean, chol_cov, 5, FALSE, FALSE))
  expect_equal(obj$dmvtc(x, mean, chol_cov, 5, FALSE, TRUE),
               Cobj$dmvtc(x, mean, chol_cov, 5, FALSE, TRUE))
  expect_equal(obj$dmvtc(x, mean, chol_prec, 5, TRUE, FALSE),
               Cobj$dmvtc(x, mean, chol_prec, 5, TRUE, FALSE))
  expect_equal(obj$dmvtc(x, mean, chol_prec, 5, TRUE, TRUE),
               Cobj$dmvtc(x, mean, chol_prec, 5, TRUE, TRUE))
  expect_equal(Cobj$dmvtc(x, mean, chol_cov, 5, FALSE, TRUE),
               Cobj$dmvtc(x, mean, chol_prec, 5, TRUE, TRUE))

  # rmvt
  expect_equal(
  {set.seed(10); obj$rmvtc(mean, chol_cov, 5, FALSE)},
  {set.seed(10); Cobj$rmvtc(mean, chol_cov, 5, FALSE)})
  expect_equal(
  {set.seed(10); obj$rmnc(mean, chol_prec, TRUE)},
  {set.seed(10); Cobj$rmnc(mean, chol_prec, TRUE)})

  # dwish
  x <- matrix(c(0.9, 0.45, 0.2, 0.45, 0.9, 0.45, 0.2, 0.45, 0.9), nrow = 3, byrow =TRUE)
  expect_equal(obj$dwishc(x, chol_cov, 5, FALSE, FALSE),
               Cobj$dwishc(x, chol_cov, 5, FALSE, FALSE))
  expect_equal(obj$dwishc(x, chol_cov, 5, FALSE, TRUE),
               Cobj$dwishc(x, chol_cov, 5, FALSE, TRUE))
  expect_equal(obj$dwishc(x, chol_prec, 5, TRUE, FALSE),
               Cobj$dwishc(x, chol_prec, 5, TRUE, FALSE))
  expect_equal(obj$dwishc(x, chol_prec, 5, TRUE, TRUE),
               Cobj$dwishc(x, chol_prec, 5, TRUE, TRUE))
  expect_equal(Cobj$dwishc(x, chol_cov, 5, FALSE, TRUE),
               Cobj$dwishc(x, chol_prec, 5, TRUE, TRUE))

  #rwish
  expect_equal(
  {set.seed(10); obj$rwishc(chol_cov, 5, FALSE)},
  {set.seed(10); Cobj$rwishc(chol_cov, 5, FALSE)})
  expect_equal(
  {set.seed(10); obj$rwishc(chol_prec, 5, TRUE)},
  {set.seed(10); Cobj$rwishc(chol_prec, 5, TRUE)})

  # ddirch
  alpha <- c(3, 4, 2, 6)
  x <- c(0.4, 0.1, 0.2, 0.3)
  expect_equal(obj$ddirch_(x, alpha, FALSE),
               Cobj$ddirch_(x, alpha, FALSE))
  expect_equal(obj$ddirch_(x, alpha, TRUE),
               Cobj$ddirch_(x, alpha, TRUE))

  # rdirch
  expect_equal(
  {set.seed(10); obj$rdirch_(alpha)},
  {set.seed(10); Cobj$rdirch_(alpha)})

  # dmulti
  x <- c(3, 4, 2, 6)
  prob <- c(0.4, 0.1, 0.2, 0.3)
  expect_equal(obj$dmulti_(x, prob, FALSE),
               Cobj$dmulti_(x, prob, FALSE))
  expect_equal(obj$dmulti_(x, prob, TRUE),
               Cobj$dmulti_(x, prob, TRUE))


  # rmulti
  expect_equal(
  {set.seed(10); obj$rmulti_(50, prob)},
  {set.seed(10); Cobj$rmulti_(50, prob)})

  rm(obj, Cobj); gc()

})
