# Indexing the results of vectorized distribution calls and other tensor
# expressions, e.g. dnorm(x, m, s)[i], (x + y)[1], runif(n, 0, 1)[2:3].

test_that("single and range indexing of d/p/q function results work", {
  f_d1 <- nFunction(
    function(x = numericVector(), i = integerScalar()) {
      ans <- dunif(x, 0, 1)[i]
      return(ans)
    },
    returnType = "numericScalar"
  )
  f_drange <- nFunction(
    function(x = numericVector()) {
      ans <- dunif(x, 0, 1)[1:2]
      return(ans)
    },
    returnType = "numericVector"
  )
  f_dnormlog <- nFunction(
    function(x = numericVector(), m = numericVector()) {
      ans <- dnorm(x, m, 2, log = TRUE)[2]
      return(ans)
    },
    returnType = "numericScalar"
  )
  f_dgamma <- nFunction(
    function(x = numericVector()) {
      ans <- dgamma(x, 2, rate = 3)[3]
      return(ans)
    },
    returnType = "numericScalar"
  )
  f_pgamma <- nFunction(
    function(x = numericVector()) {
      ans <- pgamma(x, 2, rate = 3, lower.tail = FALSE)[2]
      return(ans)
    },
    returnType = "numericScalar"
  )
  f_besselK <- nFunction(
    function(x = numericVector()) {
      ans <- besselK(x, 1.5, TRUE)[2]
      return(ans)
    },
    returnType = "numericScalar"
  )
  comp <- nCompile(f_d1, f_drange, f_dnormlog, f_dgamma, f_pgamma, f_besselK)
  x <- c(0.1, 0.5, 2)
  m <- c(1, 2, 3)
  expect_equal(comp$f_d1(x, 3L), dunif(x, 0, 1)[3])
  expect_equal(comp$f_drange(x), dunif(x, 0, 1)[1:2])
  expect_equal(comp$f_dnormlog(x, m), dnorm(x, m, 2, log = TRUE)[2])
  expect_equal(comp$f_dgamma(x), dgamma(x, 2, rate = 3)[3])
  expect_equal(comp$f_pgamma(x), pgamma(x, 2, rate = 3, lower.tail = FALSE)[2])
  expect_equal(comp$f_besselK(x), besselK(x, 1.5, TRUE)[2])
})

test_that("a vector argument where a scalar is required gives an error", {
  f_badlog <- nFunction(
    function(x = numericVector(), lg = logicalVector()) {
      ans <- dnorm(x, 0, 1, log = lg)
      return(ans)
    },
    returnType = "numericVector"
  )
  expect_error(nCompile(f_badlog), "must be scalar")
})

test_that("single-element indexing of tensor expressions works", {
  f_vec <- nFunction(
    function(x = numericVector(), y = numericVector()) {
      ans <- (x + y)[2]
      return(ans)
    },
    returnType = "numericScalar"
  )
  f_mat <- nFunction(
    function(A = numericMatrix(), B = numericMatrix()) {
      ans <- (A + B)[1, 2]
      return(ans)
    },
    returnType = "numericScalar"
  )
  comp <- nCompile(f_vec, f_mat)
  expect_equal(comp$f_vec(c(1, 2, 3), c(10, 20, 30)), 22)
  A <- matrix(as.numeric(1:4), 2)
  B <- matrix(as.numeric(5:8), 2)
  expect_equal(comp$f_mat(A, B), (A + B)[1, 2])
})

test_that("indexed r function results use the full random draw sequence", {
  f_r1 <- nFunction(
    function() {
      ans <- runif(5, 0, 1)[1]
      return(ans)
    },
    returnType = "numericScalar"
  )
  f_rrange <- nFunction(
    function() {
      ans <- runif(5, 0, 1)[2:3]
      return(ans)
    },
    returnType = "numericVector"
  )
  f_rseq <- nFunction(
    function(m = numericVector()) {
      a <- rnorm(4, m, 1)[4]
      b <- runif(3, 0, 1)
      c2 <- rgamma(4, 2, rate = 3)[2]
      ans <- c(a, b, c2)
      return(ans)
    },
    returnType = "numericVector"
  )
  comp <- nCompile(f_r1, f_rrange, f_rseq)
  m <- c(1, 2, 3)
  set.seed(1); expected <- f_r1()
  set.seed(1); expect_equal(comp$f_r1(), expected)
  set.seed(1); expected <- f_rrange()
  set.seed(1); expect_equal(comp$f_rrange(), expected)
  set.seed(1); expected <- runif(5)[2:3]
  set.seed(1); expect_equal(comp$f_rrange(), expected)
  set.seed(1); expected <- f_rseq(m)
  set.seed(1); expect_equal(comp$f_rseq(m), expected)
})
