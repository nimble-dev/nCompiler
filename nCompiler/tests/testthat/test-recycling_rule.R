context("Testing of recycling rule for distribution functions in nCompiler code")

test_that('Basics of recycling rule work',
{
  nf <- nFunction(
    function(x = numericVector(), y = numericVector(), mean = numericVector(),
             sd = integerVector(), log = logicalScalar()) {
      ans = dnorm(x + y, exp(mean), sd, log)
      returnType(numericVector())
      return(ans)
    }
  )
  nfC <- nCompile_nFunction(nf)
  x <- rnorm(4)
  y <- rnorm(4)
  mean <- rnorm(10)
  sd <- 1L:7L
  expect_equal(
    nfC(x, y, mean, sd, FALSE),
    as.array(dnorm(x + y, exp(mean), sd))
  )
  expect_equal(
    nfC(x, y, mean, sd, TRUE),
    as.array(dnorm(x + y, exp(mean), sd, log = TRUE))
  )
})
