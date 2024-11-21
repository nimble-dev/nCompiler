
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
  nfC <- nCompile(nf)
  x <- rnorm(4)
  y <- rnorm(4)
  mean <- rnorm(10)
  sd <- 1L:7L
  expect_equal(
    nfC(x, y, mean, sd, FALSE),
    dnorm(x + y, exp(mean), sd)
  )
  expect_equal(
    nfC(x, y, mean, sd, TRUE),
    dnorm(x + y, exp(mean), sd, log = TRUE)
  )
})
