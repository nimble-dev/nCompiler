# example working with a dense multivariate normal distribution

library(testthat)
library(nCompiler)

# reference evaluation for multivariate normal densities
library(nimble)


#
# base functions, parameters, and data
#

set.seed(2021)

# random variable dimension
n <- 10

# generate a random covariance matrix
Sigma <- matrix(data = rnorm(n = n^2), nrow = n)
Sigma <- Sigma %*% t(Sigma)
Sigma.chol <- chol(Sigma)

# generate a random mean vector
mu <- rnorm(n = n)

# draw from the distribution
x <- nimble::rmnorm_chol(
  n = 1, cholesky = Sigma.chol, prec_param = FALSE, mean = mu
)

# evaluate log-likelihood of sample
ll.ref <- nimble::dmnorm_chol(
  x = x, mean = mu, cholesky = Sigma.chol, prec_param = FALSE, log = TRUE
)

dmvn <- function(x, mu, Sigma, log) {
  # Density for a multivariate normal random variable
  #
  # Parameters:
  #  x - random vector
  #  mu - mean vector
  #  Sigma - covariance matrix
  #  log - TRUE to return log-density
  
  # random vector dimension
  n <- length(x)
  
  # cholesky decomposition Sigma = t(R) %*% R
  R <- chol(Sigma)
  
  # log-determinant for chol(Sigma)
  ldet_chol <- sum(log(diag(R)))
    
  # evaluate quadratic form t(x - mu) %*% solve(Sigma) %*% (x - mu)
  y <- forwardsolve(t(R), x - mu)

  res <- -.5 * n * log(2*pi) - ldet_chol - .5 * sum(y*y)
  
  if(log) {
    return(res) 
  } else {
    return(exp(res))
  }
}


#
# nCompiler code
#

nDmvn <- nFunction(
  fun = dmvn,
  argTypes = list(x = 'numericVector', mu = 'numericVector', 
                  Sigma = 'nMatrix', log = 'logical'),
  returnType = 'double'
)

cDmvn <- nCompile(nDmvn)


#
# demonstration
#

# validate the R implementation
expect_equal(dmvn(x = x, mu = mu, Sigma = Sigma, log = TRUE), ll.ref)

# validate the nFunction
expect_equal(nDmvn(x = x, mu = mu, Sigma = Sigma, log = TRUE), ll.ref)

# validate the compiled nFunction
expect_equal(cDmvn(x = x, mu = mu, Sigma = Sigma, ARG_log_ = TRUE), ll.ref)
