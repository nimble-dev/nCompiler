context("tensorOperations: interoperability with dense linear algebra methods")


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

dmvn2 <- function(x, mu, Sigma, log) {
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
  
  res <- -.5 * n * log(2*pi) - ldet_chol - .5 * t(y)%*%y
  
  if(log) {
    return(res) 
  } else {
    return(exp(res))
  }
}

#
# nCompiler code
#

opt <- nOptions()
opt$compilerOptions$cppStacktrace <- TRUE
nOptions(opt)

nDmvn <- nFunction(
  fun = dmvn,
  argTypes = list(x = 'numericVector', mu = 'numericVector', 
                  Sigma = 'nMatrix', log = 'logical'),
  returnType = 'double'
)

nDmvn2 <- nFunction(
  fun = dmvn2,
  argTypes = list(x = 'numericVector', mu = 'numericVector', 
                  Sigma = 'nMatrix', log = 'logical'),
  returnType = 'double'
)

cDmvn <- nCompile(nDmvn)
cDmvn2 <- nCompile(nDmvn2)

#
# demonstration
#

# validate the R implementations
expect_equal(dmvn(x = x, mu = mu, Sigma = Sigma, log = TRUE), ll.ref)
expect_equal(as.numeric(dmvn2(x = x, mu = mu, Sigma = Sigma, log = TRUE)), 
             ll.ref)

# validate the nFunctions
expect_equal(nDmvn(x = x, mu = mu, Sigma = Sigma, log = TRUE), ll.ref)
expect_equal(as.numeric(nDmvn2(x = x, mu = mu, Sigma = Sigma, log = TRUE)), 
             ll.ref)

# validate the compiled nFunctions
expect_equal(cDmvn(x = x, mu = mu, Sigma = Sigma, ARG_log_ = TRUE), ll.ref)
expect_equal(cDmvn2(x = x, mu = mu, Sigma = Sigma, ARG_log_ = TRUE), ll.ref)

# validate that stack tracing occurs on linear algebra errors, such as being 
# unable to compute the Cholesky decomposition for a non-square matrix
expect_error(
  cDmvn(x = x, mu = mu, Sigma = matrix(1, nrow = 3, ncol = 2), ARG_log_ = TRUE)
)
