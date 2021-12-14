# example working with an Intrinsic Gaussian Markov Random Field (IGMRF), as 
# described in Chapter 3 of Rue and Held (2005). IGMRFs typically serve as
# improper prior distributions for latent Gaussian fields, adding smoothness 
# constraints to the latent variables.  As an improper prior, care must be taken 
# to simulate IGMRF's and evaluate their density.

set.seed(2021)

library(spam)
library(Matrix)

# number of variables to sample
nsamples <- 1e4
# random variable dimension
n <- 10
# IGMRF order
k <- 1

# precision matrix for a random walk
Q <- as.dgCMatrix.spam(precmat.RW1(n = n))

rigmrf = function(n, Q) {
  # Sample random variables for the mean-zero IGMRF with precision matrix Q 
  # following Rue and Held (2005) Algorithm 3.1.
  #
  # Parameters:
  #  n - sample size
  #  Q - (semi-definite) precision matrix
  
  # random vector dimension
  d <- nrow(Q)
  
  # initialize result, storing random vectors in columns of y
  y <- matrix(nrow = d, ncol = n)
  
  # decomposition
  Qe <- eigen(Q)
  sd <- sqrt(1/Qe$values)
  
  # matrix rank
  k <- sum(sd < Inf)
  
  # draw samples
  onevec <- rep(1, k)
  D <- Diagonal(n = k)
  for(i in 1:n) {
    diag(D) <- rnorm(n = k, sd = sd)
    y[,i] <- as.matrix(Qe$vectors[,1:k] %*% D %*% onevec)
  }
  
  return(y)
}

digmrf = function(x, Q, log) {
  # Density for an IGMRF following Rue and Held(2005) eq. 3.18
  #
  # Parameters:
  #  x - random vector
  #  Q - precision matrix
  #  log - TRUE to return log-density
  
  # random vector dimension
  n <- length(x)
  
  # decomposition
  Qe <- eigen(Q)
  
  # matrix rank
  k <- n - sum(Qe$values > 0)
  
  # log-determinant of Q
  ldetQ <- sum(log(Qe$values[1:(n-k)]))
  
  res <- - (n-k) / 2 * log(2*pi) + .5 * ldetQ - .5 * t(x) %*% Q %*% x
  
  if(log) {
    return(res)
  } else {
    return(exp(res))
  }
}

# sample random variables
x <- rigmrf(n = nsamples, Q = Q)

# expect equal to 0, numerically
max(abs(colMeans(x)))

# expect equal to 0:9
round(apply(
  apply(x, 2, function(x) x - x[1]),
  1,
  var
))

digmrf(x = x[,1] - x[1,1], Q = Q, log = TRUE)
digmrf(x = x[,1], Q = Q, log = TRUE)


# #
# # visualize RW patterns
# #
# 
# library(dplyr)
# library(tidyr)
# library(ggplot2)
# library(ggthemes)
# 
# data.frame(t(apply(x, 2, function(x) x - x[1]))) %>% 
#   mutate(sample = 1:ncol(x)) %>% 
#   sample_n(size = 500) %>% 
#   pivot_longer(
#     cols = starts_with('X'), 
#     names_to = 'time',
#     values_to = 'y',
#     names_prefix = 'X'
#   ) %>% 
#   ggplot(aes(x = as.numeric(time), y = y, group = sample)) + 
#   geom_line(alpha = .1) + 
#   scale_x_continuous(breaks = 0:10) + 
#   theme_few()
