# Test code needed for new nimbleModel system.
# Some or all of this should eventually go in a separate package.

library(nCompiler)
library(testthat)

test_that("toy nimble model prototype works", {
  varInfoM <- list(list(name = "beta", nDim  = 1), list(name = "mu", nDim = 0),
                   list(name = "gamma", nDim = 2))

  #debug(makeModel_nClass)
  ncm1 <- makeModel_nClass(varInfoM)

  varInfo <- list(list(name = "x", nDim  = 0), list(name = "mu", nDim = 1),
                  list(name = "sd", nDim = 0))
  node_dnorm <- make_node_fun(varInfo)

  Cncm1 <- nCompile(ncm1, node_dnorm)

  obj <- Cncm1$ncm1$new()
  obj$call_setup_node_mgmt()
  nodeObj <- obj$beta_node
  obj$beta <- 1:3
  expect_equal(obj$beta, 1:3)

  obj$set_from_list(list(beta = 10:11))
  obj$set_from_list(list(mu = 110, beta = 11:20, alpha = 101))
  obj$mu

  obj$resize_from_list(list(beta = 7))
  expect_error(obj$resize_from_list(list(beta = 5, mu = 3, gamma = c(2, 4))))
  obj$resize_from_list(list(beta = 5, gamma = c(2, 4)))
  expect_equal(obj$beta, rep(0, 5))
  expect_equal(obj$gamma, matrix(0, nrow = 2, ncol = 4))
})
