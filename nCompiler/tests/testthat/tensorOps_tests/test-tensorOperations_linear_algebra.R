#tensorOperations: interoperability with dense linear algebra methods

# This now works except nForwardsolve is not found for uncompiled execution.

##
## multivariate normal test
##

test_that("dense forwardsolve/backsolve work in mvn example", {
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

    res <- -.5 * n * log(2*pi) - ldet_chol - .5 * (t(y)%*%y)[1,1]

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
})

##
## triangular and LU solver tests
##

test_that("solve/forwardsolve/backsolve work", {
  # solve wrapper
  lusolve <- function(A, b) {
    x <- solve(A, b)
    return(x)
  }

  # forwardsolve wrapper
  fsolve <- function(L, b) {
    x <- forwardsolve(L, b)
    return(x)
  }

  # backsolve wrapper
  bsolve <- function(U, b) {
    x <- backsolve(U, b)
    return(x)
  }

  # nCompiler implementation of forward solve with unknown matrix
  nLUsolve <- nFunction(
    fun = lusolve,
    argTypes = list(A = 'numericMatrix', b = 'numericVector'),
    returnType = 'numericVector'
  )

  # nCompiler implementation of forward solve with unknown matrix
  nLUsolveMat <- nFunction(
    fun = lusolve,
    argTypes = list(A = 'numericMatrix', b = 'numericMatrix'),
    returnType = 'numericMatrix'
  )

  # nCompiler implementation of forward solve with unknown matrix
  nFsolve <- nFunction(
    fun = fsolve,
    argTypes = list(L = 'numericMatrix', b = 'numericVector'),
    returnType = 'numericVector'
  )

  # nCompiler implementation of forward solve with unknown matrix
  nFsolveMat <- nFunction(
    fun = fsolve,
    argTypes = list(L = 'numericMatrix', b = 'numericMatrix'),
    returnType = 'numericMatrix'
  )

  # nCompiler implementation of backward solve with unknown matrix
  nBsolve <- nFunction(
    fun = bsolve,
    argTypes = list(U = 'numericMatrix', b = 'numericVector'),
    returnType = 'numericVector'
  )

  # nCompiler implementation of backward solve with unknown matrix
  nBsolveMat <- nFunction(
    fun = bsolve,
    argTypes = list(U = 'numericMatrix', b = 'numericMatrix'),
    returnType = 'numericMatrix'
  )

  # compile nFunctions
  cLUsolve <- nCompile(nLUsolve)
  cLUsolveMat <- nCompile(nLUsolveMat)
  cFsolve <- nCompile(nFsolve)
  cFsolveMat <- nCompile(nFsolveMat)
  cBsolve <- nCompile(nBsolve)
  cBsolveMat <- nCompile(nBsolveMat)

  # test data
  L <- structure(c(1, 2, 3, 0, 1, 1, 0, 0, 2), .Dim = c(3L, 3L))
  U <- t(L)
  A <- L %*% t(L)
  x <- c(-1, 3, 1)
  x2 <- cbind(x, x)
  b <- L %*% x
  b2 <- L %*% x2
  bU <- U %*% x
  b2U <- U %*% x2
  bA <- A %*% x
  bA2 <- A %*% x2


  # validate results (lower triangular system) - uncompiled
  expect_equal(as.numeric(nLUsolve(A = A, b = bA)), x)
  expect_equal(nLUsolveMat(A = A, b = bA2), x2)
  expect_equal(as.numeric(nFsolve(L = L, b = b)), x)
  expect_equal(nFsolveMat(L = L, b = b2), unname(x2))
  expect_equal(as.numeric(nBsolve(U = U, b = bU)), x)
  expect_equal(nBsolveMat(U = U, b = b2U), unname(x2))
  expect_equal(nLUsolveMat(A = L, b = b2), x2)

  # validate results (lower triangular system) - compiled
  expect_equal(as.numeric(cLUsolve(A = A, b = bA)), x)
  expect_equal(cLUsolveMat(A = A, b = bA2), unname(x2))
  expect_equal(as.numeric(cFsolve(L = L, b = b)), x)
  expect_equal(cFsolveMat(L = L, b = b2), unname(x2))
  expect_equal(as.numeric(cBsolve(U = U, b = bU)), x)
  expect_equal(cBsolveMat(U = U, b = b2U), unname(x2))
  expect_equal(cLUsolveMat(A = L, b = b2), unname(x2))
})

##
## matrix multiplication
##
test_that("matrix mult works", {
  mmult <- function(x, y) {
    ans <- x %*% y
    return(ans)
  }

  # nCompiler implementation of matrix multiplication with matrix inputs
  nMultMM <- nFunction(
    fun = mmult,
    argTypes = list(x = 'numericMatrix', y = 'numericMatrix'),
    returnType = 'numericMatrix'
  )

  # nCompiler implementation of matrix multiplication with matrix/vector inputs
  nMultMV <- nFunction(
    fun = mmult,
    argTypes = list(x = 'numericMatrix', y = 'numericVector'),
    returnType = 'numericMatrix'
  )

  # nCompiler implementation of matrix multiplication with vector/matrix inputs
  nMultVM <- nFunction(
    fun = mmult,
    argTypes = list(x = 'numericVector', y = 'numericMatrix'),
    returnType = 'numericMatrix'
  )

  # nCompiler implementation of matrix multiplication with vector/vector inputs
  nMultVV <- nFunction(
    fun = mmult,
    argTypes = list(x = 'numericVector', y = 'numericVector'),
    returnType = 'numericMatrix'
  )

  # compiled functions
  cMultMM <- nCompile(nMultMM)
  cMultMV <- nCompile(nMultMV)
  cMultVM <- nCompile(nMultVM)
  cMultVV <- nCompile(nMultVV)

  # vector dimension
  n = 10

  # vector stored as numeric
  v = runif(n = n)

  # explicit row and column vectors
  cv = matrix(data = v, ncol = 1)
  rv = matrix(data = v, nrow = 1)

  # square matrix
  m = matrix(data = runif(n = n^2), nrow = n)

  #
  # matrix multiplication tests
  #
  ## uncompiled
  expect_equal(m %*% m, nMultMM(x = m, y = m))
  expect_identical(cv %*% rv, nMultMM(x = cv, y = rv))

  expect_equal(v %*% v, nMultVV(x = v, y = v))

  expect_equal(v %*% cv, nMultVM(x = v, y = cv))
  expect_equal(v %*% m, nMultVM(x = v, y = m))
  expect_identical(v %*% t(v), nMultVM(x = v, y = t(v)))
  expect_identical(v %*% rv, nMultVM(x = v, y = rv))

  expect_identical(t(v) %*% v, nMultMV(x = t(v), y = v))
  expect_identical(rv %*% v, nMultMV(x = rv, y = v))
  expect_equal(m %*% v, nMultMV(x = m, y = v))
  expect_identical(cv %*% v, nMultMV(x = cv, y = v))

  ## compiled
  expect_equal(m %*% m, cMultMM(x = m, y = m))
  expect_identical(cv %*% rv, cMultMM(x = cv, y = rv))

  expect_equal(v %*% v, cMultVV(x = v, y = v))

  expect_equal(v %*% cv, cMultVM(x = v, y = cv))
  expect_equal(v %*% m, cMultVM(x = v, y = m))
  expect_identical(v %*% t(v), cMultVM(x = v, y = t(v)))
  expect_identical(v %*% rv, cMultVM(x = v, y = rv))

  expect_identical(t(v) %*% v, cMultMV(x = t(v), y = v))
  expect_identical(rv %*% v, cMultMV(x = rv, y = v))
  expect_equal(m %*% v, cMultMV(x = m, y = v))
  expect_identical(cv %*% v, cMultMV(x = cv, y = v))
})

test_that("log determinants work (dense matrices)", {
  
  ldet_direct = nFunction(
    fun = function(x) return(logdet(x)),
    argTypes = list(x = 'numericMatrix()'), 
    returnType = 'numericScalar()'
  )
  
  ldet_tensor_op = nFunction(
    fun = function(x, y) return(logdet(x + y)),
    argTypes = list(x = 'numericMatrix()', y = 'numericMatrix()'), 
    returnType = 'numericScalar()'
  )
  
  ldet_direct_cpp = nCompile(ldet_direct)
  ldet_tensor_op_cpp = nCompile(ldet_tensor_op)
  
  n = 9
  
  set.seed(2025)
  
  x = matrix(rnorm(n = n^2), nrow = n)
  x = t(x) %*% x
  
  y = matrix(rnorm(n = n^2), nrow = n)
  y = t(y) %*% y
  
  #
  # uncompiled execution
  #
  
  # det > 0
  expect_equal(ldet_direct(x = x), log(det(x)))
  # det < 0
  expect_identical(ldet_direct(x = -x), NaN)
  # det = 0
  expect_identical(ldet_direct(x = matrix(c(1,0,0,0), nrow = 2)), -Inf)
  
  #
  # compiled execution
  #
  
  # det > 0
  expect_equal(ldet_direct_cpp(x = x), log(det(x)))
  # det < 0
  expect_identical(ldet_direct_cpp(x = -x), NaN)
  # det = 0
  expect_identical(ldet_direct_cpp(x = matrix(c(1,0,0,0), nrow = 2)), -Inf)
  
  # tensor expressions
  expect_equal(ldet_tensor_op_cpp(x,y), log(det(x+y)))
  
})

test_that("various uses of nEigen", {
    set.seed(1)
    xnsymm <- matrix(c(1.5, .3, .1, 0, .25, .7, 0, 0, -.2), 3)
    xsymm <- xnsymm
    xsymm[upper.tri(xsymm)] <- t(xsymm[lower.tri(xsymm)])

    eig = nFunction(
        fun = function(x = 'numericMatrix') {
            y <- eigen(x)
            return(y$vectors)
        },
        returnType = 'numericMatrix'
    )
    cEig <- nCompile(eig)

    result <- eigen(xnsymm, symmetric = FALSE)$vectors
    vec <- eig(xnsymm)
    cvec <- cEig(xnsymm)
    expect_identical(result, vec)
    ## Equal up to swapping of sign.
    cvec[,1] <- -cvec[,1]
    cvec[,2] <- -cvec[,2]
    expect_equal(result, cvec)

    ## Case with complex-valued result.
    set.seed(1)
    x <- matrix(rnorm(9), 3)
    vec <- eig(x)
    cvec <- cEig(x)
    expect_identical(sum(is.nan(cvec)), 6L)
    expect_equal(Re(vec[,3]), cvec[,1])
    expect_equal(Im(vec[,3]), rep(0,3))

    eigns = nFunction(
        fun = function(x = 'numericMatrix') {
            y <- eigen(x, symmetric = FALSE)
            return(y$vectors)
        },
        returnType = 'numericMatrix'
    )
    cEigns <- nCompile(eigns)

    vec <- eig(xnsymm)
    cvec <- cEig(xnsymm)
    expect_identical(result, vec)
    cvec[,1] <- -cvec[,1]
    cvec[,2] <- -cvec[,2]
    expect_equal(result, cvec)

    eigs = nFunction(
        fun = function(x = 'numericMatrix') {
            y <- eigen(x, symmetric = TRUE)
            return(y$vectors)
        },
        returnType = 'numericMatrix'
    )
    cEigs <- nCompile(eigs)
    
    result <- eigen(xsymm, symmetric = TRUE)$vectors
    vec <- eig(xsymm)
    cvec <- cEig(xsymm)
    expect_identical(result, vec)
    ## Eigenvectors can be in different order.
    expect_equal(result[, order(result[1,])], cvec[, order(cvec[1,])])

    ## If matrix is not actually symmetric, result should be same as symmetric counterpart. 
    result2 <- eigen(xsymm, symmetric = TRUE)$vectors
    vec <- eigs(xnsymm)
    cvec <- cEigs(xnsymm)
    ## Should be the same as decomposition of the symmetric case.
    expect_identical(result2, vec)
    cvec[,2] <- -cvec[,2]
    cvec[,3] <- -cvec[,3]
    expect_equal(result2, cvec)
   

    ## Check case with EigenDecomp not as return type to make sure
    ## predefined code is included with nCompiler_generated code.
    eig = nFunction(
        fun = function(x = 'numericMatrix') {
            y <- eigen(x)$vectors   # No EigenDecomp return type.
            return(y)
        },
        returnType = 'numericMatrix'
    )
    cEig <- nCompile(eig)

    result <- eigen(xnsymm, symmetric = FALSE)$vectors
    vec <- eig(xnsymm)
    cvec <- cEig(xnsymm)
    expect_identical(result, vec)
    cvec[,1] <- -cvec[,1]
    cvec[,2] <- -cvec[,2]
    expect_equal(result, cvec)
    
    eig = nFunction(
        fun = function(x = 'numericMatrix') {
            y <- eigen(x, valuesOnly = TRUE)$values
            return(y)
        },
        returnType = 'numericVector'
    )
    cEig <- nCompile(eig)

    result <- eigen(xnsymm)$values
    vals <- eig(xnsymm)
    cvals <- cEig(xnsymm)
    expect_identical(result, vals)
    expect_equal(result, cvals)

    ## Inline as part of larger calculation.
    fun = nFunction(
        fun = function(x = 'numericMatrix', z = 'numericMatrix') {
            y <- eigen(x)$vectors %*% z
            return(y)
        },
        returnType = 'numericMatrix'
    )
    cfun <- nCompile(fun)

    result <- eigen(xnsymm)$vectors %*% diag(3)
    out <- fun(xnsymm, diag(3))
    cout <- cfun(xnsymm, diag(3))
    cout[,1] <- -cout[,1]
    cout[,2] <- -cout[,2]
    expect_identical(result, out)
    expect_equal(result, cout)

})
