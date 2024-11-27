context("tensorOperations: special read/write operators")

library(nCompiler)
library(testthat)
library(Matrix)

#
# test the ability to read/write using nCompiler C++ implementations of diag(), 
# and related accessor functions
#

#
# generate test data
#

set.seed(2022)

# diagonal matrix creation data
nr <- 10
nc <- 3
xv <- runif(n = min(nr, nc))
xv_nr <- runif(n = nr)

# random dense matrices
X <- matrix(runif(n = 100), nrow = nr)
Y <- matrix(runif(n = 100), nrow = nr)

# random sparse matrix
Xsp <- X
Xsp[sample(x = length(Xsp), size = .9 * length(Xsp))] <- 0
Xsp <- Matrix(Xsp, sparse = TRUE)
diag(Xsp)[sample(x = nrow(Xsp), size = .1 * nrow(Xsp))] <- 3

#
# diag as a creation operator
#

# Documenting many of R's behaviors for diag().  There are a few other cases 
# where x may be either a scalar or a vector, expanding upon the ideas 
# documented here.
# 
# FN. ARGS.         NROW      NCOL        DIAG  EIGENIZED
#
# (x, nrow, ncol)   nrow      ncol        x     (x, nrow, ncol)
# (nrow, ncol)      nrow      ncol        1     (1, nrow, ncol)
# (x)               x         x           1     (1, x, x)
# (x)               length(x) length(x)   x     (x, length(x), length(x))
# (x, nrow)         nrow      nrow        x     (x, nrow, nrow)
# (x, ncol)         1         ncol        x     (x, 1, ncol)
# (nrow)            nrow      nrow        1     (1, nrow, nrow)
# (ncol)            ---     ---           -     Error, stop processing!

diagXRC <- function(x, nrow, ncol) {
  ans <- diag(x = x, nrow = nrow, ncol = ncol)
  return(ans)
}

diagXR <- function(x, nrow) {
  ans <- diag(x = x, nrow = nrow)
  return(ans)
}

diagXC <- function(x, ncol) {
  ans <- diag(x = x, ncol = ncol)
  return(ans)
}

diagRC <- function(nrow, ncol) {
  ans <- diag(nrow = nrow, ncol = ncol)
  return(ans)
}

diagX <- function(x) {
  ans <- diag(x = x)
  return(ans)
}

diagR <- function(nrow) {
  ans <- diag(nrow = nrow)
  return(ans)
}

diagC <- function(ncol) {
  ans <- diag(ncol = ncol)
  return(ans)
}

nDiagXRCv <- nFunction(
  fun = diagXRC,
  argTypes = list(x = 'numericVector', nrow = 'integer', ncol = 'integer'),
  returnType = 'nMatrix'
)

nDiagXRC <- nFunction(
  fun = diagXRC,
  argTypes = list(x = 'double', nrow = 'integer', ncol = 'integer'),
  returnType = 'nMatrix'
)

nDiagXRv <- nFunction(
  fun = diagXR,
  argTypes = list(x = 'numericVector', nrow = 'integer'),
  returnType = 'nMatrix'
)

nDiagXR <- nFunction(
  fun = diagXR,
  argTypes = list(x = 'double', nrow = 'integer'),
  returnType = 'nMatrix'
)

nDiagXCv <- nFunction(
  fun = diagXC,
  argTypes = list(x = 'numericVector', ncol = 'integer'),
  returnType = 'nMatrix'
)

nDiagXC <- nFunction(
  fun = diagXC,
  argTypes = list(x = 'double', ncol = 'integer'),
  returnType = 'nMatrix'
)

nDiagRC <- nFunction(
  fun = diagRC,
  argTypes = list(nrow = 'integer', ncol = 'integer'),
  returnType = 'nMatrix'
)

nDiagXv <- nFunction(
  fun = diagX,
  argTypes = list(x = 'numericVector'),
  returnType = 'nMatrix'
)

nDiagX <- nFunction(
  fun = diagX,
  argTypes = list(x = 'integer'),
  returnType = 'nMatrix'
)

nDiagR <- nFunction(
  fun = diagR,
  argTypes = list(nrow = 'integer'),
  returnType = 'nMatrix'
)

nDiagC <- nFunction(
  fun = diagC,
  argTypes = list(ncol = 'integer'),
  returnType = 'nMatrix'
)

cDiagXRCv <- nCompile(nDiagXRCv)
cDiagXRC <- nCompile(nDiagXRC)
cDiagXRv <- nCompile(nDiagXRv)
cDiagXR <- nCompile(nDiagXR)
cDiagXCv <- nCompile(nDiagXCv)
cDiagXC <- nCompile(nDiagXC)
cDiagRC <- nCompile(nDiagRC)
cDiagXv <- nCompile(nDiagXv)
cDiagX <- nCompile(nDiagX)
cDiagR <- nCompile(nDiagR)

#
# creation usage tests
#

expect_error(nCompile(nDiagC)) # don't support behavior R doesn't support
expect_identical(diag(x = xv, nrow = nr, ncol = nc),
                 cDiagXRCv(x = xv, nrow = nr, ncol = nc))
expect_error(cDiagXRCv(x = 4, nrow = nr, ncol = nc))
expect_error(cDiagXRCv(x = runif(nr), nrow = nr, ncol = nc))
expect_identical(cDiagXRC(x = 3, nrow = nr, ncol = nc),
                 diag(x = 3, nrow = nr, ncol = nc))
expect_identical(cDiagXRv(x = xv_nr, nrow = nr), diag(x = xv_nr, nrow = nr))
expect_error(cDiagXRv(x = xv, nrow = nr))
expect_identical(cDiagXR(x = 3, nrow = nr), diag(x = 3, nrow = nr))
expect_identical(cDiagXCv(x = 3, ncol = nc), diag(x = 3, ncol = nc))
expect_identical(cDiagXC(x = 3, ncol = nc), diag(x = 3, ncol = nc))
expect_identical(cDiagRC(nrow = nr, ncol = nc), diag(nrow = nr, ncol = nc))
expect_identical(cDiagXv(x = xv), diag(x = xv))
expect_identical(cDiagX(x = 3), diag(x = 3))
expect_identical(cDiagR(nrow = nr), diag(nrow = nr))

#
# sparse creation tests
#

diagXRC <- function(x, nrow, ncol) {
  ans <- Diagonal(x = x, nrow = nrow, ncol = ncol)
  return(ans)
}

diagXR <- function(x, nrow) {
  ans <- Diagonal(x = x, nrow = nrow)
  return(ans)
}

diagXC <- function(x, ncol) {
  ans <- Diagonal(x = x, ncol = ncol)
  return(ans)
}

diagRC <- function(nrow, ncol) {
  ans <- Diagonal(nrow = nrow, ncol = ncol)
  return(ans)
}

diagX <- function(x) {
  ans <- Diagonal(x = x)
  return(ans)
}

diagR <- function(nrow) {
  ans <- Diagonal(nrow = nrow)
  return(ans)
}

diagC <- function(ncol) {
  ans <- Diagonal(ncol = ncol)
  return(ans)
}

nSpDiagXRCv <- nFunction(
  fun = diagXRC,
  argTypes = list(x = 'numericVector', nrow = 'integer', ncol = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagXRC <- nFunction(
  fun = diagXRC,
  argTypes = list(x = 'double', nrow = 'integer', ncol = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagXRv <- nFunction(
  fun = diagXR,
  argTypes = list(x = 'numericVector', nrow = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagXR <- nFunction(
  fun = diagXR,
  argTypes = list(x = 'double', nrow = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagXCv <- nFunction(
  fun = diagXC,
  argTypes = list(x = 'numericVector', ncol = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagXC <- nFunction(
  fun = diagXC,
  argTypes = list(x = 'double', ncol = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagRC <- nFunction(
  fun = diagRC,
  argTypes = list(nrow = 'integer', ncol = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagXv <- nFunction(
  fun = diagX,
  argTypes = list(x = 'numericVector'),
  returnType = 'nSparseMatrix'
)

nSpDiagX <- nFunction(
  fun = diagX,
  argTypes = list(x = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagR <- nFunction(
  fun = diagR,
  argTypes = list(nrow = 'integer'),
  returnType = 'nSparseMatrix'
)

nSpDiagC <- nFunction(
  fun = diagC,
  argTypes = list(ncol = 'integer'),
  returnType = 'nSparseMatrix'
)

cSpDiagXRCv <- nCompile(nSpDiagXRCv)
cSpDiagXRC <- nCompile(nSpDiagXRC)
cSpDiagXRv <- nCompile(nSpDiagXRv)
cSpDiagXR <- nCompile(nSpDiagXR)
cSpDiagXCv <- nCompile(nSpDiagXCv)
cSpDiagXC <- nCompile(nSpDiagXC)
cSpDiagRC <- nCompile(nSpDiagRC)
cSpDiagXv <- nCompile(nSpDiagXv)
cSpDiagX <- nCompile(nSpDiagX)
cSpDiagR <- nCompile(nSpDiagR)

expect_error(nCompile(nSpDiagC)) # don't support behavior R doesn't support
expect_identical(as(diag(x = xv, nrow = nr, ncol = nc), 'dgCMatrix'),
                 cSpDiagXRCv(x = xv, nrow = nr, ncol = nc))
expect_error(cSpDiagXRCv(x = 4, nrow = nr, ncol = nc))
expect_error(cSpDiagXRCv(x = runif(nr), nrow = nr, ncol = nc))
expect_identical(cSpDiagXRC(x = 3, nrow = nr, ncol = nc),
                 as(diag(x = 3, nrow = nr, ncol = nc), 'dgCMatrix'))
expect_identical(cSpDiagXRv(x = xv_nr, nrow = nr), 
                 as(diag(x = xv_nr, nrow = nr), 'dgCMatrix'))
expect_error(cSpDiagXRv(x = xv, nrow = nr))
expect_identical(cSpDiagXR(x = 3, nrow = nr), 
                 as(diag(x = 3, nrow = nr), 'dgCMatrix'))
expect_identical(cSpDiagXCv(x = 3, ncol = nc), 
                 as(diag(x = 3, ncol = nc), 'dgCMatrix'))
expect_identical(cSpDiagXC(x = 3, ncol = nc), 
                 as(diag(x = 3, ncol = nc), 'dgCMatrix'))
expect_identical(cSpDiagRC(nrow = nr, ncol = nc), 
                 as(diag(nrow = nr, ncol = nc), 'dgCMatrix'))
expect_identical(cSpDiagXv(x = xv), 
                 as(diag(x = xv), 'dgCMatrix'))
expect_identical(cSpDiagX(x = 3), 
                 as(diag(x = 3), 'dgCMatrix'))
expect_identical(cSpDiagR(nrow = nr), 
                 as(diag(nrow = nr), 'dgCMatrix'))


#
# diag as accessor operator
#

diagExprAccessor <- function(x, y) {
  ans <- diag(x + y)
  return(ans)
}

diagAccessor <- function(x) {
  ans <- diag(x)
  return(ans)
}

nDiagAccessor <- nFunction(
  fun = diagAccessor, 
  argTypes = list(x = 'nMatrix'), 
  returnType = 'numericVector'
)  

nDiagExprAccessor <- nFunction(
  fun = diagExprAccessor, 
  argTypes = list(x = 'nMatrix', y = 'nMatrix'), 
  returnType = 'numericVector'
)  

nDiagAccessorSp <- nFunction(
  fun = diagAccessor, 
  argTypes = list(x = 'nSparseMatrix'), 
  returnType = 'numericVector'
)  

cDiagAccessor <- nCompile(nDiagAccessor)
cDiagAccessorSp <- nCompile(nDiagAccessorSp)
cDiagExprAccessor <- nCompile(nDiagExprAccessor)

# dense accessor
expect_identical(as.numeric(cDiagAccessor(X)), diag(X))

# dense accessor of tensor expression objects
expect_identical(as.numeric(cDiagExprAccessor(X, Y)), diag(X + Y))

# sparse accessor
expect_identical(as.numeric(cDiagAccessorSp(Xsp)), diag(Xsp))


#
# diag as assignment operator
#

diagExprAssignment <- function(x, y, z) {
  diag(x) <- y + z
  return(x)
}

diagAssignment <- function(x, y) {
  diag(x) <- y
  return(x)
}

nDiagExprAssignment <- nFunction(
  fun = diagExprAssignment, 
  argTypes = list(x = 'nMatrix', y = 'numericVector', z = 'numericVector'),
  returnType = 'nMatrix'
)

nSpDiagExprAssignment <- nFunction(
  fun = diagExprAssignment, 
  argTypes = list(x = 'nSparseMatrix', y = 'numericVector', z = 'numericVector'),
  returnType = 'nSparseMatrix'
)

nDiagAssignmentv <- nFunction(
  fun = diagAssignment,
  argTypes = list(x = 'nMatrix', y = 'numericVector'),
  returnType = 'nMatrix'
)

nDiagAssignment <- nFunction(
  fun = diagAssignment,
  argTypes = list(x = 'nMatrix', y = 'double'),
  returnType = 'nMatrix'
)

nSpDiagAssignmentv <- nFunction(
  fun = diagAssignment,
  argTypes = list(x = 'nSparseMatrix', y = 'numericVector'),
  returnType = 'nSparseMatrix'
)

nSpDiagAssignment <- nFunction(
  fun = diagAssignment,
  argTypes = list(x = 'nSparseMatrix', y = 'double'),
  returnType = 'nSparseMatrix'
)

cDiagExprAssignment <- nCompile(nDiagExprAssignment)
cDiagAssignmentv <- nCompile(nDiagAssignmentv)
cDiagAssignment <- nCompile(nDiagAssignment)
cSpDiagAssignmentv <- nCompile(nSpDiagAssignmentv)
cSpDiagAssignment <- nCompile(nSpDiagAssignment)
cSpDiagExprAssignment <- nCompile(nSpDiagExprAssignment)

# dense assignment via an expression
X1 <- X
X2 <- X
diag(X1) <- diag(Y) + diag(X)
expect_identical(X1, cDiagExprAssignment(x = X2, y = diag(Y), z = diag(X)))

# dense assignment to vector
X1 <- X
X2 <- X
diag(X1) <- 1:nrow(X)
expect_identical(X1, cDiagAssignmentv(x = X2, y = 1:nrow(X)))

# dense assignment to constant
X1 <- X
X2 <- X
diag(X1) <- pi
expect_identical(X1, cDiagAssignment(x = X2, y = pi))

# sparse assignment to vector
X1 <- Xsp
X2 <- Xsp
diag(X1) = 1:nrow(X1)
expect_identical(X1, cSpDiagAssignmentv(x = X2, y = 1:nrow(X1)))

# sparse assignment to constant
X1 <- Xsp
X2 <- Xsp
diag(X1) = pi
expect_identical(X1, cSpDiagAssignment(x = X2, y = pi))

# sparse assignment via an expression
X1 <- Xsp
X2 <- Xsp
diag(X1) <- diag(Y) + diag(X)
expect_identical(X1, cSpDiagExprAssignment(x = X2, y = diag(Y), z = diag(X)))


#
# usage in composition
#

d2 <- function(x) {
  return(diag(diag(x = x)))
}

d2Sp <- function(x) {
  return(diag(Diagonal(x = x)))
}

nD2 <- nFunction(
  fun = d2, 
  argTypes = list(x = 'integer'), 
  returnType = 'numericVector'
)

nD2Sp <- nFunction(
  fun = d2Sp, 
  argTypes = list(x = 'integer'), 
  returnType = 'numericVector'
)

cD2 <- nCompile(nD2)
cD2Sp <- nCompile(nD2Sp)

expect_equivalent(cD2(x = 5), rep(1,5))
expect_equivalent(cD2Sp(x = 5), rep(1,5))

