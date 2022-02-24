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



# TODO: sparse creation operator

# sparse diagonal matrices are specified via Matrix::Diagonal, as a very general
# rule.

# sparse identity matrix
Matrix::Diagonal(n = 5)

# sparse, square diagonal matrix
Matrix::Diagonal(x = 1:10)


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

# dense assignment
diag(X) = 1:nrow(X)

# sparse assignment
diag(Xsp) = 1:nrow(Xsp)
