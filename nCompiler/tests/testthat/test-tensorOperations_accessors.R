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

# random dense matrices
X <- matrix(runif(n = 100), nrow = 10)
Y <- matrix(runif(n = 100), nrow = 10)

# random sparse matrix
Xsp <- X
Xsp[sample(x = length(Xsp), size = .9 * length(Xsp))] <- 0
Xsp <- Matrix(Xsp, sparse = TRUE)
diag(Xsp)[sample(x = nrow(Xsp), size = .1 * nrow(Xsp))] <- 3

#
# diag as a creation operator
#

# TODO: use nCompiler to fill in all of the arguments required for creation at 
# the C++ level... diagonal entries, nrow, ncol, sparse/dense.  will also need 
# some reasoning to decide how to interpret the first argument, "x".
# 
# as an additional design question, do these reasoning steps need to be done 
# at compile-time or at run time?  my hunch, for nCompiler, is that compile-time
# is appropriate since the goal is to generate c++ code for specific use cases,
# rather than for completely arbitrary user inputs... the idea is that users 
# are writing nCompiler to generate and compile c++ code for well defined, 
# specific scenarios

# non-square diagonal matrix, all arguments
diag(x = 1:10, nrow = 10, ncol = 20)

# non-square diagonal matrix, with recycling rule for main diagonal creation
diag(x = 5, nrow = 10, ncol = 20)

# non-square diagonal matrix
diag(nrow = 10, ncol = 5)

# square diagonal matrix
diag(x = 1:10)

# identity matrix
diag(x = 10)

# TODO: yes, in the below we do want to write a custom c++ function vs. writing 
#  nCompiler code to implement via asSparse(diag(...)) b/c creating a dense 
#  diagonal matrix will cause lots of wasted space.

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