library(nCompiler)
library(Matrix)
library(spam)
library(SparseM)
library(testthat)

add <- function(x, y) { 
  ans <- x + y
  return(ans)
}

add3 <- function(x, y, z) { 
  ans <- x + y + z
  return(ans)
}

nAdd <- nFunction(
  fun = add,
  argTypes = list(x = 'nSparseMatrix', 
                  y = 'nSparseMatrix'),
  returnType = 'nSparseMatrix'
)

nAdd2 <- nFunction(
  fun = add,
  argTypes = list(x = 'nSparseMatrix', 
                  y = 'nMatrix'),
  returnType = 'nMatrix'
)

nAdd3 <- nFunction(
  fun = add3,
  argTypes = list(x = 'nSparseMatrix', 
                  y = 'nMatrix',
                  z = 'nMatrix'),
  returnType = 'nMatrix'
)

nCAdd <- nCompile(nAdd)
nCAdd2 <- nCompile(nAdd2)
nCAdd3 <- nCompile(nAdd3)

#
# generate demo objects
#

# size of matrices
m = 7
n = 10
# proportion of non-zero entries
p = .1

# set up a matrix with only a few random non-zero entries
M = matrix(data = 0, nrow = m, ncol = n)
M[sample(x = length(M), size = p * length(M))] = runif(n = p * length(M))

# convert matrix to sparse format, using spam and Matrix classes
M_sparse = Matrix::Matrix(M, sparse = TRUE)
M_sparse_spam = as.spam(M)

# set up a second matrix with only a few random non-zero entries
M2 = matrix(data = 0, nrow = m, ncol = n)
M2[sample(x = length(M2), size = p * length(M2))] = runif(n = p * length(M2))

# convert matrix to sparse format, using spam and Matrix classes
M2_sparse = Matrix::Matrix(M2, sparse = TRUE)
M2_sparse_spam = as.spam(M2)

# build a symmetric matrix, which can be decomposed
Msym = t(M_sparse) %*% M_sparse + diag(ncol(M_sparse))

expect_identical(
  nCAdd(x = M_sparse, y = M2_sparse),
  M_sparse + M2_sparse
)

expect_equal(
  nCAdd2(x = M_sparse, y = M2),
  matrix(M_sparse + M2, nrow = nrow(M_sparse))
)

expect_equal(
  nCAdd3(x = M_sparse, y = M2, z = M),
  matrix(M_sparse + M2 + M, nrow = nrow(M_sparse))
)

