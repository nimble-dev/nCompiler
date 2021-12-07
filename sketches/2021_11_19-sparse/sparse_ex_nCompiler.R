library(nCompiler)
library(Matrix)
library(spam)
library(SparseM)
library(testthat)

add <- function(x, y) { 
  ans <- x + y
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

nC <- nCompile(nAdd, nAdd2)

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
  nC$nFun_1_NFID_1(x = M_sparse, y = M2_sparse),
  M_sparse + M2_sparse
)

expect_equal(
  max(abs(nC$nFun_2_NFID_2(x = M_sparse, y = M2) - as.matrix(M_sparse + M2))),
  0
)
