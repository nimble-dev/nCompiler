context("tensorOperations: interoperability with sparse matrix multiplication")

# library(testthat)
# library(nCompiler)
# library(Matrix)

##
## matrix multiplication
##

mmult <- function(x, y) {
  ans <- x %*% y
  return(ans)
}

addmult <- function(x, y, z) {
  ans <- (x + y) %*% z
  return(ans)
}

nAddMultMMS <- nFunction(
  fun = addmult,
  argTypes = list(x = 'numericMatrix', y = 'numericMatrix', z = 'nSparseMatrix'),
  returnType = 'numericMatrix'
)

nMultMS <- nFunction(
  fun = mmult,
  argTypes = list(x = 'numericMatrix', y = 'nSparseMatrix'),
  returnType = 'numericMatrix'
)

nMultSM <- nFunction(
  fun = mmult,
  argTypes = list(x = 'nSparseMatrix', y = 'numericMatrix'),
  returnType = 'numericMatrix'
)

nMultVS <- nFunction(
  fun = mmult,
  argTypes = list(x = 'numericVector', y = 'nSparseMatrix'),
  returnType = 'numericMatrix'
)

nMultSV <- nFunction(
  fun = mmult,
  argTypes = list(x = 'nSparseMatrix', y = 'numericVector'),
  returnType = 'numericMatrix'
)

# compiled functions
cMultMS <- nCompile(nMultMS)
cMultSM <- nCompile(nMultSM)
cMultVS <- nCompile(nMultVS)
cMultSV <- nCompile(nMultSV)
cAddMultMMS <- nCompile(nAddMultMMS)

# vector dimensions
m = 3
n = 2
p = 6

set.seed(2022)

# dense matrices
Xdense = matrix(runif(m * n), nrow = m)
Ydense = matrix(runif(n * p), nrow = n)

# sparse matrices
Xsparse = Xdense
Xsparse[sample(1:length(Xsparse), size = .6 * length(Xsparse))] = 0
Xsparse = Matrix::Matrix(Xsparse, sparse = TRUE)
Ysparse = Ydense
Ysparse[sample(1:length(Ysparse), size = .6 * length(Ysparse))] = 0
Ysparse = Matrix::Matrix(Ysparse, sparse = TRUE)

# sparse vectors
XsparseV = as.numeric(Xsparse[1,])
XsparseV = Matrix::sparseVector(
  x = XsparseV[which(XsparseV != 0)], 
  i = which(XsparseV != 0), 
  length = length(XsparseV)
)
YsparseV = as.numeric(Ysparse[,1])
YsparseV = Matrix::sparseVector(
  x = YsparseV[which(YsparseV != 0)], 
  i = which(YsparseV != 0), 
  length = length(YsparseV)
)

#
# matrix multiplication tests
#

expect_identical(
  unname(as.matrix((Xdense + 2 * Xdense) %*% Ysparse)), 
  cAddMultMMS(x = Xdense, y = Xdense * 2, z = Ysparse)
)
expect_identical(
  unname(as.matrix(Xdense %*% Ysparse)), 
  cMultMS(x = Xdense, y = Ysparse)
)
expect_identical(
  unname(as.matrix(Xsparse %*% Ydense)), 
  cMultSM(x = Xsparse, y = Ydense)
)
expect_identical(
  unname(as.matrix(Xdense[1:n] %*% Ysparse)), 
  cMultVS(x = Xdense[1:n], y = Ysparse)
)
expect_identical(
  unname(as.matrix(Xsparse %*% Ydense[1:n])), 
  cMultSV(x = Xsparse, y = Ydense[1:n])
)

