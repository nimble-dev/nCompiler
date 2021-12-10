context("tensorOperations: interoperability with sparse matrices/vectors")

#
# test error trapping and generated C++ code that implements nCompiler's support 
# for evaluating binary operations and conversion between between sparse and 
# dense matrices and vectors.
#

library(Matrix)
library(testthat)

#
# generate demo objects
#

# size of matrices
m <- 7
n <- 10
# proportion of non-zero entries
p <- .1

# set up a matrix with only a few random non-zero entries
M <- matrix(data = 0, nrow = m, ncol = n)
M[sample(x = length(M), size = p * length(M))] <- runif(n = p * length(M))
M_sparse <- Matrix::Matrix(M, sparse = TRUE)

# set up a second matrix with only a few random non-zero entries
M2 <- matrix(data = 0, nrow = m, ncol = n)
M2[sample(x = length(M2), size = p * length(M2))] <- runif(n = p * length(M2))
M2_sparse <- Matrix::Matrix(M2, sparse = TRUE)

# set up a third matrix with only a few random non-zero entries
M3 <- matrix(data = 0, nrow = m, ncol = n)
M3[sample(x = length(M3), size = p * length(M3))] <- runif(n = p * length(M3))
M3_sparse <- Matrix::Matrix(M3, sparse = TRUE)

# sparse matrix with additional 0's stored explicitly
Munpruned <- sparseMatrix(
  i = sample(x = 1:m, size = floor(m*n*p) + 2, replace = TRUE),
  j = sample(x = 1:m, size = floor(m*n*p) + 2, replace = TRUE),
  x = c(runif(n = floor(m*n*p)), 0, 0)
)

#
# basic R functions
#

add2_force_sparse <- function(x, y) { 
  ans <- asSparse(x + y)
  return(ans)
}

add2_force_dense <- function(x, y) { 
  ans <- asDense(x + y)
  return(ans)
}

add2 <- function(x, y) { 
  ans <- x + y
  return(ans)
}

add3 <- function(x, y, z) { 
  ans <- x + y + z
  return(ans)
}

prune <- function(x, prune) { 
  ans <- asSparse(x, prune = prune)
  return(ans)
}

asSparse1arg <- function(x) {
  return(asSparse(x))
}

asDense1arg <- function(x) {
  return(asDense(x))
}

# 
# supporting nFunctions.  different combinations of argTypes are used for each 
# basic function so we can test automatic conversion and error trapping between
# sparse and dense representations
#

# demonstrate how automatic type promotion works
nAdd2_promote <- nFunction(
  fun = add2,
  argTypes = list(
    x = 'nMatrix', y = 'nSparseMatrix'
  ),
  returnType = 'nMatrix'
)

# demonstrate how asSparse conversion works
nAdd2_force <- nFunction(
  fun = add2_force_sparse, 
  argTypes = list(
    x = 'nMatrix', y = 'nMatrix'
  ), 
  returnType = 'nSparseMatrix'
)

# demonstrate how asDense conversion works
nAdd2_force_dense <- nFunction(
  fun = add2_force_dense, 
  argTypes = list(
    x = 'nSparseMatrix', y = 'nSparseMatrix'
  ), 
  returnType = 'nMatrix'
)

# demonstrate how asDense conversion works
# (asDense will be removed from AST during compilation)
nAdd2_force_dense_unnecessary <- nFunction(
  fun = add2_force_dense, 
  argTypes = list(
    x = 'nMatrix', y = 'nMatrix'
  ), 
  returnType = 'nMatrix'
)

# demonstrate how returnType must match the return type from argument fun
nAdd2_force_bad_return <- nFunction(
  fun = add2_force_sparse, 
  argTypes = list(
    x = 'nMatrix', y = 'nMatrix'
  ), 
  returnType = 'nMatrix'
)

# demonstrate how returnType must match the return type from argument fun
nAdd2_force_dense_bad_return <- nFunction(
  fun = add2_force_dense, 
  argTypes = list(
    x = 'nSparseMatrix', y = 'nSparseMatrix'
  ), 
  returnType = 'nSparseMatrix'
)

# demonstrate support for mixed inputs
nAdd2_force_mixed <- nFunction(
  fun = add2_force_sparse,
  argTypes = list(
    x = 'nSparseMatrix', y = 'nMatrix'
  ), 
  returnType = 'nSparseMatrix'
)

# demonstrate support for mixed inputs
nAdd2_force_dense_mixed <- nFunction(
  fun = add2_force_dense,
  argTypes = list(
    x = 'nSparseMatrix', y = 'nMatrix'
  ), 
  returnType = 'nMatrix'
)

# demonstrate full use of sparse matrices
nAdd2_sparse <- nFunction(
  fun = add2,
  argTypes = list(
    x = 'nSparseMatrix', y = 'nSparseMatrix'
  ), 
  returnType = 'nSparseMatrix'
)

# demonstrate C++ works for nested statements (i.e., SparseMatrix + TensorExpr)
nAdd3 <- nFunction(
  fun = add3, 
  argTypes = list(
    x = 'nSparseMatrix', y = 'nMatrix', z = 'nMatrix'
  ),
  returnType = 'nMatrix'
)

# demonstrate pruning extra 0's from sparse matrix objects
nPrune <- nFunction(
  fun = prune, 
  argTypes = list(x = 'nSparseMatrix', prune = 'logical'),
  returnType = 'nSparseMatrix'
)

# demonstrate C++ code can work with Eigen objects, not just Eigen expressions
nAsSparse <- nFunction(
  fun = asSparse1arg,
  argTypes = list(x = 'nMatrix'),
  returnType = 'nSparseMatrix'
)

# demonstrate C++ code can work with Eigen objects, not just Eigen expressions
nAsDense <- nFunction(
  fun = asDense1arg,
  argTypes = list(x = 'nSparseMatrix'),
  returnType = 'nMatrix'
)
  
# verify asSparse and asDense work from R
expect_equal(nAdd2_force(x = M, y = M2), M_sparse + M2_sparse)
expect_equal(
  { z = nAdd2_force_dense(x = M_sparse, y = M2_sparse); 
    attr(z, 'dimnames') = NULL; 
    z }, 
  M + M2
)

# verify pruning works from R
expect_equal(nPrune(x = Munpruned, prune = TRUE), Matrix::drop0(Munpruned))
expect_equal(nPrune(x = Munpruned, prune = FALSE), Munpruned)


#
# compile functions, testing compilation and error trapping of argTypes
#

cAdd2_force <- nCompile(nAdd2_force)
cAdd2_force_mixed <- nCompile(nAdd2_force_mixed)
cAdd2_promote <- nCompile(nAdd2_promote)
cAdd3 <- nCompile(nAdd3)
cAdd2_sparse <- nCompile(nAdd2_sparse)
cPrune <- nCompile(nPrune)
cAdd2_force_dense <- nCompile(nAdd2_force_dense)
cAdd2_force_dense_mixed <- nCompile(nAdd2_force_dense_mixed)
cAdd2_force_dense_unnecessary <- nCompile(nAdd2_force_dense_unnecessary)
cAsSparse <- nCompile(nAsSparse)
cAsDense <- nCompile(nAsDense)


# nFunction will not compile if return statement cannot be converted to an 
# object of class returnType in C++; we should also get a type warning that 
# helps point us to the cause of the error
expect_warning(
  expect_error(
    cAdd2_force_bad_return <- nCompile(nAdd2_force_bad_return)
  )
)
expect_warning(
  expect_error(
    cAdd2_force_dense_bad_return <- nCompile(nAdd2_force_dense_bad_return)
  )
)


#
# test generated C++ code
#

expect_equal(cAdd2_force(x = M, y = M2), M_sparse + M2_sparse)
expect_equal(cAdd2_force_mixed(x = M_sparse, y = M2), M_sparse + M2_sparse)
expect_equal(cAdd2_promote(x = M, y = M2_sparse), M + M2)
expect_equal(cAdd3(x = M_sparse, y = M2, z = M3), M + M2 + M3)
expect_equal(cAdd2_sparse(x = M_sparse, y = M2_sparse), M_sparse + M2_sparse)
expect_equal(cPrune(x = Munpruned, prune = TRUE), Matrix::drop0(Munpruned))
expect_equal(cPrune(x = Munpruned, prune = FALSE), Munpruned)
expect_equal(cAdd2_force_dense(x = M_sparse, y = M2_sparse), M + M2)
expect_equal(cAdd2_force_dense_mixed(x = M_sparse, y = M2), M + M2)
expect_equal(cAsSparse(x = M), M_sparse)
expect_equal(cAsDense(x = M_sparse), M)
expect_equal(cAdd2_force_dense_unnecessary(x = M, y = M2), M + M2)
