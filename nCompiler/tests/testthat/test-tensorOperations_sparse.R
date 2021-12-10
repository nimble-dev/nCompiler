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


#
# basic R functions
#

add2_force_sparse <- function(x, y) { 
  ans <- asSparse(x + y)
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

# demonstrate how returnType must match the return type from argument fun
nAdd2_force_bad_return <- nFunction(
  fun = add2_force_sparse, 
  argTypes = list(
    x = 'nMatrix', y = 'nMatrix'
  ), 
  returnType = 'nMatrix'
)

# demonstrate support for mixed inputs
nAdd2_force_mixed <- nFunction(
  fun = add2_force_sparse,
  argTypes = list(
    x = 'nSparseMatrix', y = 'nMatrix'
  ), 
  returnType = 'nSparseMatrix'
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
  
# verify asSparse works from R
expect_equal(nAdd2_force(x = M, y = M2), M_sparse + M2_sparse)


#
# compile functions, testing compilation and error trapping of argTypes
#

cAdd2_force <- nCompile(nAdd2_force)
cAdd2_force_mixed <- nCompile(nAdd2_force_mixed)
cAdd2_promote <- nCompile(nAdd2_promote)
cAdd3 <- nCompile(nAdd3)
cAdd2_sparse <- nCompile(nAdd2_sparse)

# nFunction will not compile if return statement cannot be converted to an 
# object of class returnType in C++; we should also get a type warning that 
# helps point us to the cause of the error
expect_warning(
  expect_error(
    cAdd2_force_bad_return <- nCompile(nAdd2_force_bad_return)
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
