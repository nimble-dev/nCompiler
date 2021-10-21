# demonstrate templated binary operations between matrix and vector objects

library(Rcpp)
library(testthat)

# source code
base_dir = file.path('sketches', '2021_10_21-eigen_exceptions')
filename <- "eigen_exceptions.cpp"

test_suite = function(enable_errors) {
  # Test Rcpp/Eigen error handling
  #
  # Each test will ask Eigen to perform an operation on non-conformable inputs.
  #
  # Parameters:
  #  enable_errors - TRUE to enable error handling during compilation, FALSE
  #   to disable error handling (i.e., Rcpp's default behavior)

  # enable errors with a compiler flag
  Sys.setenv(
    PKG_CXXFLAGS = ifelse(enable_errors, '-DNCOMPILER_ENABLE_EIGEN_ERRORS', '')
  )
  
  # force (re)compilation of test functions
  sourceCpp(file.path(base_dir, filename), rebuild = TRUE)
  
  # dummy data
  x = 1:10
  y = 1:100
  
  xmat = matrix(x, nrow = 1)
  ymat = matrix(y, nrow = 1)
  ymat2 = matrix(y, nrow = 2)
  
  # componentwise addition of tensors with different numbers of elements
  context = 'Componentwise addition of tensors with different num. elements'
  message(paste('Testing', context))
  if(enable_errors) {
    test_that(context, expect_error(vec_plus(x, y)))
    test_that(context, expect_error(vec_plus(y, x)))
  } else {
    # Without errors, Eigen will add non-conformable tensors; return will be 
    # tensor whose length matches the first argument
    z = vec_plus(x, y)
    test_that(context, expect_equal(length(z), length(x)))
    # when first tensor is longer than the second, Eigen will read from 
    # uninitialized memory, so the second half of the results will behave 
    # inconsistently
    z = vec_plus(y, x)
    test_that(context, expect_equal(length(z), length(y)))
  }
  
  # componentwise addition of matrices with different numbers of elements
  context = 'Componentwise addition of row-matrices with different lengths'
  message(paste('Testing', context))
  if(enable_errors) {
    test_that(context, expect_error(mat_plus(xmat, ymat)))
    test_that(context, expect_error(mat_plus(ymat, xmat)))
  } else {
    # Without errors, Eigen will add non-conformable matrices; return will be 
    # matrix whose dimensions matches the second argument
    zmat = mat_plus(xmat, ymat)
    test_that(context, expect_equal(dim(zmat), dim(ymat)))
    # when first matrix is longer than the second, Eigen will read from 
    # uninitialized memory, so the second half of the results will behave 
    # inconsistently
    zmat = mat_plus(ymat, xmat)
    test_that(context, expect_equal(dim(zmat), dim(xmat)))
  }
  
  # componentwise addition of matrices with different numbers of dimensions
  context = 'Componentwise addition of matrices with different dimensions'
  message(paste('Testing', context))
  if(enable_errors) {
    test_that(context, expect_error(mat_plus(xmat, ymat2)))
    test_that(context, expect_error(mat_plus(ymat2, xmat)))
  } else {
    # Without errors, Eigen will add non-conformable matrices; return will be 
    # matrix whose dimensions matches the second argument
    zmat = mat_plus(xmat, ymat2)
    test_that(context, expect_equal(dim(zmat), dim(ymat2)))
    # when first matrix is longer than the second, Eigen will read from 
    # uninitialized memory, so the second half of the results will behave 
    # inconsistently
    zmat = mat_plus(ymat2, xmat)
    test_that(context, expect_equal(dim(zmat), dim(xmat)))
  }
  
  # multiplication of non-conformable matrices
  context = 'Non-conformable matrix multiplication'
  message(paste('Testing', context))
  if(enable_errors) {
    test_that(context, expect_error(mat_mult(xmat, ymat2)))
    test_that(context, expect_error(mat_mult(ymat2, xmat)))
  } else {
    # Without errors, Eigen will multiply non-conformable matrices; return will 
    # have a matrix with "textbook" dimensions, and pull from unallocated memory
    # as needed to evaluate the required inner products.
    zmat = mat_mult(xmat, ymat2)
    expected_dim = c(nrow(xmat), ncol(ymat2))
    test_that(context, expect_equal(dim(zmat), expected_dim))
    # 
    zmat = mat_mult(ymat2, xmat)
    expected_dim = c(nrow(ymat2), ncol(xmat))
    test_that(context, expect_equal(dim(zmat), expected_dim))
  }
  
}

# run test suites
test_suite(enable_errors = FALSE)
test_suite(enable_errors = TRUE)


#
# basic demonstration of error messages
#

# enable errors with a compiler flag
Sys.setenv(PKG_CXXFLAGS = '-DNCOMPILER_ENABLE_EIGEN_ERRORS')

# force (re)compilation of test functions
sourceCpp(file.path(base_dir, filename), rebuild = TRUE)

# dummy data
x = 1:10
y = 1:100

xmat = matrix(x, nrow = 1)
ymat = matrix(y, nrow = 1)
ymat2 = matrix(y, nrow = 2)

# connection of error to mat_mult c++ function comes from Rcpp; 
# detailed error comes directly from Eigen.  naturally, we can work on trying 
# to add more info to these messages at a later point in time, but this is the 
# simplest starting point.
mat_mult(xmat, ymat2)
