# demonstrate templated binary operations between matrix and vector objects

library(Rcpp)
library(testthat)

# source code
base_dir = file.path('sketches', '2021_10_06-eigenadd_oncode')
filename <- "R_op_generic.cpp"

# compile test functions
sourceCpp(file.path(base_dir, filename))

# main test objects
V1 <- as.numeric(1:3)
M1 <- matrix(as.numeric(1:3), nrow = 1)
M2 <- matrix(as.numeric(1:3), ncol = 1)
M3 <- matrix(as.numeric(1:6), ncol = 2)
M4 <- matrix(as.numeric(1:6), nrow = 1)
A1 <- array(as.numeric(1:3), dim = c(1, 1, 3))
A2 <- array(as.numeric(1:3), dim = c(1, 3, 1))
A3 <- array(as.numeric(1:3), dim = c(3, 1, 1))
A4 <- array(as.numeric(1:6), dim = c(1, 3, 2))

# additional test object
Xdim = c(2,6,3,5,2)
X = array(data = 1:prod(Xdim), dim = Xdim)
Ydim = c(2,45,4)
Y = array(data = 1:prod(Ydim), dim = Ydim)

#
# Merge arrays of arbitrarily different sizes, but with same number of elements
#

# tensor-tensor addition (not a design goal, but an accidental capability)
expect_identical(
  array(data = as.numeric(X) + as.numeric(Y), dim = Xdim), 
  R_add_test_5_3(a = X, b = Y)
)
expect_identical(
  array(as.numeric(A1) + as.numeric(M1), dim = dim(A1)),
  R_add_test_3_2(A1, M1)
)
expect_identical(
  array(as.numeric(A2) + as.numeric(M1), dim = dim(A2)),
  R_add_test_3_2(A2, M1)
)
expect_identical(
  array(as.numeric(A3) + as.numeric(M1), dim = dim(A3)),
  R_add_test_3_2(A3, M1)
)
#  - Note: R will error for this type of operation
expect_error(X + Y)
expect_error(M1 + A1)
expect_error(M1 + A2)
expect_error(M1 + A3)

# matrix-vector addition
expect_identical(V1 + M1, R_add_test_2_1(M1, V1))
expect_identical(V1 + M2, R_add_test_2_1(M2, V1))

# array-vector addition
expect_identical(V1 + A1, R_add_test_3_1(A1, V1))
expect_identical(V1 + A2, R_add_test_3_1(A2, V1))
expect_identical(V1 + A3, R_add_test_3_1(A3, V1))

# matrix-matrix addition with row vs. column matrix
#  - R error: non-conformable arrays
expect_error(M1 + M2)
#  - current method will not register an error; output dim depends on order
R_add_test_2_2(M1, M2)
R_add_test_2_2(M2, M1)


#
# Nest/chain operations
#

expect_identical((V1 + M1) * V1, R_add_mult_test_2_1(M1, V1))
expect_identical((V1 + M1) * V1, R_add_mult_test_2_1_alt(M1, V1))


#
# Error when trying to merge arrays with different elements
#

# "returns" custom error from Rcpp
R_add_test_2_2(M1, M3)
R_add_test_2_2(M1, M4)

# "returns" default error from Rcpp
R_add_test_2_2_default_error(M1, M3)
R_add_test_2_2_default_error(M1, M4)

# "returns" error in scenarios where R would usually use recycling rule
R_add_test_2_1(M3, V1)
R_add_test_3_1(A4, V1)

