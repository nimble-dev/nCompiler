library(Rcpp)
library(microbenchmark)
library(testthat)

sourceCpp(file.path('sketches', '2021_12_22-profiling_dense_solvers', 
                    'dense_solvers.cpp'))

set.seed(2021)

# dimension of problem
n = 50

# create positive definite matrix
A = matrix(data = runif(n = n^2), nrow = n)
A = t(A) %*% A

# create test solution
x = matrix(data = runif(n), nrow = n)

# create test output
b = A %*% x

# verify the functions work
expect_equal(LLTClassSolver(A = A, b = b), as.numeric(x))
expect_equal(LowerSolver(A = A, b = b), as.numeric(x))
expect_equal(UpperSolver(A = A, b = b), as.numeric(x))
expect_equal(LDLTClassSolver(A = A, b = b), as.numeric(x))

# benchmark: we see that the class-based LLT solver is faster, but about the 
# same speed as the manual forward-backward LLT solvers.  The class-based LDLT 
# solver is a similar speed as the LLT solvers, but claims via documentation to 
# be more numerically stable than the LLT solvers although it is somewhat slower 
# than the LLT solvers as the problem dimension grows.
microbenchmark(
  LLTClassSolver(A = A, b = b),
  LowerSolver(A = A, b = b),
  UpperSolver(A = A, b = b),
  LDLTClassSolver(A = A, b = b), 
  times = 1e3
)
