library(Rcpp)
library(testthat)

base_dir = file.path('sketches', '2021_10_06-eigenadd_oncode')

filename <- "R_op_generic.cpp"
sourceCpp(file.path(base_dir, filename))

X = matrix(1:2, ncol = 2)
# Y = matrix(sample(4), nrow = 2)
Y = sample(2)

# expected C++ output
Step1 = X + Y
# Step2 = Step1 + X
Ans = Step1 * as.numeric(Y)

# verify C++ output matches expected results
expect_equal(Ans, R_op_test(a = X, b = Y))
