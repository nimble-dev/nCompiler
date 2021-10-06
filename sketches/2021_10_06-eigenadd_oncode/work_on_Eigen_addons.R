library(Rcpp)

# This tests that compilation works and Eigen is found
filename <- "Rcpp_works.cpp"
sourceCpp(filename)
vec_plus(1:3, 2:4)

# This tests a prototype of getting into internals for "smart" run-time decisions.
filename <- "Rplus_test1.cpp"
sourceCpp(filename)
Rplus_test1(1:3, 2:4)

# This tests a prototype of using internals to do a "smart" matrix + vector op.
filename <- "Rplus_mat_vec_test1.cpp"
sourceCpp(filename)
Rplus_mat_vec_test1(matrix(1:3), 2:4)
Rplus_mat_vec_test1(matrix(1:3, nrow = 1), 2:4)
