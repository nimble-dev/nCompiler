Rcpp::sourceCpp('sketches/2021_11_05-trycatch/nCompiler_units_1.cpp')

# throws error
nFun_1_NFID_1(x = matrix(1:10, nrow = 1), y = matrix(2:11, ncol = 1))

# returns ok
nFun_1_NFID_1(x = matrix(1:10, nrow = 1), y = matrix(2:11, nrow = 1))
