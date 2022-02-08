Rcpp::sourceCpp(
  file.path('sketches', '2022_01_04-nMult', 'implicit_conversions.cpp')
)

n = 3

x = matrix(runif(n^2), nrow = n)
y = matrix(runif(n^2), nrow = n)
z = matrix(runif(n^2), nrow = n)

eval_test(x = x, y = y, z = z)
