library(nCompiler)

# setup_nCompLocal()

compilerOptions = nOptions('compilerOptions')
compilerOptions$throwEigenErrors = TRUE
set_nOption('compilerOptions', compilerOptions)

# test input
x = matrix(1:10, nrow = 1)
y = runif(n = 10)
y2 = runif(n = 12)

# generic addition function
nCAdd <- function(x, y) {
  ans <- x + y
  return(ans)
}

# matrix-vector addition
nf_matVecAdd <- nFunction(
  fun = nCAdd,
  argTypes = list(x = 'numericMatrix', y = 'numericVector'),
  returnType = 'numericMatrix'
)

# matrix-matrix addition
nf_matMatAdd <- nFunction(
  fun = nCAdd,
  argTypes = list(x = 'numericMatrix', y = 'numericMatrix'),
  returnType = 'numericMatrix'
)

# compile functions
Cnf_matVecAdd <- nCompile(nf_matVecAdd)
Cnf_matMatAdd <- nCompile(nf_matMatAdd)

# return row/colvec as appropriate, no reshaping needed when eigen_assert is off
Cnf_matVecAdd(x,y)
Cnf_matVecAdd(t(x),y)

# same results when inputs are both matrices; output dims match first argument
Cnf_matMatAdd(x,as.matrix(y))
Cnf_matMatAdd(t(x),as.matrix(y))

# same recycling issue; last elements pull from unallocated memory
Cnf_matMatAdd(as.matrix(y2),x)
