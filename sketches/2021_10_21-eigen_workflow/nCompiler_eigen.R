library(nCompiler)
library(testthat)

# wrapper to "nCompile" the operation x + y given input and return types
nCompileAddition <- function(xType, yType, retType) {

  # R-based addition
  nAdd <- function(x, y) {
    ans <- x + y
    return(ans)
  }
  
  # nCompiler addition
  nf_add <- nFunction(
    fun = nAdd,
    argTypes = list(x = xType, y = yType), 
    returnType = retType
  )
  
  nCompile(nf_add)
}

#
# nCompiler units
#

# vector first, then matrix
add_v_m <- nCompileAddition(
  xType = 'numericVector', 
  yType = 'numericMatrix', 
  retType = 'numericMatrix'
)

# reverse order of previous arguments
add_m_v <- nCompileAddition(
  xType = 'numericMatrix', 
  yType = 'numericVector', 
  retType = 'numericMatrix'
)

add_m_m <- nCompileAddition(
  xType = 'numericMatrix', 
  yType = 'numericMatrix', 
  retType = 'numericMatrix'
)

add_v_a3 <- nCompileAddition(
  xType = 'numericVector', 
  yType = 'numericArray(nDim = 3)', 
  retType = 'numericArray(nDim = 3)'
)

# nCompiler will not support ops for non-conformable arrays
expect_error(
  add_a2_a3 <- nCompileAddition(
    xType = 'numericArray(nDim = 2)', 
    yType = 'numericArray(nDim = 3)', 
    retType = 'numericArray(nDim = 3)'
  )
)

# nCompiler will not support ops for non-conformable arrays
expect_error(
  add_m_a3 <- nCompileAddition(
    xType = 'numericMatrix', 
    yType = 'numericArray(nDim = 3)', 
    retType = 'numericArray(nDim = 3)'
  )
)

#
# Demos 
#

# test data
V1 <- as.numeric(1:3)
M1 <- matrix(as.numeric(1:3), nrow = 1)
M2 <- matrix(as.numeric(1:3), ncol = 1)
M3 <- matrix(as.numeric(1:6), ncol = 2)
M4 <- matrix(as.numeric(1:6), nrow = 1)
A1 <- array(as.numeric(1:3), dim = c(1, 1, 3))
A2 <- array(as.numeric(1:3), dim = c(1, 3, 1))
A3 <- array(as.numeric(1:3), dim = c(3, 1, 1))
A4 <- array(as.numeric(1:6), dim = c(1, 3, 2))

# nCompiler supports R-like matrix-vector behavior
expect_identical(V1 + M1, add_m_v(x = M1, y = V1))
expect_identical(V1 + M1, add_v_m(y = M1, x = V1))
expect_identical(V1 + M2, add_m_v(x = M2, y = V1))
expect_identical(V1 + M2, add_v_m(y = M2, x = V1))
expect_identical(V1 + A1, add_v_a3(x = V1, y = A1))
expect_identical(V1 + A2, add_v_a3(x = V1, y = A2))
expect_identical(V1 + A3, add_v_a3(x = V1, y = A3))

# nCompiler runtime errors on non-conformable inputs
expect_error(add_m_m(x = M1, y = M2))
expect_error(add_m_m(x = M1, y = M3))
expect_error(add_m_m(x = M1, y = M4))

# nCompiler runtime errors where R would normally use recycling rule
expect_error(add_m_v(x = M3, y = V1))
expect_error(add_v_a3(x = V1, y = A4))

# Issue: This should fail, but doesn't.  Do runtime arg types get validated?
add_v_m(x = V1, y = A3)
