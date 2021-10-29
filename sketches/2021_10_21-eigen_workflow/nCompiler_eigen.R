library(nCompiler)
library(testthat)

# wrapper to "nCompile" the operation x "op" y given input and return types
nCompileOp <- function(xType, yType, retType, op) {
  
  # R-based operation
  nOp <- eval(parse(text = gsub(
    pattern = 'op', 
    replacement = op, 
    x = "function(x, y) {
          ans <- x op y
          return(ans)
        }"
  )))
  
  # nCompiler addition
  nf_op <- nFunction(
    fun = nOp,
    argTypes = list(x = xType, y = yType), 
    returnType = retType
  )
  
  nCompile(nf_op)
}

# only currently supporting commutative binary, componentwise operations
optypes = c('+', '-', '*', '/')
for(o in optypes) {
  
  #
  # nCompiler units
  #
  
  # vector first, then matrix
  op_v_m <- nCompileOp(
    xType = 'numericVector', 
    yType = 'numericMatrix', 
    retType = 'numericMatrix',
    op = o
  )
  
  # reverse order of previous arguments
  op_m_v <- nCompileOp(
    xType = 'numericMatrix', 
    yType = 'numericVector', 
    retType = 'numericMatrix',
    op = o
  )
  
  op_m_m <- nCompileOp(
    xType = 'numericMatrix', 
    yType = 'numericMatrix', 
    retType = 'numericMatrix',
    op = o
  )
  
  op_v_a3 <- nCompileOp(
    xType = 'numericVector', 
    yType = 'numericArray(nDim = 3)', 
    retType = 'numericArray(nDim = 3)',
    op = o
  )
  
  # nCompiler will not support ops for non-conformable arrays
  expect_error(
    op_a2_a3 <- nCompileOp(
      xType = 'numericArray(nDim = 2)', 
      yType = 'numericArray(nDim = 3)', 
      retType = 'numericArray(nDim = 3)',
      op = o
    )
  )
  
  # nCompiler will not support ops for non-conformable arrays
  expect_error(
    op_m_a3 <- nCompileOp(
      xType = 'numericMatrix', 
      yType = 'numericArray(nDim = 3)', 
      retType = 'numericArray(nDim = 3)',
      op = o
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
  
  opfun = match.fun(o)
  
  # nCompiler supports R-like matrix-vector behavior
  expect_identical(opfun(V1, M1), op_m_v(x = M1, y = V1))
  expect_identical(opfun(V1, M1), op_v_m(y = M1, x = V1))
  expect_identical(opfun(V1, M2), op_m_v(x = M2, y = V1))
  expect_identical(opfun(V1, M2), op_v_m(y = M2, x = V1))
  expect_identical(opfun(V1, A1), op_v_a3(x = V1, y = A1))
  expect_identical(opfun(V1, A2), op_v_a3(x = V1, y = A2))
  expect_identical(opfun(V1, A3), op_v_a3(x = V1, y = A3))
  
  # nCompiler runtime errors on non-conformable inputs
  expect_error(op_m_m(x = M1, y = M2))
  expect_error(op_m_m(x = M1, y = M3))
  expect_error(op_m_m(x = M1, y = M4))
  
  # nCompiler runtime errors where R would normally use recycling rule
  expect_error(op_m_v(x = M3, y = V1))
  expect_error(op_v_a3(x = V1, y = A4))
  
  # # Issues: These should throw errors, but don't. Are runtime args validated?
  # op_v_m(x = V1, y = A3)  # essentially returns V1 "op" A3[1:3]
  # op_v_a3(x = V1, y = M1) # crashes R
}
