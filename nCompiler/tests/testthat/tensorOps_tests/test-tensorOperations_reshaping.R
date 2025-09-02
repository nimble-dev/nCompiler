# tensorOperations: reshaping for 'vector OP row-matrix'

#
# test error trapping and generated C++ code that implements nCompiler's support 
# for evaluating binary operations between a vector and a row/column matrix or 
# more complicated array.  generated code automatically reshapes the vector to 
# match the dimensions of the row/column matrix or array.
#

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

# wrapper to "nCompile" the nested operations given input and return types
nCompileTwoOp <- function(xType, yType, zType, retType, op1, op2) {
  
  # R-based operation
  
  nOp <- eval(parse(text = gsub(
    pattern = 'op2', 
    replacement = op2, 
    x = gsub(
      pattern = 'op1', 
      replacement = op1, 
      x = "function(x, y, z) {
          ans <- (x op1 y) op2 z
          return(ans)
        }"
    )
  )))
  
  # nCompiler addition
  nf_op <- nFunction(
    fun = nOp,
    argTypes = list(x = xType, y = yType, z = zType), 
    returnType = retType
  )
  
  nCompile(nf_op)
}

# test data
set.seed(2021)
V1 <- as.numeric(sample(1:100, size = 3))
V2 <- as.numeric(sample(1:100, size = 3))
M1 <- matrix(as.numeric(sample(1:100, size = 3)), nrow = 1)
M2 <- matrix(as.numeric(sample(1:100, size = 3)), ncol = 1)
M3 <- matrix(as.numeric(sample(1:100, size = 6)), ncol = 2)
M4 <- matrix(as.numeric(sample(1:100, size = 6)), nrow = 1)
A1 <- array(as.numeric(sample(1:100, size = 3)), dim = c(1, 1, 3))
A2 <- array(as.numeric(sample(1:100, size = 3)), dim = c(1, 3, 1))
A3 <- array(as.numeric(sample(1:100, size = 3)), dim = c(3, 1, 1))
A4 <- array(as.numeric(sample(1:100, size = 6)), dim = c(1, 3, 2))


# %*% is not supported b/c it requires more specific reshaping rules
logical_optypes <- c('<=', '>=', '<', '>', '&', '|', '==', '!=')
optypes <- c('+', '-', '*', '/', logical_optypes)

# testing single operations
for(o in optypes) {
  
  message(paste('testing:', 'A', o, 'B'))
  
  #
  # nCompiler units
  #
  
  # return types
  retMatrix <- ifelse(o %in% logical_optypes, 'logicalMatrix', 'numericMatrix')
  retArray <- ifelse(o %in% logical_optypes, 
                     'logicalArray(nDim = 3)', 
                     'numericArray(nDim = 3)')
  
  # vector first, then matrix
  op_v_m <- nCompileOp(
    xType = 'numericVector', 
    yType = 'numericMatrix', 
    retType = retMatrix,
    op = o
  )
  
  # reverse order of previous arguments
  op_m_v <- nCompileOp(
    xType = 'numericMatrix', 
    yType = 'numericVector', 
    retType = retMatrix,
    op = o
  )
  
  op_m_m <- nCompileOp(
    xType = 'numericMatrix', 
    yType = 'numericMatrix', 
    retType = retMatrix,
    op = o
  )
  
  op_v_a3 <- nCompileOp(
    xType = 'numericVector', 
    yType = 'numericArray(nDim = 3)', 
    retType = retArray,
    op = o
  )
  
  # nCompiler will not support ops for non-conformable arrays
  expect_error(
    op_a2_a3 <- nCompileOp(
      xType = 'numericArray(nDim = 2)', 
      yType = 'numericArray(nDim = 3)', 
      retType = retArray,
      op = o
    )
  )
  
  # nCompiler will not support ops for non-conformable arrays
  expect_error(
    op_m_a3 <- nCompileOp(
      xType = 'numericMatrix', 
      yType = 'numericArray(nDim = 3)', 
      retType = retArray,
      op = o
    )
  )
  
  #
  # Demos 
  #
  
  opfun = match.fun(o)
  
  # nCompiler supports R-like matrix-vector behavior
  expect_identical(opfun(M1, V1), op_m_v(x = M1, y = V1))
  expect_identical(opfun(V1, M1), op_v_m(x = V1, y = M1))
  expect_identical(opfun(M2, V1), op_m_v(x = M2, y = V1))
  expect_identical(opfun(V1, M2), op_v_m(x = V1, y = M2))
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

# testing compositions of operations
for(o in optypes) {
  
  opfun = match.fun(o)
  
  for(o2 in optypes) {
    
    message(paste('testing:', '(A', o, 'B)', o2, 'C'))
    
    retMatrix <- ifelse(
      o2 %in% logical_optypes, 'logicalMatrix', 'numericMatrix'
    )
    
    op_m_v_v <- nCompileTwoOp(
      xType = 'numericMatrix',
      yType = 'numericVector', 
      zType = 'numericVector', 
      retType = retMatrix, 
      op1 = o, 
      op2 = o2
    )
    
    op_v_v_m <- nCompileTwoOp(
      xType = 'numericVector',
      yType = 'numericVector',
      zType = 'numericMatrix',
      retType = retMatrix, 
      op1 = o, 
      op2 = o2
    )
    
    opfun2 = match.fun(o2)
    
    # nCompiler supports composition of R-like matrix-vector behavior
    expect_identical(
      opfun2(opfun(M1, V1), V2),
      op_m_v_v(x = M1, y = V1, z = V2)
    )
    expect_identical(
      opfun2(opfun(V1, V2), M1),
      op_v_v_m(x = V1, y = V2, z = M1)
    )
    
  }
  
}
