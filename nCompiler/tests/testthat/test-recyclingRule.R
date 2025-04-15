test_that('basic recycling rule vector-to-matrix works', {
  
  nf = nFunction(
    fun = function(x, y) {
      ans = matrix()
      cppLiteral('ans = recyclingTensor(x, y)')
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      y = 'numericMatrix()'
    ),
    returnType = 'numericMatrix()'
  )
  
  cpp = nCompile(nf)
  
  x = 1:3
  y = matrix(0, ncol = 3, nrow = 4)
  
  expect_identical(x + y, cpp(x, y))
  
})

test_that('basic recycling rule vector-to-vector works', {
  
  nf = nFunction(
    fun = function(x, y) {
      ans = numeric()
      cppLiteral('ans = recyclingTensor(x, y)')
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      y = 'numericVector()'
    ),
    returnType = 'numericVector()'
  )
  
  cpp = nCompile(nf)
  
  x = 1:3
  y = numeric(20)
  
  suppressWarnings({res = x + y})
  
  expect_identical(res, cpp(x, y))
  
})

test_that('recycling rule can take and emit tensor expressions', {
  
  nf = nFunction(
    fun = function(x, y, z) {
      ans = matrix()
      cppLiteral('ans = recyclingTensor(x * z, y.exp()) + y')
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      y = 'numericMatrix()',
      z = 'numericVector()'
    ),
    returnType = 'numericMatrix()'
  )
  
  cpp = nCompile(nf)
  
  x = 1:3
  z = sqrt(1:3)
  y = matrix(1:12, ncol = 3, nrow = 4)
  
  expect_identical(x * z + y, cpp(x, y, z))
  
})
