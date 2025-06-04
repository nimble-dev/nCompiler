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

test_that('recycling rule with density functions (dnorm)', {

  nf = nFunction(
    fun = function(x, mu, sd, logval) {
      ans = dnorm(x, mu, sd, logval[1])
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      mu = 'numericMatrix()',
      sd = 'numericMatrix()',
      logval = 'numericVector()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  x = rnorm(n = 10)
  mu = matrix(rnorm(n = length(x)), ncol = 2)
  sd = matrix(rexp(n = length(x)), ncol = 1)
  logval = rbinom(n = length(x), size = 1, prob = .5)

  expect_identical(
    nc(x = x, mu = mu, sd = sd, logval = logval),
    dnorm(x = x, mean = mu, sd = sd, log = logval[1])
  )

})

test_that('density functions require scalar log argument (dnorm)', {

  nf = nFunction(
    fun = function(x, mu, sd, logval) {
      ans = dnorm(x, mu, sd, logval)
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      mu = 'numericMatrix()',
      sd = 'numericMatrix()',
      logval = 'numericVector()'
    ),
    returnType = 'numericVector()'
  )

  expect_error(nc = nCompile(nf))
  
})

test_that('density functions with scalar arguments (dnorm)', {

  nf = nFunction(
    fun = function(x, mu, sd, logval) {
      ans = dnorm(x, mu, sd, logval)
      return(ans)
    },
    argTypes = list(
      x = 'double()',
      mu = 'double()',
      sd = 'double()',
      logval = 'double()'
    ),
    returnType = 'double()'
  )

  nc = nCompile(nf)

  x = rnorm(n = 1)
  mu = rnorm(n = length(x))
  sd = rexp(n = length(x))
  logval = rbinom(n = length(x), size = 1, prob = .5)

  expect_identical(
    nc(x = x, mu = mu, sd = sd, logval = logval),
    dnorm(x = x, mean = mu, sd = sd, log = logval)
  )

})

test_that('density functions with mixed arguments (dnorm)', {

  nf = nFunction(
    fun = function(x, mu, sd, logval) {
      ans = dnorm(x, mu, sd, logval)
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      mu = 'double()',
      sd = 'numericMatrix()',
      logval = 'double()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  x = rnorm(n = 10)
  mu = rnorm(n = 1)
  sd = matrix(rexp(n = length(x)), ncol = 2)
  logval = rbinom(n = 1, size = 1, prob = .5)

  expect_identical(
    nc(x = x, mu = mu, sd = sd, logval = logval),
    dnorm(x = x, mean = mu, sd = sd, log = logval)
  )

})

test_that('density functions with tensor expressions in/out (dnorm)', {

  nf = nFunction(
    fun = function(x, mu, var, logval) {
      ans = exp(dnorm(x, mu, sqrt(var), logval))
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      mu = 'double()',
      var = 'numericMatrix()',
      logval = 'double()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  x = rnorm(n = 10)
  mu = rnorm(n = 1)
  var = matrix(rexp(n = length(x)), ncol = 2)
  sd = sqrt(var)
  logval = rbinom(n = 1, size = 1, prob = .5)

  expect_identical(
    nc(x = x, mu = mu, var = var, logval = logval),
    exp(dnorm(x = x, mean = mu, sd = sd, log = logval))
  )

})

test_that('density assignment to slice (dnorm)', {

  nf = nFunction(
    fun = function(x, mu, var, logval) {
      ans = numeric(length = 10)
      ans[2:5] = dnorm(x, mu, sqrt(var), logval)
      return(ans)
    },
    argTypes = list(
      x = 'numericVector()',
      mu = 'double()',
      var = 'numericVector()',
      logval = 'double()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  x = rnorm(n = 4)
  mu = rnorm(n = 1)
  var = rexp(n = length(x))
  sd = sqrt(var)
  logval = rbinom(n = 1, size = 1, prob = .5)

  ans = numeric(10)
  ans[2:5] = dnorm(x = x, mean = mu, sd = sd, log = logval)

  expect_identical(
    nc(x = x, mu = mu, var = var, logval = logval),
    ans
  )

})


test_that('density functions with blockRef args (dnorm)', {

  nf <- nFunction(
    function(
      x = numericVector(), 
      mu = numericVector(), 
      sd = numericVector(),
      logval = double(), 
      d = numericVector()
    ) {
      d <- dnorm(x = x, mean = mu, sd = sd, log = logval)
      return(d)
    },
    blockRefArgs = 'd',
    returnType = 'numericVector'
  )

  nc = nCompile(nf)

  n = 100
  d = numeric(n)
  x = rnorm(n = n)
  mu = rnorm(n = n)
  sd = rexp(n = n)
  logval = TRUE

  res = nc(x = x, mu = mu, sd = sd, logval = logval, d = d)

  expect_equal(res, d)

})

test_that('random generation with vector size argument (rnorm)', {

  nf = nFunction(
    fun = function(n, mu, sd) {
      ans = rnorm(n, mu, sd)
      return(ans)
    },
    argTypes = list(
      n = 'numericVector()',
      mu = 'double()',
      sd = 'numericMatrix()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  n = rep(1,10)

  set.seed(2025)

  cres = nc(n = n, mu = 0, sd = matrix(1, nrow = 1, ncol = 3))

  set.seed(2025)

  rres = rnorm(n = n, mean = 0 , sd = 1)

  expect_identical(cres, rres)

})

test_that('random generation with scalar size argument (rnorm)', {

  nf = nFunction(
    fun = function(n, mu, sd) {
      ans = rnorm(n, mu, sd)
      return(ans)
    },
    argTypes = list(
      n = 'double()',
      mu = 'double()',
      sd = 'numericMatrix()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  n = 3

  set.seed(2025)

  cres = nc(n = n, mu = 0, sd = matrix(1, nrow = 1, ncol = 3))

  set.seed(2025)

  rres = rnorm(n = n, mean = 0 , sd = 1)

  expect_identical(cres, rres)

})

test_that('random generation with 0-dim tensor size argument (rnorm)', {

  nf = nFunction(
    fun = function(n, mu, sd) {
      ans = rnorm(n[1], mu, sd)
      return(ans)
    },
    argTypes = list(
      n = 'numericVector()',
      mu = 'double()',
      sd = 'numericMatrix()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  n = rep(10,4)

  set.seed(2025)

  cres = nc(n = n, mu = 0, sd = matrix(1, nrow = 1, ncol = 3))

  set.seed(2025)

  rres = rnorm(n = n[1], mean = 0 , sd = 1)

  expect_identical(cres, rres)

})

test_that('random generation with tensor expression in/out (runif)', {

  nf = nFunction(
    fun = function(n, lower) {
      ans = -runif(n, lower, 1 + lower)
      return(ans)
    },
    argTypes = list(
      n = 'numericVector()',
      lower = 'double()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  n = rep(10,4)

  lower = 3 

  set.seed(2025)

  cres = nc(n = n, lower = lower)

  set.seed(2025)

  rres = -runif(n = n, min = lower, max = 1 + lower)

  expect_identical(cres, rres)

})

test_that('random generation assignment to subset (runif)', {

  nf = nFunction(
    fun = function(lower) {
      ans = numeric(length = 10)
      ans[2:5] = -runif(4, lower, 1 + lower)
      return(ans)
    },
    argTypes = list(
      lower = 'double()'
    ),
    returnType = 'numericVector()'
  )

  nc = nCompile(nf)

  lower = 3 

  set.seed(2025)

  cres = nc(lower = lower)

  set.seed(2025)

  rres = numeric(10)
  rres[2:5] = -runif(n = 4, min = lower, max = 1 + lower)

  expect_identical(cres, rres)

})
