context("Testing indexing")

test_that("drop arg variations give correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, ] ## no drop arg => TRUE
        return(ans)
        returnType(numericArray(nDim = 2))
      }),
      test2 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = TRUE] ## explicit TRUE
        return(ans)
        returnType(numericArray(nDim = 2))
      }),
      test3 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = FALSE] ## explicit FALSE
        return(ans)
        returnType(numericArray(nDim = 3))
      }),
      test4 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = 0] ## drop = 0 => FALSE
        return(ans)
        returnType(numericArray(nDim = 3))
      }),
      test5 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = 1] ## drop = 1 => TRUE
        return(ans)
        returnType(numericArray(nDim = 2))
      }),
      test6 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = "T"] ## drop = "T" => TRUE
        return(ans)
        returnType(numericArray(nDim = 2))
      }),
      test7 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = "F"] ## drop = "F" => FALSE
        return(ans)
        returnType(numericArray(nDim = 3))
      }),
      test8 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = NA] ## drop = NA => TRUE
        return(ans)
        returnType(numericArray(nDim = 2))
      }),
      test9 = nFunction(function(x = numericArray(nDim = 3)) {
        i  <- 1
        ans <- x[i, 2:3, , drop = NaN] ## drop = NaN => TRUE
        return(ans)
        returnType(numericArray(nDim = 2))
      })
    )
  )

  cobj <- nCompile_nClass(nC)$new()

  x <- array(1:27, c(3, 3, 3))

  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    expect_equal(
      nC$public_methods[[test_i]](x), ## R
      cobj[[test_i]](x)               ## C++
    )
  }

})

test_that("indexing arg variations give correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction(
        ## empty indexing args works
        function(x = numericArray(nDim = 3)) {
          ans <- x[]
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      ),
      test2 = nFunction(
        ## basic scalar expressions work as indexing args
        function(x = numericArray(nDim = 3)) {
          i  <- 1
          ans <- x[i+1, 2:3, ]
          return(ans)
          returnType(numericArray(nDim = 2))
        }
      ),
      test3 = nFunction(
        ## basic scalar expressions work as indexing args, drop = FALSE
        function(x = numericArray(nDim = 3)) {
          i  <- 1
          ans <- x[2*i + 1, 2:3, , drop = FALSE]
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      ),
      test4 = nFunction(
        ## using indexing args like 3:3 gives same result in compiled/uncompiled
        ## output when drop = FALSE ...
        ## see test_that("3:3 indexing arg doesn't drop dimension", ...) below
        function(x = numericArray(nDim = 3)) {
          ans <- x[3:3, 1:2, 3, drop = FALSE]
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      )
    )
  )

  cobj <- nCompile_nClass(nC)$new()

  x <- array(1:27, c(3, 3, 3))

  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    expect_equal(
      nC$public_methods[[test_i]](x), ## R
      cobj[[test_i]](x)               ## C++
    )
  }

})

test_that("3:3 style indexing arg doesn't drop dimension", {
  nf <- nFunction(
    function(x = numericArray(nDim = 3)) {
      i <- 1
      ans <- x[3:3, i:i, 3]
      return(ans)
      returnType(numericArray(nDim = 2))
    }
  )
  nfc <- nCompile_nFunction(nf)
  x <- array(1:27, c(3, 3, 3))
  expect_silent(nfc(x))
})

test_that("compilation of [ throws errors as expected ", {
  nf1 <- nFunction( ## call that is not ':' as indexing arg
    function(x = numericArray(nDim = 3)) {
      ans <- x[exp(1:4), 2:3, 3]
      return(ans)
      returnType(numericArray(nDim = 2))
    }
  )
  nf2 <- nFunction( ## logical literal as indexing arg
    function(x = numericArray(nDim = 3)) {
      ans <- x[1, TRUE, 2:3]
      return(ans)
      returnType(numericArray(nDim = 1))
    }
  )
  nf3 <- nFunction( ## non-literal drop arg
    function(x = numericArray(nDim = 3)) {
      drop_expr <- TRUE
      ans <- x[1, , 3, drop = drop_expr]
      return(ans)
      returnType(numericArray(nDim = 1))
    }
  )
  nf4 <- nFunction( ## wrong number of indexing args, no drop arg
    function(x = numericArray(nDim = 3), y = numericVector()) {
      ans <- x[1, 3]
      return(ans)
      returnType(numericArray(nDim = 2))
    }
  )
  nf5 <- nFunction( ## wrong number of indexing args with drop arg
    function(x = numericArray(nDim = 3)) {
      ans <- x[1, 3, drop = FALSE]
      return(ans)
      returnType(numericArray(nDim = 3))
    }
  )
  nf6 <- nFunction( ## bad indexing arg dimension
    function(x = numericArray(nDim = 3), y = numericMatrix()) {
      ans <- x[y]
      return(ans)
      returnType(numericArray(nDim = 2))
    }
  )
  expect_error(nCompiler_nFunction(nf1))
  expect_error(nCompiler_nFunction(nf2))
  expect_error(nCompiler_nFunction(nf3))
  expect_error(nCompiler_nFunction(nf4))
  expect_error(nCompiler_nFunction(nf5))
  expect_error(nCompiler_nFunction(nf6))
})
