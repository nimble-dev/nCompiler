## not working
context("Testing indexing")

test_that("drop arg variations give correct results, 3D input", {
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
  x <- array(1:105, c(3, 5, 7))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})

test_that("indexing arg variations give correct results, 3D input", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## empty indexing args works
        function(x = numericArray(nDim = 3)) {
          ans <- x[]
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      ),
      test2 = nFunction( ## basic scalar expressions work as indexing args
        function(x = numericArray(nDim = 3)) {
          i  <- 1
          ans <- x[i+1, 2:3, ]
          return(ans)
          returnType(numericMatrix())
        }
      ),
      test3 = nFunction( ## basic scalar expressions work as indexing args, drop = FALSE
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
  x <- array(1:105, c(3, 5, 7))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})

test_that("assignment involving indexing give correct results, 3D input", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## assign scalar to indexed block (does not work)
        function(x = numericArray(nDim = 3)) {
          ans <- x
          ## ans[1:2, 2:3, ] <- 0 ## this fails
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      ),
      test2 = nFunction( ## assign scalar to indexed scalar (does not work)
        function(x = numericArray(nDim = 3)) {
          ans <- x
          ans[2, 3, 7] <- 0 ## this fails
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      ),
      test3 = nFunction( ## assign indexed block to indexed block
        function(x = numericArray(nDim = 3)) {
          ans <- x
          ans[2, 2:3, ] <- x[1, 3:4, ]
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- array(1:105, c(3, 5, 7))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})

test_that("expressions involving indexing give correct results, 3D input", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## exp of indexed block
        function(x = numericArray(nDim = 3)) {
          i  <- 1
          ans <- exp(x[2, 2:3, ])
          return(ans)
          returnType(numericMatrix())
        }
      ),
      test2 = nFunction( ## add indexed blocks
        function(x = numericArray(nDim = 3)) {
          ans <- x[1, 3:4, 4] + x[1, 5:6, 2]
          return(ans)
          returnType(numericVector())
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- array(1:105, c(3, 8, 7))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})

test_that("scalar input gives correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## return scalar, empty indexing arg
        function(x = numericScalar()) {
          ans <- x[]
          return(ans)
          returnType(numericScalar())
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- 3
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    expect_equal(
      nC$public_methods[[test_i]](x), ## R
      cobj[[test_i]](x)               ## C++
    )
  }
})

test_that("vector input gives correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## return vector
        function(x = numericVector()) {
          ans <- x[1:4]
          return(ans)
          returnType(numericVector())
        }
      ),
      test2 = nFunction( ## return scalar
        function(x = numericVector()) {
          ans <- x[3]
          return(ans)
          returnType(numericScalar())
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- 1:11
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})


test_that("matrix input gives correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## return matrix
        function(x = numericMatrix()) {
          ans <- x[3:6, 1:2]
          return(ans)
          returnType(numericMatrix())
        }
      ),
      test2 = nFunction( ## return vector
        function(x = numericMatrix()) {
          ans <- x[2, 1:3]
          return(ans)
          returnType(numericVector())
        }
      ),
      test3 = nFunction( ## return scalar
        function(x = numericMatrix()) {
          ans <- x[3, 1]
          return(ans)
          returnType(numericScalar())
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- matrix(1:21, c(7, 3))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})

test_that("3-dimensional input array gives correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## return matrix
        function(x = numericArray(nDim = 3)) {
          ans <- x[3, 1:4, ]
          return(ans)
          returnType(numericMatrix())
        }
      ),
      test2 = nFunction( ## return vector
        function(x = numericArray(nDim = 3)) {
          ans <- x[3, 1:4, 2]
          return(ans)
          returnType(numericVector())
        }
      ),
      test3 = nFunction( ## return scalar
        function(x = numericArray(nDim = 3)) {
          ans <- x[3, 1, 2]
          return(ans)
          returnType(numericScalar())
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- array(1:84, c(3, 4, 7))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})

test_that("4-dimensional input array gives correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## return 4D array
        function(x = numericArray(nDim = 4)) {
          ans <- x[2:3, , 2:4, ]
          return(ans)
          returnType(numericArray(nDim = 4))
        }
      ),
      test2 = nFunction( ## return 3D array
        function(x = numericArray(nDim = 4)) {
          ans <- x[3, 1:4, 2:4, ]
          return(ans)
          returnType(numericArray(nDim = 3))
        }
      ),
      test3 = nFunction( ## return matrix
        function(x = numericArray(nDim = 4)) {
          ans <- x[3, 1:4, 3, ]
          return(ans)
          returnType(numericMatrix())
        }
      ),
      test4 = nFunction( ## return vector
        function(x = numericArray(nDim = 4)) {
          ans <- x[3, 1:4, 2, 11]
          return(ans)
          returnType(numericVector())
        }
      ),
      test5 = nFunction( ## return scalar
        function(x = numericArray(nDim = 4)) {
          ans <- x[3, 1, 2, 5]
          return(ans)
          returnType(numericScalar())
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- array(1:924, c(3, 7, 4, 11))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
  }
})

test_that("5-dimensional input array gives correct results", {
  nC <- nClass(
    Cpublic = list(
      test1 = nFunction( ## return 4D array
        function(x = numericArray(nDim = 5)) {
          ans <- x[1:2, , 2:4, 3, ]
          return(ans)
          returnType(numericArray(nDim = 4))
        }
      )
    )
  )
  cobj <- nCompile_nClass(nC)$new()
  x <- array(1:2310, c(2, 3, 7, 5, 11))
  for (i in seq_along(ls(nC$public_methods)[-1])) {
    test_i  <- paste0('test', i)
    outC <- cobj[[test_i]](x)
    outR <- nC$public_methods[[test_i]](x)
    if (is.array(outC) && length(attributes(outC)$dim) == 1)
      attributes(outC)$dim <- NULL
    expect_equal(outC, outR)
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
  nf7 = nFunction( ## index a scalar with more than 1 indexing arg
    function(x = numericArray(nDim = 3)) {
      a <- x[3, 1, 2]
      ans <- a[1, 1]
      return(ans)
      returnType(numericScalar())
    }
  )
  nf8 = nFunction( ## index a scalar with non-scalar
    function(x = numericArray(nDim = 3)) {
      a <- x[3, 1, 2]
      ans <- a[1:2]
      return(ans)
      returnType(numericScalar())
    }
  )
  nf9 = nFunction( ## index a scalar with a scalar (not currently supported)
    function(x = numericArray(nDim = 3)) {
      a <- x[3, 1, 2]
      ans <- a[1]
      return(ans)
      returnType(numericScalar())
    }
  )
  expect_error(nCompile_nFunction(nf1))
  expect_error(nCompile_nFunction(nf2))
  expect_error(nCompile_nFunction(nf3))
  expect_error(nCompile_nFunction(nf4))
  expect_error(nCompile_nFunction(nf5))
  expect_error(nCompile_nFunction(nf6))
  expect_error(nCompile_nFunction(nf7))
  expect_error(nCompile_nFunction(nf8))
  expect_error(nCompile_nFunction(nf9))
})
