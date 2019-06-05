context("Testing indexing")

testthat("drop arg variations" {
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
        returnType(numericArray(nDim = 3))
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
