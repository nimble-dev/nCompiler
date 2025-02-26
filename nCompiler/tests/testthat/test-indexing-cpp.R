## These were from working files for general indexing.
## They should be cleaned up into testthat format and compile from the cpp directory.

#library(testthat)
library(Rcpp)

test_that("C++ code for indexing and chipping works", {
  cppfile <- system.file(
    file.path('tests', 'testthat', 'cpp', 'general_indexing_examples.cpp'),
    package = 'nCompiler')
  nCompiler:::QuietSourceCpp(cppfile)

  # warming up and making sure Rcpp works:
  # Should print: hello world.  first element is 10
  # hw(10:20)

  ##### SINGLE CHIPS
  # Single access via chip (scalar index)
  x <- matrix(1:20, nrow = 4)
  expect_equal(ex1(x), x[3,]) # x.chip(2, 0)
  expect_equal(ex1b(x), x[3,]) # macro version: ISINGLE_(0, 2, x)

  # Single assign via chip
  x <- matrix(1:20, nrow = 4)
  v <- x[3,] + 100
  ans <- x; ans[3,] <- v
  expect_equal(ex2(x, v), ans) # x.chip(2,0)=v
  expect_equal(ex2b(x, v), ans) #  # nCompiler::IndexByScalar<0>().op(2, x) = v;

  ##### CHIP OF CHIP
  ## Access
  x3 <- array(1:(4*5*6), dim = c(4, 5, 6))
  expect_equal(ex1p1(x3), x3[,4,3]) # x.chip<2>(2).chip<1>(3)
  expect_equal(ex1p1b(x3), x3[,4,3]) # nCompiler::IndexByScalar<1>().op(3, nCompiler::IndexByScalar<2>().op(2, x));

  ## Assign
  x3 <- array(1:(4*5*6), dim = c(4, 5, 6))
  v <- x3[,4,3] + 100
  test <- ex2p1(x3, v) # x.chip<2>(2).chip<1>(3) = v;
  ans <- x3
  ans[, 4, 3] <- v
  expect_equal(test, ans)
  test2 <- ex2p1b(x3, v) # ISINGLE_(1, 3, ISINGLE_(2, 2, x))=v;
  expect_equal(test2, ans)

  ###### ELEMENT FROM CHIP OF CHIP USING TENSORREF
  ## I am not sure if MakeTensorRef continues to be used or was drafted and then not needed.
  ## Access
  x3 <- array(1:(4*5*6), dim = c(4, 5, 6))
  expect_equal(ex1p2(x3), x3[2, 4, 3])
  expect_equal(ex1p2b(x3), x3[2, 4, 3])

  ## Assign
  test <- ex2p2(x3, 1000)
  ans <- x3
  ans[2 , 4, 3] <- 1000
  expect_equal(test, ans)
  test2 <- ex2p2b(x3, 1000)
  expect_equal(test2, ans)

  #####  SINGLE INDEX VECS
  # Single access via indexing vector
  x <- matrix(1:20, nrow = 4)
  iv <- c(1, 3) # 2, 4 in R
  expect_equal(ex3(x, iv), x[c(2, 4),]) # nCompiler::IndexByVec<0>().op(iv,x);

  # Single assign via indexing vector
  x <- matrix(1:20, nrow = 4)
  v <- x[c(2, 4), ] + 100
  iv <- c(1, 3) # 2, 4 in R
  test <- ex4(x, iv, v) #   nCompiler::IndexByVec<0>().op(iv,x) = v;
  ans <- x; ans[c(2, 4), ] <- v
  expect_equal(test, ans)

  #####  INDEX VEC OF INDEX VEC
  # Access
  x <- matrix(1:20, nrow = 4)
  iv <- c(1, 3) # 2, 4 in R
  iv2 <- c(3, 4) # 4, 5 in R
  expect_equal(ex3p1(x, iv, iv2), x[c(2,4), c(4, 5)]) # nCompiler::IndexByVec<1>().op(iv2, nCompiler::IndexByVec<0>().op(iv,x));

  # Assign
  x <- matrix(1:20, nrow = 4)
  iv <- c(1, 3) # 2, 4 in R
  iv2 <- c(3, 4) # 4, 5 in R
  v <- x[c(2,4), c(4, 5)] + 100
  test <- ex4p1(x, iv, iv2, v)
  ans <- x; ans[c(2,4), c(4, 5)] <- v
  expect_equal(test, ans)

  ###### Index vec of index scalar and vice versa
  ## Access
  x <- matrix(1:20, nrow = 4)
  iv <- c(1, 3) # 2, 4 in R
  expect_equal(ex3p2(x, iv), x[2, iv+1])
  expect_equal(ex4p2(x, iv), x[2, iv+1])

  ## Assign
  x <- matrix(1:20, nrow = 4)
  iv <- c(1, 3) # 2, 4 in R
  v <- x[2, iv+1] + 100
  test <- ex3p3(x, iv, v)
  ans <- x; ans[2, iv+1] <- v
  expect_equal(test, ans)
  test2 <- ex4p3(x, iv, v)
  expect_equal(test2, ans)

  ############ SINGLE SEQUENCES
  # Single access via sequence
  x <- matrix(1:20, nrow = 4)
  expect_equal(ex5(x), x[2:3,])
  expect_equal(ex5b(x), x[2:3,])

  # Single access via sequence in two indices
  x <- matrix(1:20, nrow = 4)
  expect_equal(ex5p1(x), x[2:3, 2:4])

  # Single assign via sequence
  x <- matrix(1:20, nrow = 4)
  v <- x[2:3,] + 100
  test <- ex6(x, v) # CRASH!
  ans <- x; ans[2:3,] <- v
  expect_equal(test, ans)

  ############ CHIP OF INDEX SEQ
  # Access
  x <- matrix(1:20, nrow = 4)
  expect_equal(ex7(x), x[2:3, 3])

  # Assign
  x <- matrix(1:20, nrow = 4)
  v <- x[2:3, 3] + 100
  test <- ex8(x, v)
  ans <- x; ans[2:3, 3] <- v
  expect_equal(test, ans)

  ############ INDEX SEQ OF CHIP
  # Access
  x <- matrix(1:20, nrow = 4)
  expect_equal(ex7p1(x), x[2:3, 3])

  # Assign
  x <- matrix(1:20, nrow = 4)
  v <- x[2:3, 3] + 100
  test <- ex8p1(x, v)
  ans <- x; ans[2:3, 3] <- v
  expect_equal(test, ans)


  ############ INDEX VEC OF INDEX SEQ
  # Access
  x <- matrix(1:20, nrow = 4)
  iv <- sample(1:3, 8, replace = TRUE)
  test <- ex9(x, iv-1)
  ans <- x[, 2:4][,iv]
  expect_equal(test, ans)

  # Assign
  x <- matrix(1:20, nrow = 4)
  iv <- c(1, 3)
  v <- x[, 2:4][,iv] + 100
  test <- ex10(x, iv-1, v)
  ans <- x; ans[, 2:4][,iv] <- v
  expect_equal(test, ans)

})
