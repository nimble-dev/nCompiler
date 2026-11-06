library(Rcpp)
test_that("basic uses of StridedTensorMap work",{
  cppfile <- system.file(
    file.path('tests', 'testthat', 'cpp', 'StridedTensorMap_tests.cpp'),
    package = 'nCompiler')
  test <- nCompiler:::QuietSourceCpp(cppfile)
  x <- array(1:(6*5*4), dim = c(6, 5, 4))
  expect_equal(STM1(x), x[, 2:3, ][2, 2, 3])
  expect_equal(STM2(x), x[, 2:3, ])
  expect_equal(STM3(x), log(x[, 2:3, ]))
  expect_equal(STM4(x), log(x[, 2:3, ]))
  expect_equal(STM5(x), {temp <- x; temp[,,] <- 0; temp[, 2:3, ] <- x[, 2:3, ]; temp})
  expect_equal(STM6(x), x[2:5, 3, 2:3])
  expect_equal(STM7(x), x[ 5, 1:3, 2:3 ])
  expect_equal(STM8(x), x[5, 1:3, 2])
  expect_equal(STM9(x), x[5, , 2:3])

  ## Tests of being able to create an empty STM and then use rebind()
  x <- array(1:(6*5*4), dim = c(6, 5, 4))
  y <- array(101:(100 + 6*5*4), dim = c(6, 5, 4))
  # Default-constructed (empty) map, then rebind() to the raw-pointer path
  # used by make_fieldSTM/rebind_fieldSTM; same slice/index as STM1.
  expect_equal(STM10(x), x[, 2:3, ][2, 2, 3])
  # rebind() called a second time re-seats the same map object to a
  # different tensor's storage, rather than copying values into the old one.
  res <- STM11(x, y)
  expect_equal(res$ansX, x[, 2:2,,drop=FALSE ])
  expect_equal(res$ansY, y[, 2:2,,drop=FALSE ])
  # rebind() with a singleton (rank-reducing) selection, same result as STM6.
  expect_equal(STM12(x), x[2:5, 3, 2:3])
})
