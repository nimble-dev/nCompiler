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
})
