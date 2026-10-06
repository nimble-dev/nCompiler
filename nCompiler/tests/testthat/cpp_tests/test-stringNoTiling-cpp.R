## Regression tests for tensorStringNoTiling.h: Eigen must not use tiled
## evaluation for std::string tensors. That bug segfaults on Linux but usually
## passes by luck on macOS, so these tests detect the cause directly rather than
## relying on a crash. See notes in cpp/stringNoTiling_tests.cpp.
library(Rcpp)
test_that("Eigen does not use tiled evaluation for std::string tensors", {
  cppfile <- system.file(
    file.path('tests', 'testthat', 'cpp', 'stringNoTiling_tests.cpp'),
    package = 'nCompiler')
  test <- nCompiler:::QuietSourceCpp(cppfile)

  tileable <- stringNoTiling_isTileable()
  expect_equal(tileable[["string_chip_slice"]], 0L)
  expect_equal(tileable[["string_chip_reshape"]], 0L)
  ## If these change, the string cases above may no longer exercise the
  ## tiled path, and the expressions here should be revisited.
  expect_equal(tileable[["double_chip_slice"]], 1L)
  expect_equal(tileable[["double_chip_reshape"]], 1L)

  ## Strings longer than any short-string buffer, so they are heap-allocated.
  long <- function(x) paste0(x, strrep("_", 40))

  z <- matrix(long(LETTERS[1:6]), nrow = 2)
  res <- stringNoTiling_assign2D(z)
  expect_true(res$noScratch)
  ans <- matrix("", nrow = 2, ncol = 3)
  ans[1, 1:3] <- z[2, 1:3]
  expect_equal(res$y, ans)

  z <- array(long(LETTERS[1:24]), dim = c(2, 3, 4))
  res <- stringNoTiling_assign3D(z)
  expect_true(res$noScratch)
  ans <- array("", dim = c(2, 3, 4))
  ans[1, 2:3, 3:4] <- z[2, 1:2, 2:3]
  expect_equal(res$y, ans)
})
