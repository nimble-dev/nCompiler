context("setWhich")

library(Rcpp)
test_that("basic uses of setWhich work", {
  cppfile <- system.file(file.path('tests', 'testthat', 'cpp',
                                   'setWhich_tests.cpp'), package = 'nCompiler')
  QuietSourceCpp(cppfile)
  expect_equivalent(setWhich_test1(TRUE), which(TRUE))
  expect_equivalent(setWhich_test1(FALSE), which(FALSE))
  expect_equivalent(setWhich_test2(c(FALSE, FALSE)), which(c(FALSE, FALSE)))
  x <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  expect_equivalent(setWhich_test2(x), which(x))
  expect_equivalent(setWhich_test2(!x), which(!x))
})
