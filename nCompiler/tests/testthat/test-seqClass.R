context("seqClass")
library(Rcpp)
test_that("basic uses of seqClass work",{
  cppfile <- system.file(file.path('tests', 'testthat', 'cpp', 'seqClass_tests.cpp'), package = 'nCompiler')
  QuietSourceCpp(cppfile)
  expect_equal(as.numeric(seqClass_test1(0, 10, 1)), 0:10)
  expect_equal(as.numeric(seqClass_test1(0, 10, 1.1)), seq(0, 10, 1.1))
})
