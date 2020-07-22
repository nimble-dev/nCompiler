# NOT WORKING
context("repClass")

library(Rcpp)
test_that("basic uses of repClass work", {
  cppfile <- system.file(file.path('tests', 'testthat', 'cpp', 'repClass_tests.cpp'), package = 'nCompiler')
  nCompiler:::QuietSourceCpp(cppfile)
  expect_equivalent(repClass_test1(1:3), rep(1:3, 10))
  expect_equivalent(repClass_test2(matrix(1:10, 5), 3), rep(matrix(1:10, 5), 3))
  expect_equivalent(repClass_test3(1:3, 12), rep(1:3, length.out = 12))
  expect_equivalent(repClass_test3(1:7, 4), rep(1:7, length.out = 4))
  expect_equivalent(repClass_test4(array(1:12, c(2,3,2)), 7), rep(array(1:12, c(2,3,2)), length.out=7))
  expect_equivalent(repClass_test5(matrix(1:16, 8), 3), rep(matrix(1:16, 8), 3))
})
