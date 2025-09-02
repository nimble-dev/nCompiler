# nCompiler's extensions to as<> and wrap<> templates.
# (A warning about lack of Rcpp::export attributes or RCPP_MODULE declarations is expected.")
library(Rcpp)
message("basic as<> and wrap<> tests work but are not thorough and may be deprecated by argument passing tests.")
test_that("basic use of as<> and wrap<> work. (Warning about no Rcpp::Export or RCPP_MODULE declarations is expected.)",{
  cppfile <- system.file(file.path('tests', 'testthat', 'cpp', 'as_wrap_tests.cpp'), package = 'nCompiler')
  test <- expect_warning(nCompiler:::QuietSourceCpp(cppfile))
  sofile <- {
    files <- list.files(test$buildDirectory)
    files[grepl("sourceCpp", files)][1]
  }
  dyn.load(file.path(test$buildDirectory, sofile))
  x <- array(rnorm(8), dim = c(2,2,2))
  xcopy <- .Call("tensor2D_by_copy", x)
  expect_identical(x, xcopy)
})
