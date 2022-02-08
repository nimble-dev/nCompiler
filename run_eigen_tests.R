#!/usr/bin/env Rscript

library(methods)
library(testthat)
library(nCompiler)

help_message <-
    "Eigen development testing routine for nCompiler"

argv <- commandArgs(trailingOnly = TRUE)


# path to test files if nCompiler is installed with "--install-tests" flag,
# i.e., via R CMD INSTALL . --install-tests
NCpath <- path.package("nCompiler")
testpath <- file.path(NCpath,'tests','testthat')

if(length(dir(testpath)) == 0) {
  # asssume path to test files in local clone of github repo
  testpath <- file.path(getwd(), 'nCompiler', 'tests', 'testthat')
}

test_dir(path = testpath, filter = 'tensorOperations_reshaping')
test_dir(path = testpath, filter = 'tensorOperations_sparse')
test_dir(path = testpath, filter = 'tensorOperations_sparse_multiplication')
test_dir(path = testpath, filter = 'tensorOperations_linear_algebra')

