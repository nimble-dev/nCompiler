#!/usr/bin/env Rscript

library(methods)
library(testthat)
library(nCompiler)

help_message <-
    "Eigen development testing routine for nCompiler"

argv <- commandArgs(trailingOnly = TRUE)

NCpath <- path.package("nCompiler")
testpath <- file.path(NCpath,'tests','testthat')

test_dir(
  path = file.path(testpath),
  filter = 'tensorOperations'
)