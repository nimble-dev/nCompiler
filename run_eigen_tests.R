#!/usr/bin/env Rscript

help_message <-
    "Eigen development testing routine for nCompiler"

argv <- commandArgs(trailingOnly = TRUE)

require(nCompiler)
NCpath <- path.package("nCompiler")
testpath <- file.path(NCpath,'tests','testthat')
