#!/usr/bin/env Rscript

requirements <- c(
    'testthat',
    'Rcpp',
    'RcppEigen',
    'RcppEigenAD',
    'inline',
    'R6',
    'pkgKitten',  ## needed for test-distributions.R
    'Rcereal'
    )

for (package in requirements) {
    if (!suppressPackageStartupMessages(require(package,
                                                character.only = TRUE))) {
        install.packages(package, repos = 'https://cran.cnr.berkeley.edu/') ##'http://cran.us.r-project.org'
    }
}
