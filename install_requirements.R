#!/usr/bin/env Rscript

requirements <- c(
    'testthat',
    'Rcpp',
    'RcppEigen',
    'RcppParallel',
    'RcppEigenAD',
    'inline',
    'R6',
    'pkgKitten',  ## needed for test-distributions.R
    'Rcereal',
    'numDeriv',
    'roxygen2'
    )

for (package in requirements) {
    if (!suppressPackageStartupMessages(require(package,
                                                character.only = TRUE))) {
        install.packages(package, repos = 'http://cran.us.r-project.org')
    }
}
