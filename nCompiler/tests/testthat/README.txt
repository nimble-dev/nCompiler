To install with tests, use --install-tests flag:
R CMD install --install-tests package_version.tar.gz

To run tests from installed directory:
library(testthat)
test_package("libraryname","testthat")

To run tests from source code directory:
library(devtools)
devtools::test("libraryname","testthat")
