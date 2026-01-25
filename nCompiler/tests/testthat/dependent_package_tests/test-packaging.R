library(testthat)
library(remotes)

## All of these test packages have `nCompiler` in `Imports` in `DESCRIPTION`.


## `import(nCompiler)` in `NAMESPACE`.
test_that("dependent package using `imports` works correctly", {
    install_local("testImportPkg", force = TRUE)
    library(testImportPkg)
    expect_identical(myfun(4), 5)
})

## `importFrom(nCompiler, nClass, nFunction, nCompile)` in `NAMESPACE`.
test_that("dependent package using `importFrom` works correctly", {
    install_local("testImportFromPkg", force = TRUE)
    library(testImportFromPkg)
    expect_identical(myfun(4), 5)
})

## `nCompiler::foo` in package code. No import in `NAMESPACE`.
test_that("dependent package using `nCompiler::foo` works correctly", {
    install_local("testNamespaceUsingPkg", force = TRUE)
    library(testNamespaceUsingPkg)
    expect_identical(myfun(4), 5)
})

