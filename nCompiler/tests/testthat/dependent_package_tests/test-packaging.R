library(testthat)

## All of these test packages have `nCompiler` in `Imports` in `DESCRIPTION`.
## All of the test packages use `nRep` in R and C methods as example of
## using an nCompiler operator that must be found.

## `import(nCompiler)` in `NAMESPACE`.
test_that("dependent package using `imports` works correctly", {
    devtools::install_local("testImportPkg", force = TRUE)

    ## Test without importing package.
    
    ## Test execution within nClass.
    expect_identical(testImportPkg::fun_using_nClass(1), rep(2, 8))

    ## Test object generation within nClass.
    objs <- testImportPkg::fun_using_nClass(1, returnObj = TRUE)
    expect_identical(objs[[1]]$Rfoo(1), c(2,2))
    expect_identical(objs[[1]]$Cfoo(1), c(2,2))
    expect_identical(objs[[2]]$Rfoo(1), c(2,2))
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Test generator creation within nClass.
    gens <- testImportPkg::fun_using_nClass(1, returnGen = TRUE)
    objs[[1]] <- gens[[1]]$new()
    objs[[2]] <- gens[[2]]$new()
    expect_identical(objs[[1]]$Rfoo(1), c(2,2))
    expect_identical(objs[[1]]$Cfoo(1), c(2,2))
    expect_identical(objs[[2]]$Rfoo(1), c(2,2))
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Repeat tests after importing the package.
    
    library(testImportPkg)

    expect_identical(fun_using_nClass(1), rep(2, 8))

    ## Test object generation within nClass.
    objs <- fun_using_nClass(1, returnObj = TRUE)
    expect_identical(objs[[1]]$Rfoo(1), c(2,2))
    expect_identical(objs[[1]]$Cfoo(1), c(2,2))
    expect_identical(objs[[2]]$Rfoo(1), c(2,2))
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Test generator creation within nClass.
    gens <- fun_using_nClass(1, returnGen = TRUE)
    objs[[1]] <- gens[[1]]$new()
    objs[[2]] <- gens[[2]]$new()
    expect_identical(objs[[1]]$Rfoo(1), c(2,2))
    expect_identical(objs[[1]]$Cfoo(1), c(2,2))
    expect_identical(objs[[2]]$Rfoo(1), c(2,2))
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))

})

## `importFrom(nCompiler, nClass, nFunction, nCompile)` in `NAMESPACE`.
## `nRep` is not in `importFrom` in package to
## mimic situation like using `dinvgamma` in a model or `rep` in an nFunction.
test_that("dependent package using `importFrom` works correctly", {
    devtools::install_local("testImportFromPkg", force = TRUE)

    ## Test without importing package.
    
    ## Test execution within nClass.
    expect_error(testImportFromPkg::fun_using_nClass(1), "nRep")

    ## Test object generation within nClass.
    objs <- testImportFromPkg::fun_using_nClass(1, returnObj = TRUE)
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Test generator creation within nClass.
    gens <- testImportFromPkg::fun_using_nClass(1, returnGen = TRUE)
    objs[[1]] <- gens[[1]]$new()
    objs[[2]] <- gens[[2]]$new()
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Repeat tests after importing the package.
    
    library(testImportFromPkg)

    expect_error(fun_using_nClass(1), "nRep")

    ## Test object generation within nClass.
    objs <- fun_using_nClass(1, returnObj = TRUE)
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Test generator creation within nClass.
    gens <- fun_using_nClass(1, returnGen = TRUE)
    objs[[1]] <- gens[[1]]$new()
    objs[[2]] <- gens[[2]]$new()
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
})

## `nCompiler::foo` in package code. No import in `NAMESPACE`.
## `nRep` in package does not use `nCompile::nRep` to
## mimic situation like using `dinvgamma` in a model or `rep` in an nFunction.
test_that("dependent package using `nCompiler::foo` works correctly", {
    devtools::install_local("testNamespaceUsingPkg", force = TRUE)
    ## Test without importing package.
    
    ## Test execution within nClass.
    expect_error(testNamespaceUsingPkg::fun_using_nClass(1), "nRep")

    ## Test object generation within nClass.
    objs <- testNamespaceUsingPkg::fun_using_nClass(1, returnObj = TRUE)
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Test generator creation within nClass.
    gens <- testNamespaceUsingPkg::fun_using_nClass(1, returnGen = TRUE)
    objs[[1]] <- gens[[1]]$new()
    objs[[2]] <- gens[[2]]$new()
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Repeat tests after importing the package.
    
    library(testNamespaceUsingPkg)

    expect_error(fun_using_nClass(1), "nRep")

    ## Test object generation within nClass.
    objs <- fun_using_nClass(1, returnObj = TRUE)
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    
    ## Test generator creation within nClass.
    gens <- fun_using_nClass(1, returnGen = TRUE)
    objs[[1]] <- gens[[1]]$new()
    objs[[2]] <- gens[[2]]$new()
    expect_error(objs[[1]]$Rfoo(1), "nRep")
    expect_error(objs[[1]]$Cfoo(1), "nRep")
    expect_error(objs[[2]]$Rfoo(1), "nRep")
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))

})

test_that("Package function can use nClass defined in package namespace", {
    devtools::install_local("testImportPkg", force = TRUE)

    cnc <- testImportPkg::fun_using_nClass_in_pkg(5, returnGen = TRUE)
    Robj <- testImportPkg::nc$new()
    Cobj <- cnc$new()
    expect_identical(Robj$Cfoo(3), 4)
    expect_identical(Cobj$Cfoo(3), 4)

    objs <- testImportPkg::fun_using_nClass_in_pkg(5, returnObj = TRUE)
    expect_identical(objs[[1]]$Cfoo(3), 4)
    expect_identical(objs[[2]]$Cfoo(3), 4)

    expect_identical(testImportPkg::fun_using_nClass_in_pkg(5), c(6,6))
    
    library(testImportPkg)
    cnc <- fun_using_nClass_in_pkg(5, returnGen = TRUE)
    Robj <- nc$new()
    Cobj <- cnc$new()
    expect_identical(Robj$Cfoo(3), 4)
    expect_identical(Cobj$Cfoo(3), 4)

    objs <- fun_using_nClass_in_pkg(5, returnObj = TRUE)
    expect_identical(objs[[1]]$Cfoo(3), 4)
    expect_identical(objs[[2]]$Cfoo(3), 4)

    expect_identical(fun_using_nClass_in_pkg(5), c(6,6))
})

