library(testthat)

## All of these test packages have `nCompiler` in `Imports` in `DESCRIPTION`.
## All of the test packages use `nRep` in R and C methods as example of
## using an nCompiler operator that must be found.

remotes::install_local("testImportPkg", force = TRUE)  
remotes::install_local("testImportFromPkg", force = TRUE)
remotes::install_local("testNamespaceUsingPkg", force = TRUE)

## `import(nCompiler)` in `NAMESPACE`.
test_that("dependent package using `imports` works correctly without importing package", {
    ## Test direct use of nClass.
    Robj <- testImportPkg::nc$new()
    expect_identical(Robj$Rfoo(3), c(4,4))
    Robj$Rv <- 5
    expect_identical(Robj$Rfoo2(3), 8)
    expect_identical(Robj$Rfoo3(3), 12)
    expect_identical(Robj$Cfoo(3), c(4,4))
    Robj$Cv <- 5
    expect_identical(Robj$Cfoo2(3), 8)
    expect_identical(Robj$Cfoo3(3), 12)
    
    expect_error(cnc <- nCompiler::nCompile(testImportPkg::nc), "object 'new_testImportPkg' not found")

    ## Test direct use of (nested) nFunction.
    expect_identical(testImportPkg::outerFun(4), 6)
    expect_error(cOuterFun <- nCompiler::nCompile(testImportPkg::outerFun), "object 'testImportPkg' not found")
    # cOuterFun(4)
    
    ## Test use of package nFunction in user nFunction.
    testfun <- nCompiler::nFunction(
        fun = function(x) {
            return(x+testImportPkg::outerFun(x))
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar'
    )
    expect_identical(testfun(3), 8)
    ## We don't have namespace resolution in nCompiler (yet).
    ## This prints out an error message while testing; would be nice to suppress that.
    expect_error(ctestfun <- nCompiler::nCompile(testfun), "trying to make a function call from something that is not an nFunction")

    ## Test execution of functions from the package.
    
    ## Test execution within nClass.
    expect_identical(testImportPkg::fun_using_package_nClass(1), c(rep(2, 8), rep(3, 4), rep(6,4)))

    ## Test object generation within nClass.
    objs <- testImportPkg::fun_using_package_nClass(1, returnObj = TRUE)
    expect_identical(objs[[1]]$Rfoo(1), c(2,2))
    expect_identical(objs[[1]]$Cfoo(1), c(2,2))
    expect_identical(objs[[2]]$Rfoo(1), c(2,2))
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    objs[[1]]$Rv <- objs[[1]]$Cv <- objs[[2]]$Rv <- objs[[2]]$Cv <- 2
    expect_identical(objs[[1]]$Rfoo2(1), 3)
    expect_identical(objs[[1]]$Cfoo2(1), 3)
    expect_identical(objs[[2]]$Rfoo2(1), 3)
    expect_identical(objs[[2]]$Cfoo2(1), 3)

    expect_identical(objs[[1]]$Rfoo3(1), 6)
    expect_identical(objs[[1]]$Cfoo3(1), 6)
    expect_identical(objs[[2]]$Rfoo3(1), 6)
    expect_identical(objs[[2]]$Cfoo3(1), 6)
    
    ## Test generator creation within nClass.
    cnc <- testImportPkg::fun_using_package_nClass(1, returnGen = TRUE)
    Cobj <- cnc$new()
    
    expect_identical(Cobj$Rfoo(1), c(2,2))
    expect_identical(Cobj$Cfoo(1), c(2,2))
    Cobj$Rv <- Cobj$Cv <- 2
    expect_identical(Cobj$Rfoo2(1), 3)
    expect_identical(Cobj$Cfoo2(1), 3)

    expect_identical(Cobj$Rfoo3(1), 6)
    expect_identical(Cobj$Cfoo3(1), 6)

    ## Test execution and nFunction generator. 
    expect_identical(testImportPkg::fun_using_package_nFun(3), c(5,5))
    cOuterFun <- testImportPkg::fun_using_package_nFun(3, returnFun = TRUE)
    expect_identical(cOuterFun(3), 5)
})

## testImportPkg runs `registerOpDef` in `.onLoad`.

test_that("user-defined operator works in dependent package without importing package", {
    expect_identical(testImportPkg::testfun(), c(6,6))
    expect_error(ctestfun <- nCompiler::nCompile(testImportPkg::testfun), "object 'testImportPkg' not found")
    ## ctestfun()

    expect_identical(testImportPkg::fun_using_testfun_with_op(), c(6,6))
    ctestfun <- testImportPkg::fun_using_testfun_with_op(returnFun = TRUE)
    expect_identical(ctestfun(), c(6,6))

    Robj <- testImportPkg::nc_userOp$new()
    expect_error(Cnc <- nCompiler::nCompile(testImportPkg::nc_userOp), "object 'new_testImportPkg' not found")
    ## Cobj <- Cnc$new()

    expect_identical(Robj$foo(), c(6,6))
    ## Cobj$foo()

    expect_identical(testImportPkg::fun_using_class_with_op(), rep(6,4))
    objs <- testImportPkg::fun_using_class_with_op(returnObj = TRUE)
    expect_identical(objs[[1]]$foo(), c(6,6))
    expect_identical(objs[[2]]$foo(), c(6,6))

    Cnc <- testImportPkg::fun_using_class_with_op(returnGen = TRUE)
    Cobj <- Cnc$new()
    expect_identical(Cobj$foo(), c(6,6))

    userFun <- nCompiler::nFunction(
        fun = function(x) {
            ans <- testImportPkg::nimArray(6, dim = 2)
            return(ans)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericVector'
    )

    expect_identical(userFun(3), c(6,6))
    expect_error(cuserFun <- nCompiler::nCompile(userFun), "trying to make a function call from something that is not an nFunction")
    ## cuserFun(3)
})
 
    
test_that("dependent package using `imports` works correctly with importing package", {
    library(testImportPkg)

    ## Test direct use of nClass.
    Robj <- nc$new()
    expect_identical(Robj$Rfoo(3), c(4,4))
    Robj$Rv <- 5
    expect_identical(Robj$Rfoo2(3), 8)
    expect_identical(Robj$Rfoo3(3), 12)
    expect_identical(Robj$Cfoo(3), c(4,4))
    Robj$Cv <- 5
    expect_identical(Robj$Cfoo2(3), 8)
    expect_identical(Robj$Cfoo3(3), 12)
    
    cnc <- nCompiler::nCompile(nc)
    Cobj <- cnc$new()
    expect_identical(Cobj$Rfoo(3), c(4,4))
    Cobj$Rv <- 5
    expect_identical(Cobj$Rfoo2(3), 8)
    expect_identical(Cobj$Rfoo3(3), 12)
    expect_identical(Cobj$Cfoo(3), c(4,4))
    Cobj$Cv <- 5
    expect_identical(Cobj$Cfoo2(3), 8)
    expect_identical(Cobj$Cfoo3(3), 12)

    ## Test direct use of (nested) nFunction.
    expect_identical(outerFun(4), 6)
    cOuterFun <- nCompiler::nCompile(outerFun)
    expect_identical(cOuterFun(4), 6)
    
    ## Test use of package nFunction in user nFunction.
    testfun <- nCompiler::nFunction(
        fun = function(x) {
            return(x+outerFun(x))
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar'
    )
    expect_identical(testfun(3), 8)
    ctestfun <- nCompiler::nCompile(testfun)
    expect_identical(ctestfun(3), 8)

    ## Could also add test use of package nClass in user nFunction or nClass.

    ## Test execution within nClass.
    expect_identical(fun_using_package_nClass(1), c(rep(2, 8), rep(3, 4), rep(6,4)))

    ## Test object generation within nClass.
    objs <- fun_using_package_nClass(1, returnObj = TRUE)
    expect_identical(objs[[1]]$Rfoo(1), c(2,2))
    expect_identical(objs[[1]]$Cfoo(1), c(2,2))
    expect_identical(objs[[2]]$Rfoo(1), c(2,2))
    expect_identical(objs[[2]]$Cfoo(1), c(2,2))
    objs[[1]]$Rv <- objs[[1]]$Cv <- objs[[2]]$Rv <- objs[[2]]$Cv <- 2
    expect_identical(objs[[1]]$Rfoo2(1), 3)
    expect_identical(objs[[1]]$Cfoo2(1), 3)
    expect_identical(objs[[2]]$Rfoo2(1), 3)
    expect_identical(objs[[2]]$Cfoo2(1), 3)

    expect_identical(objs[[1]]$Rfoo3(1), 6)
    expect_identical(objs[[1]]$Cfoo3(1), 6)
    expect_identical(objs[[2]]$Rfoo3(1), 6)
    expect_identical(objs[[2]]$Cfoo3(1), 6)
    
    ## Test generator creation within nClass.
    cnc <- fun_using_package_nClass(1, returnGen = TRUE)
    Cobj <- cnc$new()
    
    expect_identical(Cobj$Rfoo(1), c(2,2))
    expect_identical(Cobj$Cfoo(1), c(2,2))
    Cobj$Rv <- Cobj$Cv <- 2
    expect_identical(Cobj$Rfoo2(1), 3)
    expect_identical(Cobj$Cfoo2(1), 3)

    expect_identical(Cobj$Rfoo3(1), 6)
    expect_identical(Cobj$Cfoo3(1), 6)

    ## Test execution and nFunction generator. 
    expect_identical(fun_using_package_nFun(3), c(5,5))
    cOuterFun <- fun_using_package_nFun(3, returnFun = TRUE)
    expect_identical(cOuterFun(3), 5)

})

test_that("user-defined operator works in dependent package with importing package", {
    library(testImportPkg)
    expect_identical(testfun(), c(6,6))
    ctestfun <- nCompiler::nCompile(testfun)
    expect_identical(ctestfun(), c(6,6))

    expect_identical(fun_using_testfun_with_op(), c(6,6))
    ctestfun <- fun_using_testfun_with_op(returnFun = TRUE)
    expect_identical(ctestfun(), c(6,6))

    Robj <- nc_userOp$new()
    Cnc <- nCompiler::nCompile(nc_userOp)
    Cobj <- Cnc$new()

    expect_identical(Robj$foo(), c(6,6))
    expect_identical(Cobj$foo(), c(6,6))

    expect_identical(fun_using_class_with_op(), rep(6,4))
    objs <- fun_using_class_with_op(returnObj = TRUE)
    expect_identical(objs[[1]]$foo(), c(6,6))
    expect_identical(objs[[2]]$foo(), c(6,6))

    Cnc <- fun_using_class_with_op(returnGen = TRUE)
    Cobj <- Cnc$new()
    expect_identical(Cobj$foo(), c(6,6))

    userFun <- nCompiler::nFunction(
        fun = function(x) {
            ans <- nimArray(6, dim = 2)
            return(ans)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericVector'
    )

    expect_identical(userFun(3), c(6,6))
    cuserFun <- nCompiler::nCompile(userFun)
    expect_identical(cuserFun(3), c(6,6))
})


## TODO: need to update these tests when we update testImportFromPkg and
## testNamespaceUsingPkg packages to reflect new structure of testImportPkg.

## `importFrom(nCompiler, nClass, nFunction, nCompile)` in `NAMESPACE`.
## `nRep` is not in `importFrom` in package to
## mimic situation like using `dinvgamma` in a model or `rep` in an nFunction.
test_that("dependent package using `importFrom` works correctly", {
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

