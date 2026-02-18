## This presumably overlaps with testing in nCompile_tests/test-nCompile.R, but
## I am adding it, at least temporarily, as part of testing various ways
## that a user package might use nCompiler.
test_that("basic testing of package produced by `writePackage`", {

    system2("Rscript", "setup-writePackage.R")
    
    remotes::install_local(file.path("/tmp", "writePackageTestPkg"), force = TRUE, upgrade = "never")
    
    library(writePackageTestPkg)
    
    ## Users can use the compiled functions and classes.
    
    expect_identical(outerFun(3), c(4,4))
    obj <- nc$new()
    obj$Cv <- 3
    expect_identical(obj$Cfoo(5), c(9,9))

    ## We don't expect that the package user could then build nFunctions using the
    ## writePackageTestPkg nFunctions since the nFunction definition is not part of the package.
    newfun <- nCompiler::nFunction(
        fun = function(x) {
            ans <- outerFun(3)
            return(ans)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericVector'
    )
    expect_identical(newfun(3), c(4,4))
    expect_failure(cnewfun <- nCompiler::nCompile(newfun), "No operator definition found for outerFun")

}
