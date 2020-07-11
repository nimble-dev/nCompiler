context("Test writePackage and buildPackage")

test_that("writePackage and buildPackage work for nFunction", 
          {
            nCompiler:::nFunctionIDMaker(reset = TRUE)
            nCompiler:::nClassIDMaker(reset = TRUE)
            
            foo <- nFunction(
              name = "foo",
              fun = function(x = numericScalar()) {
                ans <- x+1
                return(ans)
                returnType(numericScalar())
              }
            )
            
            writePackage(foo,
                         dir = tempdir(),
                         package.name = "fooPackage",
                         clean = TRUE, 
                         control = list(export = TRUE))
            ans <- buildPackage("fooPackage",
                                dir = tempdir())
            
            expect_equal(fooPackage::foo(2), 3)
          })

test_that("writePackage and buildPackage work for nClass", 
          {
            nCompiler:::nFunctionIDMaker(reset = TRUE)
            nCompiler:::nClassIDMaker(reset = TRUE)
            
            nc1 <- nClass(
              classname = "nc1",
              Rpublic = list(
                p1 = function(x) x+1
              ),
              Cpublic = list(
                x = 'numericScalar',
                cp1 = nFunction(
                  fun = function(x = 'numericScalar') {
                    ans <- x+1
                    return(ans)
                    returnType(numericScalar())
                  }
                )
              )
            )
            writePackage(
                nc1,
                dir = tempdir(),
                package.name = "nc1Package",
                clean = TRUE,
                nClass_full_interface = FALSE
            )
            buildPackage("nc1Package",
                         dir = tempdir())
                         #, lib = "test_package_nc1Package_lib")
            expect_true(require("nc1Package")) #, lib.loc = "test_package_nc1Package_lib"))
            expect_true(is.function(nc1Package:::new_nc1))
            obj <- nc1Package:::new_nc1()
            expect_equal(method(obj, 'cp1')(2), 3)
          })

test_that("writePackage and buildPackage work for nClass with full interface",
{
  nCompiler:::nFunctionIDMaker(reset = TRUE)
  nCompiler:::nClassIDMaker(reset = TRUE)

  nc2 <- nClass(
    classname = "nc2",
    Rpublic = list(
      p1 = function(x) x+1
    ),
    Cpublic = list(
      x = 'numericScalar',
      cp1 = nFunction(
        fun = function(x = numericScalar()) {
          ans <- x+1
          return(ans)
          returnType(numericScalar())
        }
      )
    )
  )
  writePackage(
      nc2,
      dir = tempdir(),
      package.name = "nc2Package",
      clean = TRUE
  )
  expect_true(buildPackage("nc2Package",
                           dir = tempdir()))
                           #, lib = "test_package_nc2Package_lib"))
  expect_true(isCompiledNCgenerator(nc2Package::nc2))
  obj <- nc2Package::nc2$new()
  expect_true(inherits(obj, "nClass"))
  expect_equal(obj$cp1(2), 3)
})


test_that("Create package with multiple objects", 
          {
            nCompiler:::nFunctionIDMaker(reset = TRUE)
            nCompiler:::nClassIDMaker(reset = TRUE)
            
            foo1 <- nFunction(
              name = "foo1",
              fun = function(x = numericScalar()) {
                ans <- x+1
                return(ans)
                returnType(numericScalar())
              }
            )
            
            foo2 <- nClass(
              classname = "foo2",
              Cpublic = list(
                cp1 = nFunction(
                  fun = function(x = numericScalar()) {
                    ans <- x+1
                    return(ans)
                    returnType(numericScalar())
                  }
                )
              )
            )
            
            writePackage(foo1, foo2,
                         dir = tempdir(),
                         package.name = "fooPackageMultiples",
                         clean = TRUE,
                         control = list(export = TRUE))
            ans <- buildPackage("fooPackageMultiples",
                                dir = tempdir())
            
            expect_equal(fooPackageMultiples::foo1(1), 2)
            my_foo2 <- fooPackageMultiples::foo2$new()
            expect_equal(my_foo2$cp1(10), 11)
          })


test_that("Export flag works", 
          {
            nCompiler:::nFunctionIDMaker(reset = TRUE)
            nCompiler:::nClassIDMaker(reset = TRUE)
            
            foo <- nFunction(
              name = "foo",
              fun = function(x = numericScalar()) {
                ans <- x+1
                return(ans)
                returnType(numericScalar())
              }
            )
            foo2 <- nFunction(
              name = "foo2",
              fun = function(x = numericScalar()) {
                ans <- x+2
                return(ans)
                returnType(numericScalar())
              }
            )
            
            writePackage(foo, foo2,
                         dir = tempdir(),
                         package.name = "fooPackageNoExport",
                         clean = TRUE,
                         control = list(
                           foo = list(export = FALSE),
                           foo2 = list(export = TRUE)))
            ans <- buildPackage("fooPackageNoExport",
                                dir = tempdir())
            
            expect_error(fooPackageNoExport::foo(2))
            expect_equal(fooPackageNoExport:::foo(2), 3)
            expect_equal(fooPackageNoExport::foo2(2), 4)
          })

test_that("Package writing errors and warnings for naming",
          {
            foo_duplicate1 <- nFunction(
              name = "foo",
              fun = function(x = numericScalar()) {
                ans <- x+1
                return(ans)
                returnType(numericScalar())
              }
            )
            foo_duplicate2 <- nFunction(
              name = "foo",
              fun = function(x = numericScalar()) {
                ans <- x+2
                return(ans)
                returnType(numericScalar())
              }
            )
            expect_error(
              writePackage(foo_duplicate1, foo_duplicate2,
                           dir = tempdir(),
                           package.name = "fooPackageDuplicates",
                           clean = TRUE)
            )
            
            foo_badname <- nFunction(
              name = "invalid.cpp.name",
              fun = function(x = numericScalar()) {
                ans <- x+2
                return(ans)
                returnType(numericScalar())
              }
            )
            expect_warning(
              writePackage(foo_badname,
                           dir = tempdir(),
                           package.name = "fooPackageNoExport",
                           clean = TRUE)
            )
            
          })

# Roxygen tests

test_that("Package writing documentation for nFunctions",
          {
            foo <- nFunction(
              name = "foo",
              fun = function(x = numericScalar()) {
                ans <- x+1
                return(ans)
                returnType(numericScalar())
              }
            )
            foo2 <- nFunction(
              name = "foo2",
              fun = function(x = numericScalar()) {
                ans <- x+2
                return(ans)
                returnType(numericScalar())
              }
            )
            rox <- documentNFunction(obj = foo, 
                    name  = "foo",
                    title = "A Test nFunction",
                    description = "This nFunction just adds 1 to a 
                                   scalar input.",
                    params = list(x = "A scalar to which 1 will be added."), 
                    otherRoxygen = "//' @export")
            writePackage(foo2, foo,
                         dir = tempdir(),
                         package.name = "fooPackageWriteDocnFunction",
                         clean = TRUE, 
                         roxygen = list(foo = rox),
                         roxygenize = TRUE)
            buildPackage("fooPackageWriteDocnFunction", 
                         dir = tempdir())
            expect_true(length(help("foo", package = "fooPackageWriteDocnFunction")) > 0)
            expect_error(help("foo2", package = "fooPackageWriteDocnFunction"))
          })

# TODO: nClass methods documentation
test_that("Package writing documentation for nClasses",
          {
            foo <- nClass(
              classname = "foo",
              Cpublic = list(
              x = "numericScalar",
              cp1 = nFunction(fun = function(x = numericScalar()) {
                x <<- x
                ans <- x + 1
                return(ans)
                returnType(numericScalar())
              }))
            )
            foo2 <- nClass(
              classname = "foo2",
              Cpublic = list(
                x = "numericScalar",
                cp2 = nFunction(fun = function(x = numericScalar()) {
                  x <<- x
                  ans <- x + 2
                  return(ans)
                  returnType(numericScalar())
                }))
            )
            rox <- documentNClass(obj = foo, 
                                 name  = "foo",
                                 title = "A Test nClass",
                                 description = "This nClass has a method to add 1 
                                    to a scalar input, and has a field to store
                                    that input.",
                                 fields = list(x = "A scalar input."),
                                 CMethodsDescriptions = list(cp1 = "Adds 1 to x."),
                                 CMethodsParams = 
                                   list(cp1 = list(x = "A scalar to which 
                                                        1 will be added.")),
                                 methodsComment = "#'")
            
            writePackage(foo2, foo,
                         dir = tempdir(),
                         package.name = "fooPackageWriteDocnClass",
                         clean = TRUE, 
                         roxygen = list(foo = rox),
                         roxygenize = TRUE
                         )
            buildPackage("fooPackageWriteDocnClass", 
                         dir = tempdir())
            expect_true(length(help("foo", package = "fooPackageWriteDocnClass")) > 0)
            expect_error(help("foo2", package = "fooPackageWriteDocnClass"))
          })
