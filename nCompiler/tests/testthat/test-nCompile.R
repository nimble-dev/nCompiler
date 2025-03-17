message("add tests for multiple units via packaging")
message("add tests for 'none' interface")
message("add test for control$changeKeywords")

# Notes:
# Three pathways:
# A. nCompile(package =FALSE)
# B. nCompile(package = TRUE)
# C. writePackage
#
# Pathway A will be faster but does not allow work to be saved.
# Pathway B allows work to be saved with all package details managed internally.
# Pathway C allows development of one's own package, requiring some attention to package details.
#
# For B, the returned functions or list of functions should match those of A but be obtained via `::`.
#  This will allow repeated re-compiling without needing to unload DLLs.
#  It will also require some tracking of assigned names when saving. That could get messy.
# For C, one needs control of the exported R names, perhaps to be called compiledName and defaulting to name.
# Return object of nCompile(package = TRUE) needs attention.

test_that("nCompile direct, package, and writePackage work with Eigen::Tensors", {
  add_vectors <- nFunction(
    fun = function(x = double(1),
                   y = double(1)) {
      returnType(double(1))
      ans <- x + y
      return(ans)
    }
  )
  x1 <- 1:3
  x2 <- 11:13
  expect_equal(x1 + x2, add_vectors(x1, x2))
  c1 <- nCompile(add_vectors)
  expect_equal(x1 + x2, c1(x1, x2))
  c2 <- nCompile(add_vectors, package = TRUE)
  expect_equal(x1 + x2, c2(x1, x2))
  dir <- file.path(tempdir(), "test_nComp_testpackage")
  test <- writePackage(add_vectors, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  expect_equal(x1+x2, testpackage::add_vectors(x1, x2))
  pkgload::unload("testpackage")
})


test_that("nCompile direct, package, and writePackage work with nClass interfaces", {
  nc <- nClass(
    Cpublic = list(
      add_vectors = nFunction(
        fun = function(x = double(1),
                       y = double(1)) {
          returnType(double(1))
          ans <- x + y
          return(ans)
        }
      )
    )
  )
  x1 <- 1:3
  x2 <- 11:13
  nc1 <- nc$new()
  expect_equal(x1 + x2, nc1$add_vectors(x1, x2))
  c1 <- nCompile(nc)
  obj <- c1$new()
  expect_equal(x1 + x2, obj$add_vectors(x1, x2))
  CppObj <- obj$private$CppObj
  expect_equal(x1 + x2, method(CppObj, 'add_vectors')(x1, x2))
  rm(CppObj, obj); gc();

  c2 <- nCompile(nc, package = TRUE)
  obj <- c2$new()
  expect_equal(x1 + x2, obj$add_vectors(x1, x2))
  CppObj <- obj$private$CppObj
  expect_equal(x1 + x2, method(CppObj, 'add_vectors')(x1, x2))
  rm(CppObj, obj); gc();

  c3 <- nCompile(nc, package = TRUE, interfaces = "generic")
  CppObj <- c3()
  expect_equal(x1 + x2, method(CppObj, 'add_vectors')(x1, x2))
  obj <- to_full_interface(CppObj)
  expect_equal(x1 + x2, obj$add_vectors(x1, x2))
  rm(CppObj, obj); gc();

  dir <- file.path(tempdir(), "test_nComp_testpackage")
  test <- writePackage(nc, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::nc$new()
  expect_equal(x1 + x2, obj$add_vectors(x1, x2))
  CppObj <- obj$private$CppObj
  expect_equal(x1 + x2, method(CppObj, 'add_vectors')(x1, x2))
  rm(CppObj, obj); gc(); pkgload::unload("testpackage")

  dir <- file.path(tempdir(), "test_nComp_testpackage")
  test <- writePackage(nc, pkgName = "testpackage", interfaces = "generic", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  CppObj <- testpackage::nc()
  expect_equal(x1 + x2, method(CppObj, 'add_vectors')(x1, x2))
  obj <- to_full_interface(CppObj)
  expect_equal(x1 + x2, obj$add_vectors(x1, x2))
  rm(CppObj, obj); gc(); pkgload::unload("testpackage")
})

test_that("nCompile direct, package, and writePackage work with various name management", {
  add.Scalars_name <- nFunction(
    name = 'Cadd.scalars',
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )

  add.Scalars_name_eName <- nFunction(
    name = 'Cadd.scalars',
    compileInfo = list(exportName = "foo2"),
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )

  add.Scalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )

  add.Scalars_eName <- nFunction(
    compileInfo = list(exportName = "foo1"),
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )

  test <- nCompile(add.Scalars, package = FALSE, returnList = TRUE)
  expect_equal(test$add.Scalars(2, 3), 5)
  test <- nCompile(add.Scalars, package = TRUE, returnList = TRUE)
  expect_equal(test$add.Scalars(2, 3), 5)
  dir <- file.path(tempdir(), "test_nComp_testpackage")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(add.Scalars, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  expect_equal(testpackage::add.Scalars(2, 3), 5)
  pkgload::unload("testpackage")

  test <- nCompile(add.Scalars_name, package = FALSE, returnList = TRUE)
  expect_equal(test$add.Scalars_name(2, 3), 5)
  test <- nCompile(add.Scalars_name, package = TRUE, returnList = TRUE)
  expect_equal(test$add.Scalars_name(2, 3), 5)
  dir <- file.path(tempdir(), "test_nComp_testpackage")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(add.Scalars_name, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  expect_equal(testpackage::add.Scalars_name(2, 3), 5)
  pkgload::unload("testpackage")

  test <- nCompile(add.Scalars_eName, package = FALSE, returnList = TRUE)
  expect_equal(test$foo1(2, 3), 5)
  test <- nCompile(add.Scalars_eName, package = TRUE, returnList = TRUE)
  expect_equal(test$foo1(2, 3), 5)
  dir <- file.path(tempdir(), "test_nComp_testpackage")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(add.Scalars_eName, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  expect_equal(testpackage::foo1(2, 3), 5)
  pkgload::unload("testpackage")

  test <- nCompile(add.Scalars_name_eName, package = FALSE, returnList = TRUE)
  expect_equal(test$foo2(2, 3), 5)
  test <- nCompile(add.Scalars_name_eName, package = TRUE, returnList = TRUE)
  expect_equal(test$foo2(2, 3), 5)
  dir <- file.path(tempdir(), "test_nComp_testpackage")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(add.Scalars_name_eName, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  expect_equal(testpackage::foo2(2, 3), 5)
  pkgload::unload("testpackage")
})

test_that("nCompile works for nClass with classname and/or exportName and either interface", {
  nc_name <- nClass(
    classname = "nc.1",
    Cpublic = list(
      v.1 = 'numericVector',
      go.1 = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v.1)
        },
        returnType = 'numericVector'
      )
    )
  )

  nc <- nClass(
    Cpublic = list(
      v.1 = 'numericVector',
      go.1 = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v.1)
        },
        returnType = 'numericVector'
      )
    )
  )

  nc_name_eName <- nClass(
    classname = "nc.1",
    compileInfo = list(exportName = "exnc2"),
    Cpublic = list(
      v.1 = 'numericVector',
      go.1 = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v.1)
        },
        returnType = 'numericVector'
      )
    )
  )

  nc_eName <- nClass(
    compileInfo = list(exportName = "exnc1"),
    Cpublic = list(
      v.1 = 'numericVector',
      go.1 = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v.1)
        },
        returnType = 'numericVector'
      )
    )
  )

  test_obj <- function(obj) {
    if(nCompiler:::is.loadedObjectEnv(obj)) {
      value(obj, "v.1") <- 1:3
      expect_equal(value(obj, "v.1"), 1:3)
      expect_identical(method(obj, "go.1")(10), 10 * (1:3))
    } else {
      obj$v.1 <- 1:3
      expect_equal(obj$v.1, 1:3)
      expect_identical(obj$go.1(10), 10 * (1:3))
    }
  }

###### plain version of nc
  ## generic & direct
  test <- nCompile(nc, package=FALSE, interfaces = "generic", returnList = TRUE)
  obj <- test$nc(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()
  ## full & direct
  test <- nCompile(nc, package=FALSE, interfaces = "full", returnList = TRUE)
  obj <- test$nc$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & package
  test <- nCompile(nc, package=TRUE, interfaces = "generic", returnList = TRUE)
  obj <- test$nc(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()
  ## full & package
  test <- nCompile(nc, package=TRUE, interfaces = "full", returnList = TRUE)
  obj <- test$nc$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc, interfaces = "generic", pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::nc(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc(); pkgload::unload("testpackage")

  ## full & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::nc$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

###### name  version of nc
  ## generic & direct
  test <- nCompile(nc_name, package=FALSE, interfaces = "generic", returnList = TRUE)
  obj <- test$nc_name(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()

  ## full & direct
  test <- nCompile(nc_name, package=FALSE, interfaces = "full", returnList = TRUE)
  obj <- test$nc_name$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & package
  test <- nCompile(nc_name, package=TRUE, interfaces = "generic", returnList = TRUE)
  obj <- test$nc_name(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()

  ## full & package
  test <- nCompile(nc_name, package=TRUE, interfaces = "full", returnList = TRUE)
  obj <- test$nc_name$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc_name, interfaces = "generic", pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::nc_name(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc(); pkgload::unload("testpackage")

  ## full & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc_name, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::nc_name$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc(); pkgload::unload("testpackage")


###### eName version of nc
  ## generic & direct
  test <- nCompile(nc_eName, package=FALSE, interfaces = "generic", returnList = TRUE) ## we got a full interface!
  obj <- test$exnc1(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()

  ## full & direct
  test <- nCompile(nc_eName, package=FALSE, interfaces = "full", returnList = TRUE)
  obj <- test$exnc1$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & package
  test <- nCompile(nc_eName, package=TRUE, interfaces = "generic", returnList = TRUE)
  obj <- test$exnc1(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()

  ## full & package
  test <- nCompile(nc_eName, package=TRUE, interfaces = "full", returnList = TRUE)
  obj <- test$exnc1$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc_eName, interfaces = "generic", pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::exnc1(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc(); pkgload::unload("testpackage")

  ## full & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc_eName, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::exnc1$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc(); pkgload::unload("testpackage")

###### name and eName version of nc
  ## generic & direct
  test <- nCompile(nc_name_eName, package=FALSE, interfaces = "generic", returnList = TRUE) ## we got a full interface!
  obj <- test$exnc2(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()

  ## full & direct
  test <- nCompile(nc_name_eName, package=FALSE, interfaces = "full", returnList = TRUE)
  obj <- test$exnc2$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & package
  test <- nCompile(nc_name_eName, package=TRUE, interfaces = "generic", returnList = TRUE)
  obj <- test$exnc2(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc()

  ## full & package
  test <- nCompile(nc_name_eName, package=TRUE, interfaces = "full", returnList = TRUE)
  obj <- test$exnc2$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc()

  ## generic & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc_name_eName, interfaces = "generic", pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::exnc2(); test_obj(obj)
  objf <- to_full_interface(obj); test_obj(objf)
  rm(obj, objf); gc(); pkgload::unload("testpackage")

  ## full & writePackage
  dir <- file.path(tempdir(), "test_nComp_testpackage2")
  dir.create(dir, showWarnings=FALSE)
  test <- writePackage(nc_name_eName, pkgName = "testpackage", dir = dir, modify="clear")
  lib <- file.path(tempdir(), "test_nComp_lib2")
  dir.create(lib, showWarnings=FALSE)
  withr::with_libpaths(lib, devtools::install(file.path(dir, "testpackage"),
                                              upgrade = "never", quick=TRUE, quiet=TRUE))
  withr::with_libpaths(lib, loadNamespace("testpackage"))
  obj <- testpackage::exnc2$new(); test_obj(obj)
  objC <- obj$private$CppObj; test_obj(objC)
  rm(obj, objC); gc(); pkgload::unload("testpackage")
})

test_that("Compile one nFunction via nCompile, returning a list (and testing external R name invalid for C++).",
{ 
  add.Scalars <- nFunction(
    name = 'Cadd.scalars',
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  test <- nCompile(add.Scalars, package = FALSE, returnList = TRUE)
  #test <- nCompile2(add.Scalars, returnList = TRUE)
  expect_equal(test$add.Scalars(2, 3), 5)
  test <- nCompile(add.Scalars, package = TRUE, returnList = TRUE)
  expect_equal(test$add.Scalars(2, 3), 5)
}
)

test_that("Compile one nFunction via nCompile, not returning a list (and testing internal name invalid for C++).",
{ 
  addScalars <- nFunction(
    name = "add.Scalars",
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  test <- nCompile(addScalars, returnList = FALSE)
  expect_equal(test(2, 3), 5)
})

test_that("Compile two nFunctions via nCompile, returning a list.",
{ 
  addScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  multScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x * y
      return(ans)
    }
  )
  test <- nCompile(addScalars, multScalars)
  expect_equal(test$addScalars(2, 3), 5)
  expect_equal(test$multScalars(2, 3), 6)
}
)

test_that("Compile two nFunctions via nCompile provided as a list, returning a list.",
{ 
  addScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x + y
      return(ans)
    }
  )
  multScalars <- nFunction(
    fun = function(x = double(0),
                   y = double(0)) {
      returnType(double(0))
      ans <- x * y
      return(ans)
    }
  )
  test <- nCompile(list(f1 = addScalars, f2 = multScalars))
  expect_equal(test$f1(2, 3), 5)
  expect_equal(test$f2(2, 3), 6)
})

test_that("Compile one nClass via nCompile provided as a list, returning not as list (and checking R to C++ conversions of class, method, and member names).",
{
  # Some other variants are in the "...various ways..." test below
  nc <- nClass(
    classname = "nc.1",
    Cpublic = list(
      v.1 = 'numericVector',
      go.1 = nFunction(
        fun = function(c = 'numericScalar') {
          return(c * v.1)
        },
        returnType = 'numericVector'
      )
    )
  )
  Cnc <- nCompile(list(nc = nc))
#  Cnc <- nCompile2(list(nc = nc))
  Cnc1 <- Cnc$new()
  Cnc1$v.1 <- 1:3
  expect_equal(Cnc1$go.1(2), 2*(1:3))
})

test_that("nCompile naming and interface choices work in various ways",
{
  nc1 <- nClass(
    classname = "nc1_",
    Cpublic = list(
      x = 'numericScalar'
    )
  )
  
  nc2 <- nClass(
    Cpublic = list(
      y = 'numericScalar'
    )
  )
  
  nc3 <- nClass(
    Cpublic = list(
      z = 'numericScalar'
    )
  )

  # Basic use
  #nOptions(showCompilerOutput = TRUE)
  comp <- nCompile(nc1, nc2)
  expect_identical(names(comp), c("nc1", "nc2"))
  expect_true(inherits(comp$nc1$new(), "nClass"))
  expect_true(inherits(comp$nc2$new(), "nClass"))
 
  # One named element in the ..., and generic interface for ALL
  comp <- nCompile(nc1x = nc1, nc2,
                   interfaces = "generic")
  expect_identical(names(comp), c("nc1x", "nc2"))
  expect_true(class(comp$nc1())=="loadedObjectEnv")
  expect_true(class(comp$nc2())=="loadedObjectEnv")

  # One named element in the ..., and different interface choices
  comp <- nCompile(nc1x = nc1, nc2,
                   interfaces = c(nc1x = "full", nc2 = "generic"))
  expect_identical(names(comp), c("nc1x", "nc2"))
  expect_true(inherits(comp$nc1x$new(), "CnClass"))
  expect_true(class(comp$nc2())=="loadedObjectEnv")

  # Call with singleton does not return a list
  comp <- nCompile(nc1)
  expect_true(inherits(comp$new(), "CnClass"))

  # Option to return a list with a singleton
  comp <- nCompile(nc1, returnList = TRUE)
  expect_true(inherits(comp$nc1$new(), "CnClass"))

  # Provide compilation units as a named list
  comp <- nCompile(list(nc1 = nc1, nc2 = nc2), interfaces = "generic")
  expect_identical(names(comp), c("nc1", "nc2"))
  expect_true(class(comp$nc1())=="loadedObjectEnv")
  expect_true(class(comp$nc2())=="loadedObjectEnv")

  # Error if a list is not completely named
  expect_error(comp <- nCompile(list(nc1 = nc1, nc2))) ## expect error due to only partial naming in list

  # Mix of named list and individual unit, both in ...
  comp <- nCompile(list(nc1 = nc1, nc3 = nc3), nc2, interfaces = "generic")
  expect_identical(names(comp), c("nc1", "nc3", "nc2"))
  expect_true(class(comp$nc1())=="loadedObjectEnv")
  expect_true(class(comp$nc2())=="loadedObjectEnv")
  expect_true(class(comp$nc3())=="loadedObjectEnv")
  
  # Move on to nFunctions  
  nfA <- nFunction(
    name = "nfA_",
    fun = function() {
      return(2)
      returnType('integerScalar')
    })
  
  nfB <- nFunction(
    fun = function() {
      return(nfA())
      returnType('integerScalar')
    })
  
  nfC <- nFunction(
    fun = function() {
      return(nfB())
      returnType('integerScalar')
    })

  # Basic use
  #debug(nCompile)
  comp <- nCompile(nfB, nfA)
  expect_identical(names(comp), c("nfB", "nfA"))
  expect_true(is.function(comp$nfB))
  expect_true(is.function(comp$nfA))
  expect_equal(comp$nfB(), 2)
  # Singleton
  comp <- nCompile(nfA)
  expect_true(is.function(comp))

  # Singleton returned as list
  comp <- nCompile(nfA, returnList = TRUE)
  expect_identical(names(comp), c("nfA"))
  expect_true(is.function(comp$nfA))

  # One item named, the other not, in ...
  comp <- nCompile(f2 = nfB, nfA)
  expect_identical(names(comp), c("f2", "nfA"))
  expect_true(is.function(comp$f2))
  expect_true(is.function(comp$nfA))

  # Error from incompletely named list
  expect_error(comp <- nCompile(list(f2 = nfB, nfA))) # expected error due to incompletely named list 

  # Fully named list
  comp <- nCompile(list(f2 = nfB, f1 = nfA)) 
  expect_identical(names(comp), c("f2", "f1"))
  expect_true(is.function(comp$f2))
  expect_true(is.function(comp$f1))

  # Mix of list and individual item, both in ...
  comp <- nCompile(list(f2 = nfB, f3 = nfC), nfA) 
  expect_identical(names(comp), c("f2", "f3", "nfA"))
  expect_true(is.function(comp$f2))
  expect_true(is.function(comp$f3))
  expect_true(is.function(comp$nfA))

  # Mix of nFunction and nClass
  comp <- nCompile(nfA, nc1)
  expect_identical(names(comp), c("nfA", "nc1"))
  expect_true(is.function(comp$nfA))
  expect_true(inherits(comp$nc1$new(), "nClass"))
})

test_that("copyFiles field of compileInfo works", {
  td <- tempdir()
  workdir <- file.path(td, "nCompiler_copyFiles_test")
  dir.create(workdir, showWarnings = FALSE)
  fromFile <- file.path(workdir, "fromFile.txt")
  random_string <- basename(tempfile())
  writeLines(random_string, con=fromFile)
  foo <- nFunction(
    function(){},
    compileInfo = list(copyFiles = fromFile)
  )
  outdir <- file.path(workdir, "generated_code")
  Cfoo <- nCompile(foo,
                   dir = outdir)
  expect_true(file.exists(file.path(outdir, "fromFile.txt")))
  expect_identical(readLines(file.path(outdir, "fromFile.txt"),n=1), random_string)
})

test_that("manual C++ pieces in nFunction work", {
  foo <- nFunction(
    name = "foo",
    function(x = numericScalar()) { # manually to become vector
      nCpp("return x[0];")
      returnType('numericVector') # manually to become scalar
    },
    compileInfo = list(
      prototype = "double foo(Eigen::Tensor<double, 1> y)",
      deftype = "double foo(Eigen::Tensor<double, 1> x)"
    )
  )
  cppDefs <- nCompile(foo, control = list(return_cppDefs = TRUE))
  decl <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=TRUE)))
  def <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=FALSE)))
  expect_true(grepl("^double foo\\(Eigen::Tensor<double, 1> y\\);$", decl))
  expect_true(grepl("^double foo\\(Eigen::Tensor<double, 1> x\\)", def[2]))
  #  cfoo <- nCompile(foo) # these should work but we're avoiding full compilation for speed
  #  expect_identical(cfoo(1:3), 1)

  foo <- nFunction(
    name = "foo",
    function(x = integerVector()) { # replace with numericVector
      nCpp("return x[0];")
      returnType('integerScalar') # replace with numericScalar
    },
    compileInfo = list(
      name = "myfoo",
      cpp_code_name = "myfoo2", # not used because name over-rides it
      scopes = c("s1", "s2"),
      qualifiers = c("const -> double"),
      args = "(Eigen::Tensor<double, 1> x, double y)",
      returnType = "double"
    )
  )
  cppDefs <- nCompile(foo, control = list(return_cppDefs = TRUE))
  decl <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=TRUE)))
  def <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=FALSE)))
  expect_true(grepl("double s1::s2::myfoo \\(Eigen::Tensor<double, 1> x, double y\\) const -> double", decl))
  expect_true(grepl("double s1::s2::myfoo \\(Eigen::Tensor<double, 1> x, double y\\) const -> double", def[2]))
  ##
  foo <- nFunction(
    name = "foo",
    function() { # replace with numericVector
      nCpp("return x[0];")
    },
    compileInfo = list(
      cpp_code_name = "myfoo2",
      args = "(Eigen::Tensor<TYPE, 1> x)",
      returnType = "double",
      template = "template<typename TYPE>",
      callFromR = FALSE
    )
  )
  cppDefs <- nCompile(foo, control = list(return_cppDefs = TRUE))
  decl <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=TRUE)))
  def <- capture.output( writeCode(cppDefs[[1]]$generate(declaration=FALSE)))
  expect_true(grepl("template<typename TYPE>", decl[1]))
  expect_true(grepl("double myfoo2 \\(Eigen::Tensor<TYPE, 1> x\\)", decl[2]))
  expect_true(grepl("template<typename TYPE>", def[1]))
  expect_true(grepl("double myfoo2 \\(Eigen::Tensor<TYPE, 1> x\\)", def[2]))
  ##
  foo <- nFunction(
    name = "foo",
    function() { },
    compileInfo = list(
      cpp_code_name = "myfoo2",
      args = "(Eigen::Tensor<TYPE, 1> x)",
      returnType = "double",
      template = "template<typename TYPE>",
      callFromR = FALSE
    )
  )
})

library(nCompiler); library(testthat)
test_that("nCompile for nClass with compileInfo$createFromR=FALSE works", {
  nc_inner <- nClass(
    classname = "nc_inner",
    Cpublic = list(
      x = 'numericScalar',
      get_x = nFunction(function() {return(x)}, returnType = 'numericScalar')
    ),
    compileInfo = list(interface = "generic", createFromR = FALSE)
  )
  nc_outer <- nClass(
    classname = "nc_outer",
    Cpublic = list(
      my_inner = 'nc_inner',
      init = nFunction(function() {my_inner = nc_inner$new()}),
      get_inner = nFunction(function() {return(my_inner)}, returnType = 'nc_inner')
    )
  )
  nOptions(showCompilerOutput=TRUE)
  #comp <- nCompile(nc_inner, nc_outer)
  #debug(nCompiler:::nCompile)
  #debug(nCompiler:::writePackage)
  #debug(nCompiler:::setup_nClass_environments)
  debug(nCompiler:::setup_nClass_environments_from_package)
  comp <- nCompile(nc_inner, nc_outer, package = TRUE)
  expect_error(comp$nc_inner_new())
  obj <- comp$nc_outer$new()
  inner_obj <- obj$my_inner
  expect_true(is.null(inner_obj))
  obj$init()
  inner_obj <- obj$my_inner
  inner_obj2 <- obj$my_inner
  inner_obj <- obj$get_inner()
})

## 1. createFromR = FALSE does not have environments set up etc.
## 2. createFromR = TRUE (status quo) does not access an inner obj via interface correctly
