#library(nCompiler)
library(testthat)

message("There will be a problem with serialization and pre-defined nClasses.\n",
        "I think all serialization code needs to be #ifdef protected so that in\n",
        "future compilations it will be dynamically included or not.")

message('We need serialization tests of object networks.')
message('Serialization does not fully work in non-package mode, when it will still be needed for object copying.')
message('Need to tetst interactions between serialization and nClass inheritance.')

# These tests contain basics via four modes of compilation:
# nCompile(package=TRUE) or writePackage following by devtools::install and
# full interface or generic interface

# We still need tests of interesting patterns of object ownership/referencing networks
# and also of all native data types (Eigen, Rcpp, and CppAD types in particular).

old_serialize_option <- get_nOption("serialize")
set_nOption("serialize", TRUE)

# This is a workaround to pkg_name::var.
# This is necessary because on GitHub Actions for testing, we use
# `setup-r-dependencies`. This **aggressively** checks **all** directories
# in the package structure and identifies **all** pkg::var code
# and then attempts to install a package called "pkg".
# Here we are dynamically generating packages, e.g. nc1Package,
# and then checking that they work. So we use access_dynamic_package,
# which internally uses get(), to avoid the `::` syntax.
access_dynamic_package <- function(pkg_name, var) {
  if (!isNamespaceLoaded(pkg_name)) {
    stop(paste("Dynamic package", pkg_name, "is not loaded"))
  }
  ns <- asNamespace(pkg_name)
  get(var, envir = ns, inherits = FALSE)
}

# Same rationale as above:
load_dynamic_namespace <- function(pkg_name) {
  eval(call("loadNamespace", pkg_name))
}

in_new_R_session <- function(code,
                             pkgName,
                             lib,
                             transfer,
                             runfile = system.file("tests","testthat","serialization_test_in_new_R_session.R",
                                                   package = "nCompiler"),
                             outdir,
                             overwrite=TRUE,
                             test = TRUE
                             ) {
  code <- substitute(code)

  if(!dir.exists(outdir))
    dir.create(outdir)
  else {
    if(overwrite) {
      unlink(outdir, recursive = TRUE)
      dir.create(outdir)
    }
  }

  codefile <- file.path(outdir, "testing_code_.R")
  writeLines(deparse(code),
             con = codefile)

  savefile <- NULL
  if(!missing(transfer)) {
    savefile <- file.path(outdir, "objects_.RData")
    save(transfer, file = savefile)
  }

  system2("Rscript",
          c(runfile,
            if(!missing(pkgName)) paste0("--pkgName=",pkgName) else NULL,
            if(!missing(lib)) paste0("--lib=",lib) else NULL,
            paste0("--codefile=",codefile),
            if(!is.null(savefile)) paste0("--savefile=",savefile) else NULL),
          stdout = file.path(outdir, "stdout"),
          stderr = file.path(outdir, "stderr"))

  if(test) {
    output <- readLines(file.path(outdir, "stdout"))
    failure_lines <- grepl("Failure", output)
    if(any(failure_lines)) {
      cat("There were some failures\n")
      writeLines(output)
    }
    expect_true(!any(failure_lines))
  }
}

test_that("in_new_R_session works",
          in_new_R_session({test_that("foo", {expect_true(1==1)})},
                 outdir = file.path(tempdir(), "test_output"))
)

test_that("Basic serialization works (via writePackage, with generic interface, for multiple objects, in current or new session)",
{
  nc1 <- nClass(
    classname = "nc1",
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      C.v = 'numericScalar',
      C.x = 'integerScalar',
      Cfoo = nFunction(
        fun = function(x) {
          return(x+1)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar'),
      Cbar = nFunction(
        fun = function(x, y) {
          return(x + y)
        },
        argTypes = list(x = 'numericMatrix',
                        y = 'numericMatrix'),
        returnType = 'numericMatrix')
    )
  )

  writePackage(
    nc1,
    dir = tempdir(),
    pkgName = "nc1Package",
    modify = "clear",
    interfaces = "generic"
  )

  lib <- file.path(tempdir(), "test_nCompile_serialization_lib")
  dir.create(lib, showWarnings=FALSE, recursive=TRUE)
  withr::with_libpaths(lib, action = "prefix", code = {
    devtools::install(file.path(tempdir(), "nc1Package"), quiet=TRUE,
                      upgrade = "never", quick=TRUE)
    # more evasion of setup-r-dependencies used in CI testing.
    # Write loadNamespace("nc1Package") indirectly so it
    # doesn't think nc1Package is a CRAN package
    load_dynamic_namespace("nc1Package")
  })

  ## serialize and deserialize 1 object
  # build and test object
  obj <- access_dynamic_package("nc1Package", "nc1")()
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(obj))
  expect_equal(method(obj, "Cfoo")(1.2), 2.2)
  value(obj, "C.v") <- 1.23
  expect_equal(value(obj, "C.v"), 1.23)
  value(obj, "C.x") <- 3
  expect_equal(value(obj, "C.x"), 3L)

  # serialize it
  serialized_obj <- nSerialize(obj)
  # restore (unserialize) it
  restored_obj <- nUnserialize(serialized_obj)

  # test the restored objected
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(restored_obj))
  expect_equal(method(restored_obj, "Cfoo")(1.2), 2.2)
  expect_equal(value(restored_obj, "C.v"), 1.23)
  value(restored_obj, "C.v") <- 2.34
  expect_equal(value(restored_obj, "C.v"), 2.34)
  expect_equal(value(restored_obj, "C.x"), 3L)
  # done

  # serialize and deserialize 3 objects
  obj2 <- access_dynamic_package("nc1Package","nc1")()
  value(obj2, "C.v") <- -8.5
  value(obj2, "C.x") <- -100
  obj3 <- access_dynamic_package("nc1Package","nc1")()
  value(obj3, "C.x") <- 2134
  serialized_objlist <- nSerialize(list(obj, obj2, obj3))
  restored_objlist <- nUnserialize(serialized_objlist, "nc1Package") # alt mode of providing package
  # test the restored objects
  expect_equal(value(restored_objlist[[1]], "C.v"), 1.23)
  expect_equal(value(restored_objlist[[2]], "C.v"), -8.5)
  expect_equal(value(restored_objlist[[2]], "C.x"), -100)
  expect_equal(value(restored_objlist[[3]], "C.x"), 2134)
  # done

  outdir <- file.path(tempdir(), "nComp_serialize_working_test")
  # test in a new R session
  in_new_R_session({
    serialized_objlist <- transfer$serialized_objlist
    restored_objlist <- nUnserialize(serialized_objlist)
    test_that("restored objects work", {
      expect_equal(value(restored_objlist[[1]], "C.v"), 1.23)
      expect_equal(value(restored_objlist[[2]], "C.v"), -8.5)
      expect_equal(value(restored_objlist[[2]], "C.x"), -100)
      expect_equal(value(restored_objlist[[3]], "C.x"), 2134)
    })
  },
  transfer = list(serialized_objlist = serialized_objlist),
  pkgName = "nc1Package",
  lib = lib,
  outdir = outdir
  )
})

test_that("Basic serialization works (via nCompile(package=TRUE), with generic interface, for single and multiple objects, in current or new session)",
{
  nc1 <- nClass(
    ## classname = "nc1", ## If we give this the same name as above, on linux the old one is found and used at some step, making it look like deserialization fails.
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cx = 'integerScalar',
      Cfoo = nFunction(
        fun = function(x) {
          return(x+1)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar'),
      Cbar = nFunction(
        fun = function(x, y) {
          return(x + y)
        },
        argTypes = list(x = 'numericMatrix',
                        y = 'numericMatrix'),
        returnType = 'numericMatrix')
    )
  )

  nc1gen <- nCompile(nc1, package=TRUE, interfaces = "generic", returnList=FALSE)

  ## serialize and deserialize 1 object
  # build and test object
  obj <- nc1gen()
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(obj))
  expect_equal(method(obj, "Cfoo")(1.2), 2.2)
  value(obj, "Cv") <- 1.23
  expect_equal(value(obj, "Cv"), 1.23)
  value(obj, "Cx") <- 3
  expect_equal(value(obj, "Cx"), 3L)

  # serialize it
  serialized_obj <- nSerialize(obj)
  # restore (unserialize) it
  restored_obj <- nUnserialize(serialized_obj)

  # test the restored objected
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(restored_obj))
  expect_equal(method(restored_obj, "Cfoo")(1.2), 2.2)
  expect_equal(value(restored_obj, "Cv"), 1.23)
  value(restored_obj, "Cv") <- 2.34
  expect_equal(value(restored_obj, "Cv"), 2.34)
  expect_equal(value(restored_obj, "Cx"), 3L)
  # done

  # serialize and deserialize 3 objects
  obj2 <- nc1gen()
  value(obj2, "Cv") <- -8.5
  value(obj2, "Cx") <- -100
  obj3 <- nc1gen()
  value(obj3, "Cx") <- 2134
  serialized_objlist <- nSerialize(list(obj, obj2, obj3))
  restored_objlist <- nUnserialize(serialized_objlist)
  # test the restored objects
  expect_equal(value(restored_objlist[[1]], "Cv"), 1.23)
  expect_equal(value(restored_objlist[[2]], "Cv"), -8.5)
  expect_equal(value(restored_objlist[[2]], "Cx"), -100)
  expect_equal(value(restored_objlist[[3]], "Cx"), 2134)
  # done

  outdir <- file.path(tempdir(), "nComp_serialize_working_test3")
  # test in a new R session
  lib <- file.path(tempdir(), "templib")
  in_new_R_session({
    serialized_objlist <- transfer$serialized_objlist
    restored_objlist <- nUnserialize(serialized_objlist)
    test_that("restored objects work", {
      expect_equal(value(restored_objlist[[1]], "Cv"), 1.23)
      expect_equal(value(restored_objlist[[2]], "Cv"), -8.5)
      expect_equal(value(restored_objlist[[2]], "Cx"), -100)
      expect_equal(value(restored_objlist[[3]], "Cx"), 2134)
    })
  },
  transfer = list(serialized_objlist = serialized_objlist),
  pkgName = "nc1Package",
  lib = lib,
  outdir = outdir
  )
})

test_that("Basic serialization works (via writePackage, with full interface, for single or multiple objects, in current or new session)",
{
  nc1 <- nClass(
    # classname = "nc1",
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cx = 'integerScalar',
      Cfoo = nFunction(
        fun = function(x) {
          return(x+1)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar'),
      Cbar = nFunction(
        fun = function(x, y) {
          return(x + y)
        },
        argTypes = list(x = 'numericMatrix',
                        y = 'numericMatrix'),
        returnType = 'numericMatrix')
    )
  )

  writePackage(
    nc1,
    dir = tempdir(),
    pkgName = "nc1PackageB",
    modify = "clear",
    interfaces = "full"
  )

  lib <- file.path(tempdir(), "test_nCompile_serialization_lib")
  dir.create(lib, showWarnings=FALSE, recursive=TRUE)
  withr::with_libpaths(lib, action = "prefix", code = {
    devtools::install(file.path(tempdir(), "nc1PackageB"), quiet=TRUE,
                      upgrade = "never", quick=TRUE, force=TRUE)
    # more evasion of setup-r-dependencies used in CI testing.
    # Write loadNamespace("nc1PackageB") indirectly so it
    # doesn't think nc1PackageB is a CRAN package
    load_dynamic_namespace("nc1PackageB")
  })

  ## serialize and deserialize 1 object
  # build and test object
  obj <- access_dynamic_package("nc1PackageB", "nc1")$new() #nc1PackageB::nc1$new()
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(obj$private$CppObj))
  expect_equal(obj$Cfoo(1.2), 2.2)
  obj$Cv <- 1.23
  expect_equal(obj$Cv, 1.23)
  obj$Cx <- 3
  expect_equal(obj$Cx, 3L)

  # serialize it
  serialized_obj <- nSerialize(obj)
  # restore (unserialize) it
  restored_obj <- nUnserialize(serialized_obj)

  # test the restored objected
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(restored_obj$private$CppObj))
  expect_equal(restored_obj$Cfoo(1.2), 2.2)
  expect_equal(restored_obj$Cv, 1.23)
  restored_obj$Cv <- 2.34
  expect_equal(restored_obj$Cv, 2.34)
  expect_equal(restored_obj$Cx, 3L)
  # done

  # serialize and deserialize 3 objects
  obj2 <- access_dynamic_package("nc1PackageB", "nc1")$new() # nc1PackageB::nc1$new()
  obj2$Cv <- -8.5
  obj2$Cx <- -100
  obj3 <- access_dynamic_package("nc1PackageB", "nc1")$new() # nc1PackageB::nc1$new()
  obj3$Cx <- 2134
  serialized_objlist <- nSerialize(list(obj, obj2, obj3))
  restored_objlist <- nUnserialize(serialized_objlist, "nc1PackageB") # alt mode of providing package
  # test the restored objects
  expect_equal(restored_objlist[[1]]$Cv, 1.23)
  expect_equal(restored_objlist[[2]]$Cv, -8.5)
  expect_equal(restored_objlist[[2]]$Cx, -100)
  expect_equal(restored_objlist[[3]]$Cx, 2134)
  # done

  outdir <- file.path(tempdir(), "nComp_serialize_working_test2")
  # test in a new R session
  in_new_R_session({
    serialized_objlist <- transfer$serialized_objlist
    restored_objlist <- nUnserialize(serialized_objlist)
    test_that("restored objects work", {
      expect_equal(restored_objlist[[1]]$Cv, 1.23)
      expect_equal(restored_objlist[[2]]$Cv, -8.5)
      expect_equal(restored_objlist[[2]]$Cx, -100)
      expect_equal(restored_objlist[[3]]$Cx, 2134)
    })
  },
  transfer = list(serialized_objlist = serialized_objlist),
  pkgName = "nc1PackageB",
  lib = lib,
  outdir = outdir
  )
})

test_that("Basic serialization works (via nCompile(package=TRUE), with full interface, for single or multiple objects, in current or new session)",
{
  nc1 <- nClass(
    # classname = "nc1",
    Rpublic = list(
      Rv = NULL,
      Rfoo = function(x) x+1
    ),
    Cpublic = list(
      Cv = 'numericScalar',
      Cx = 'integerScalar',
      Cfoo = nFunction(
        fun = function(x) {
          return(x+1)
        },
        argTypes = list(x = 'numericScalar'),
        returnType = 'numericScalar'),
      Cbar = nFunction(
        fun = function(x, y) {
          return(x + y)
        },
        argTypes = list(x = 'numericMatrix',
                        y = 'numericMatrix'),
        returnType = 'numericMatrix')
    )
  )

  nc1gen <- nCompile(nc1, package = TRUE, interfaces="full", returnList=FALSE)

  ## serialize and deserialize 1 object
  # build and test object
  obj <- nc1gen$new()
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(obj$private$CppObj))
  expect_equal(obj$Cfoo(1.2), 2.2)
  obj$Cv <- 1.23
  expect_equal(obj$Cv, 1.23)
  obj$Cx <- 3
  expect_equal(obj$Cx, 3L)

  # serialize it
  serialized_obj <- nSerialize(obj)
  # restore (unserialize) it
  restored_obj <- nUnserialize(serialized_obj)

  # test the restored objected
  expect_true(`:::`("nCompiler", "is.loadedObjectEnv")(restored_obj$private$CppObj))
  expect_equal(restored_obj$Cfoo(1.2), 2.2)
  expect_equal(restored_obj$Cv, 1.23)
  restored_obj$Cv <- 2.34
  expect_equal(restored_obj$Cv, 2.34)
  expect_equal(restored_obj$Cx, 3L)
  # done

  # serialize and deserialize 3 objects
  obj2 <- nc1gen$new()
  obj2$Cv <- -8.5
  obj2$Cx <- -100
  obj3 <- nc1gen$new()
  obj3$Cx <- 2134
  serialized_objlist <- nSerialize(list(obj, obj2, obj3))
  restored_objlist <- nUnserialize(serialized_objlist) # alt mode of providing package
  # test the restored objects
  expect_equal(restored_objlist[[1]]$Cv, 1.23)
  expect_equal(restored_objlist[[2]]$Cv, -8.5)
  expect_equal(restored_objlist[[2]]$Cx, -100)
  expect_equal(restored_objlist[[3]]$Cx, 2134)
  # done

  outdir <- file.path(tempdir(), "nComp_serialize_working_test4")
  # test in a new R session
  lib <- file.path(tempdir(), "templib")
  in_new_R_session({
    serialized_objlist <- transfer$serialized_objlist
    restored_objlist <- nUnserialize(serialized_objlist)
    test_that("restored objects work", {
      expect_equal(restored_objlist[[1]]$Cv, 1.23)
      expect_equal(restored_objlist[[2]]$Cv, -8.5)
      expect_equal(restored_objlist[[2]]$Cx, -100)
      expect_equal(restored_objlist[[3]]$Cx, 2134)
    })
  },
  transfer = list(serialized_objlist = serialized_objlist),
  pkgName = "nc1PackageB",
  lib = lib,
  outdir = outdir
  )
})

set_nOption("serialize", old_serialize_option)
