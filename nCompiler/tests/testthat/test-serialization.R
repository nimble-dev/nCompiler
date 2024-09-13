library(nCompiler)
library(testthat)

old_serialize_option <- get_nOption("serialize")
set_nOption("serialize", TRUE)

# To do:
# Each generated package should have one static DLLenv to always use.
# Then nUnserialize can take as a second argument simply a package name.
# And one CnCenv for each nClass.

test_that("Basic serialization via packaging works",
{
  nc1 <- nClass(
    classname = "nc1",
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
  # Use this line for debugging any issues with nc1
  # Cnc1 <- nCompile(nc1)

  writePackage(
    nc1,
    dir = tempdir(),
    package.name = "nc1Package",
    nClass_full_interface = FALSE
  )
  buildPackage("nc1Package",
               dir = tempdir(), quiet = FALSE)

  # serialize and deserialize 1 object
  obj <- new_nc1()
  expect_true(nCompiler:::is.loadedObjectEnv(obj))
  expect_equal(method(obj, "Cfoo")(1.2), 2.2)
  value(obj, "Cv") <- 1.23
  expect_equal(value(obj, "Cv"), 1.23)
  value(obj, "Cx") <- 3
  expect_equal(value(obj, "Cx"), 3L)

  serialized_obj <- nSerialize(obj)
  DLLenv <- DLLenv(obj)
  restored_obj <- nUnserialize(serialized_obj, DLLenv) # second argument should be package name. DLLenv should be recorded in the package.
  # Also check on persistence of DLLenv.
  # add a clean argument to writePackage that will do erasePackage if needed.

  expect_true(nCompiler:::is.loadedObjectEnv(restored_obj))
  expect_equal(method(restored_obj, "Cfoo")(1.2), 2.2)
  expect_equal(value(restored_obj, "Cv"), 1.23)
  value(restored_obj, "Cv") <- 2.34
  expect_equal(value(restored_obj, "Cv"), 2.34)
  expect_equal(value(restored_obj, "Cx"), 3L)
  # done

  # serialize and deserialize 3 objects
  obj2 <- new_nc1()
  value(obj2, "Cv") <- -8.5
  value(obj2, "Cx") <- -100
  obj3 <- new_nc1()
  value(obj3, "Cx") <- 2134
  serialized_objlist <- nSerialize(list(obj, obj2, obj3))
  restored_objlist <- nUnserialize(serialized_objlist, DLLenv)
  expect_equal(value(restored_objlist[[1]], "Cv"), 1.23)
  expect_equal(value(restored_objlist[[2]], "Cv"), -8.5)
  expect_equal(value(restored_objlist[[2]], "Cx"), -100)
  expect_equal(value(restored_objlist[[3]], "Cx"), 2134)
  # done

})

test_that("Basic serialization via packaging works",
{
  nc1 <- nClass(
    classname = "nc1",
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
  # Use this line for debugging any issues with nc1
  # Cnc1 <- nCompile(nc1)

  writePackage(
    nc1,
    dir = tempdir(),
    package.name = "nc1Package",
    nClass_full_interface = TRUE
  )
  buildPackage("nc1Package",
               dir = tempdir(), quiet = FALSE)

  # serialize and deserialize 1 object
  obj <- nc1Package::nc1$new()
  expect_true(inherits(obj, "R6"))
  expect_equal(obj$Cfoo(1.2), 2.2)
  obj$Cv <- 1.23
  expect_equal(obj$Cv, 1.23)
  obj$Cx <- 3
  expect_equal(obj$Cx, 3L)

  serialized_obj <- nSerialize(obj)
  message("Make finding DLLenv better")
  DLLenv <- obj$private$DLLenv
  restored_obj <- nUnserialize(serialized_obj, DLLenv) # second argument should be package name. DLLenv should be recorded in the package.
  # Also check on persistence of DLLenv.
  # add a clean argument to writePackage that will do erasePackage if needed.

  expect_true(inherits(restored_obj, "R6"))
  expect_equal(restored_obj$Cfoo(2.2), 3.2)
  restored_obj$Cv <- 1.23
  expect_equal(restored_obj$Cv, 1.23)
  restored_obj$Cx <- 3
  expect_equal(restored_obj$Cx, 3L)
  restored_obj$Cv <- 2.34
  expect_equal(restored_obj$Cv, 2.34)

  # serialize and deserialize 3 objects
  obj2 <- nc1Package::nc1$new()
  obj2$Cv <- -8.5
  obj2$Cx <- -100
  obj3 <- nc1Package::nc1$new()
  obj3$Cx <- 2134
  serialized_objlist <- nSerialize(list(obj, obj2, obj3))
  restored_objlist <- nUnserialize(serialized_objlist, DLLenv)
  expect_equal(restored_objlist[[1]]$Cv, 1.23)
  expect_equal(restored_objlist[[2]]$Cv, -8.5)
  expect_equal(restored_objlist[[2]]$Cx, -100)
  expect_equal(restored_objlist[[3]]$Cx, 2134)
  # done
})

# stopped here.

test_that("Basic serialization works (one object, same session, generic or full interface)",
          {
            nc1 <- nClass(
              classname = "nc1",
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
            ans <- nCompile(nc1, interfaces = "generic")
            obj <- ans()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)

            serialized_obj <- nSerialize(obj)
            restored_obj <- nUnserialize(serialized_obj, DLLenv(obj))

            expect_true(nCompiler:::is.loadedObjectEnv(restored_obj))
            expect_equal(method(restored_obj, "Cfoo")(1.2), 2.2)
            ## FAILS BECAUSE THE POINTER INSIDE THE RCPP FUNCTIONS IS 0x0.
            ## SO WE CAN ***ONLY*** OPERATE FROM A GENERATED PACKAGE
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)


            f_obj <- nCompiler:::to_full_interface(obj)
            f_obj$Cv
            f_obj$Rv <- 53
            # prototyping nSerialize and nUnserialize
            # These can then be worked into updates on NC_Serialize.R
            nSerialize <- function(obj, serializer) {
              # obj might be a list or other compound object.
              refhook <- function(ref_obj) {
                if(isTRUE(nCompiler:::is.loadedObjectEnv(ref_obj))) {
                  browser()
                  ## Quite half-baked toying with the serialization_mgr here.
                  ## So far I can see that it is "breathing" but haven't checked more deeply.
                  new_smgr_fn <- get("new_serialization_mgr", envir=ref_obj)
                  smgr <- new_smgr_fn()
                  ID <- method(smgr, "add_extptr")(nCompiler:::getExtptr(ref_obj))
                  as.character(ID)
                  SOE <- nCompiler:::new.serialObjectEnv(ID, parent.env(ref_obj))
                  ser_fn <- get("nComp_serialize_", envir=ref_obj)
                  test <- ser_fn(nCompiler:::getExtptr(smgr)) # Crash :)
                  unser_fn <- get("nComp_deserialize_", envir=ref_obj)
                  untest <- unser_fn(test) # This is a pointer, not an LOE

                }
                NULL
              }
              ans <- serialize(obj, connection=NULL, refhook = refhook)
              ans
            }
            NULL
            test <- nSerialize(f_obj)




            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans)) ## compilation succeeded
            obj <- ans()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)

            test <- parent.env(obj)$nComp_serialize_( nCompiler:::getExtptr(obj) )
            untest <- parent.env(obj)$nComp_deserialize_(test)

            debug(nCompiler:::serialize_nComp_object)
            test2 <- nCompiler:::serialize_nComp_object(obj)

            nCompiler:::get_value(untest, "Cv")

            serialized <-
              nCompiler:::serialize_nComp_object(obj, nComp_serialize_nc1)
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))

            deserialized <-
              nCompiler:::deserialize_nComp_object(serialized,
                                                   nComp_deserialize_nc1)
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))
            expect_equal(value(deserialized, "Cv"), 1.23)
            x <- matrix(as.numeric(1:6), nrow = 2)
            y <- matrix(as.numeric(101:106), nrow = 2)

            expect_equal(
              method(deserialized, "Cbar")(
                x, y
              ),
              x+y)
          })



test_that("Basic serialization works",
          {
            nc1 <- nClass(
              classname = "nc5",
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
            set_nOption("showCompilerOutput", TRUE)
            set_nOption("pause_after_writing_files", TRUE)
            ans <- try(nCompile(nc1, interfaces = "generic"))
            obj <- ans()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)
            f_obj <- nCompiler:::to_full_interface(obj)
            f_obj$Cv
            f_obj$Rv <- 53
            # prototyping nSerialize and nUnserialize
            # These can then be worked into updates on NC_Serialize.R
            nSerialize <- function(obj, serializer) {
              # obj might be a list or other compound object.
              refhook <- function(ref_obj) {
                if(isTRUE(nCompiler:::is.loadedObjectEnv(ref_obj))) {
                  browser()
                  ## Quite half-baked toying with the serialization_mgr here.
                  ## So far I can see that it is "breathing" but haven't checked more deeply.
                  new_smgr_fn <- get("new_serialization_mgr", envir=ref_obj)
                  smgr <- new_smgr_fn()
                  ID <- method(smgr, "add_extptr")(nCompiler:::getExtptr(ref_obj))
                  as.character(ID)
                  SOE <- nCompiler:::new.serialObjectEnv(ID, parent.env(ref_obj))
                  ser_fn <- get("nComp_serialize_", envir=ref_obj)
                  test <- ser_fn(nCompiler:::getExtptr(smgr)) # Crash :)
                  unser_fn <- get("nComp_deserialize_", envir=ref_obj)
                  untest <- unser_fn(test) # This is a pointer, not an LOE

                }
                NULL
              }
              ans <- serialize(obj, connection=NULL, refhook = refhook)
              ans
            }
            NULL
            test <- nSerialize(f_obj)




            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans)) ## compilation succeeded
            obj <- ans()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)

            test <- parent.env(obj)$nComp_serialize_( nCompiler:::getExtptr(obj) )
            untest <- parent.env(obj)$nComp_deserialize_(test)

            debug(nCompiler:::serialize_nComp_object)
            test2 <- nCompiler:::serialize_nComp_object(obj)

            nCompiler:::get_value(untest, "Cv")

            serialized <-
              nCompiler:::serialize_nComp_object(obj, nComp_serialize_nc1)
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))

            deserialized <-
              nCompiler:::deserialize_nComp_object(serialized,
                                                   nComp_deserialize_nc1)
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))
            expect_equal(value(deserialized, "Cv"), 1.23)
            x <- matrix(as.numeric(1:6), nrow = 2)
            y <- matrix(as.numeric(101:106), nrow = 2)

            expect_equal(
              method(deserialized, "Cbar")(
                x, y
              ),
              x+y)
          })


test_that("Basic serialization via packaging works",
{
  library(nCompiler)
  library(testthat)
  old_serialize_option <- get_nOption("serialize")
  set_nOption("serialize", TRUE)

  nc1 <- nClass(
              classname = "nc1",
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
            set_nOption("showCompilerOutput", TRUE)
            set_nOption("pause_after_writing_files", TRUE)
  # Use this line for debugging any issues with nc1
  #  Cnc1 <- nCompile(nc1)

            writePackage(
                nc1,
                dir = tempdir(),
                package.name = "nc1Package",
                nClass_full_interface = FALSE
            )
            buildPackage("nc1Package",
                         dir = tempdir(), quiet=FALSE)

  #debug(setup_nClass_environments_from_package)
  #setup_nClass_environments_from_package(pkgName = "nc1Package", "nc1")
            # prototype for .onAttach
            ## newDLLenv <- nCompiler:::make_DLLenv()
            ## compiledFuns <- list(new_nc1 = nc1Package:::new_nc1,
            ##                      set_CnClass_env_nc1 =  nc1Package:::set_CnClass_env_nc1,
            ##                      new_serialization_mgr =  nc1Package:::new_serialization_mgr,
            ##                      nComp_serialize_ =  nc1Package:::nComp_serialize_,
            ##                      nComp_deserialize_ =  nc1Package:::nComp_deserialize_,
            ##                      call_method = nc1Package:::call_method,
            ##                      get_value = nc1Package:::get_value,
            ##                      set_value = nc1Package:::set_value)
            ## compiledFuns <- nCompiler:::setup_nClass_environments(compiledFuns,
            ##                                        newDLLenv,
            ##                                        nC_names = "nc1",
            ##                                        R6interfaces = list(),
            ##                                        returnList = TRUE)

            #
            obj <- new_nc1()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)
            # prototyping nSerialize and nUnserialize
            # These can then be worked into updates on NC_Serialize.R

  test <- nSerialize(obj)
  DLLenv <- nCompiler:::get_DLLenv(obj)
            orig_obj <- obj
            obj <- test
  test2 <- nUnserialize(test, DLLenv) # second argument should be package name. DLLenv should be recorded in the package.
                                        # Also check on persistence of DLLenv.
                                        # add a clean argument to writePackage that will do erasePackage if needed.

  value(test2, "Cx")
  method(test2, "Cfoo")(1.2)

            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans)) ## compilation succeeded
            obj <- ans()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)
            
            test <- parent.env(obj)$nComp_serialize_( nCompiler:::getExtptr(obj) )
            untest <- parent.env(obj)$nComp_deserialize_(test)
            
            debug(nCompiler:::serialize_nComp_object)
            test2 <- nCompiler:::serialize_nComp_object(obj)
            
            nCompiler:::get_value(untest, "Cv")
            
            serialized <- 
              nCompiler:::serialize_nComp_object(obj, nComp_serialize_nc1)
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))
            
            deserialized <- 
              nCompiler:::deserialize_nComp_object(serialized, 
                                                   nComp_deserialize_nc1)
            expect_true(nCompiler:::is.loadedObjectEnv(serialized))
            expect_equal(value(deserialized, "Cv"), 1.23)
            x <- matrix(as.numeric(1:6), nrow = 2)
            y <- matrix(as.numeric(101:106), nrow = 2)
            
            expect_equal(
              method(deserialized, "Cbar")(
                x, y
              ),
              x+y)
          })


# The below block uses to calls to `RScript` to test nClass saving/loading in
# distinct sessions. See scripts in the dir testthat/serialization_test_utils/
delete_dir <- FALSE
if (!dir.exists("testserial_nCompInternalOnly")) {
  dir.create("testserial_nCompInternalOnly")
  delete_dir <- TRUE
}

test_that("Saving and loading nClasses across sessions works (generic interface)", {
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_save_Generic.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_read_Generic.R'), 
                            package = 'nCompiler')))
})

test_that("Saving and loading nClasses across sessions works (full interface)", {
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_save_Full.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_read_Full.R'), 
                            package = 'nCompiler')))
})


test_that("Saving and loading mult nClasses across sessions works", {
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_save_Multiple.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_read_Multiple.R'), 
                            package = 'nCompiler')))
})


test_that("Saving and loading an nClass from a specified package works", {

  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_create_Package.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_save_Package.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testserial_read_Package.R'), 
                            package = 'nCompiler')))
})


if (delete_dir) unlink("testserial_nCompInternalOnly", recursive = TRUE)




test_that("Serialization works for Rcpp types", {
  # Can we serialize and retrieve all supported Rcpp types?
  rcpp_supported_types <- c(
    "RcppNumericVector",
    "RcppNumericMatrix",
    "RcppIntegerVector",
    "RcppIntegerMatrix",
    "RcppLogicalVector",
    "RcppLogicalMatrix",
    "RcppCharacterVector",
    "RcppCharacterMatrix",
    "RcppComplexVector",
    "RcppComplexMatrix",
    # "RcppDateVector", # Doesn't work
    # "RcppDatetimeVector", # Doesn't work
    "RcppRawVector",
    # "RcppDataFrame", # Doesn't work
    "RcppS4"#,
    # "RcppFunction", # Doesn't work
    # "RcppEigenMatrixXd", # Doesn't work
    # "RcppEigenMatrixXi", # Doesn't work
    # "RcppEigenMatrixXcd", # Doesn't work
    # "RcppEigenVectorXd", # Doesn't work
    # "RcppEigenVectorXi", # Doesn't work
    # "RcppEigenVectorXcd" # Doesn't work
  )
  IntMat <- matrix(1:9, nrow = 3)
  DblMat <- matrix(1:9 / 10, nrow = 3)
  LogMat <- IntMat %% 2 == 0
  ChrMat <- matrix(as.character(1:9), nrow = 3)
  CpxMat <- matrix(1:9 + 2i, nrow  = 3)
  track <- setClass("track", slots = c(x="numeric", y="numeric"))
  t1 <- track(x = 1:10, y = 1:10 + rnorm(10))
  
  # The following is a list of objects with types corresponding to the list of
  # supported Rcpp types above.
  rcpp_type_values <- list(as.numeric(DblMat), DblMat,
                           as.numeric(IntMat), IntMat,
                           as.logical(LogMat), LogMat,
                           as.character(ChrMat), ChrMat,
                           as.complex(CpxMat), CpxMat,
                           #rep(as.Date(c("2010-01-01", "2011-02-03")), 2),
                           #rep(as.POSIXct(c(Sys.time(), Sys.time() + 100900)), 2),
                           serialize(ChrMat, connection = NULL),
                           #data.frame(x = 1:9, y = (1:9)^2),
                           t1# rnorm, 
                           #DblMat, IntMat, CpxMat,
                           #as.numeric(DblMat), as.numeric(IntMat), 
                           #as.complex(CpxMat)
                           )
  compare_fn <- list(all.equal.numeric, all.equal.numeric,
                     all.equal.numeric, all.equal.numeric,
                     all.equal, all.equal, 
                     all.equal.character, all.equal.character,
                     all.equal, all.equal, 
                     #all.equal, all.equal, 
                     all.equal.raw,
                     #identical, 
                     identical#, 
                     #identical, 
                     #all.equal, all.equal, all.equal, 
                     #all.equal, all.equal, all.equal
                     )
  
  test_rcpp_serial_class <- function(type, value, compfn) {
    name <- paste0("nc_", type)
    nc1 <- nClass(classname = name, 
                  Cpublic = list(x = type))
    nc1_generator <- nCompile_nClass(nc1, interface = "generic")
    
    my_nc1 <- nc1_generator[[1]]()
    value(my_nc1, "x") <- value
    serialized <- serialize_nComp_object(my_nc1, 
                                         get(paste0("nComp_serialize_", name)))
    
    deserialized <- deserialize_nComp_object(serialized, 
                                             get(paste0("nComp_deserialize_", name)))
    return(compfn(value(deserialized, "x"), value(my_nc1, "x")))
  }

  for (i in 1:length(rcpp_supported_types)) {
    # cat(i, "\n")
    expect_true(
      test_rcpp_serial_class(value = rcpp_type_values[[i]],
                             type = rcpp_supported_types[i],
                             compfn = compare_fn[[i]])
    )
  }
})


test_that("Serialize with a bad classname", {
  nc1 <- nClass(
    classname = "nc one",
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
  ans <- try(nCompile_nClass(nc1, interface = "generic"))
  expect_true(is.function(ans[[1]])) ## compilation succeeded
  
})

set_nOption("serialize", old_serialize_option)


