# Issue with where to write objects... can't use tempdir() across sessions

context("Test serialization with cereal package")

old_serialize_option <- get_nOption("serialize")
set_nOption("serialize", TRUE)

test_that("Basic serialization works",
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
            ans <- try(nCompile_nClass(nc1, interface = "generic"))
            expect_true(is.function(ans[[1]])) ## compilation succeeded
            obj <- ans[[1]]()
            expect_true(nCompiler:::is.loadedObjectEnv(obj))
            expect_equal(method(obj, "Cfoo")(1.2), 2.2)
            value(obj, "Cv") <- 1.23
            expect_equal(value(obj, "Cv"), 1.23)
            value(obj, "Cx") <- 3
            expect_equal(value(obj, "Cx"), 3L)
            
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
# distinct sessions. See scripts in the folder testthat/serialization_test_utils/
delete_dir <- FALSE
if (!dir.exists("testserial_nCompInternalOnly")) {
  dir.create("testserial_nCompInternalOnly")
  delete_dir <- TRUE
}

test_that("Saving and loading nClasses across sessions works (generic interface)", {
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testutil_save_nClass.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testutil_read_nClass.R'), 
                            package = 'nCompiler')))
})

test_that("Saving and loading nClasses across sessions works (full interface)", {
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testutil_save_full_nClass.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testutil_read_full_nClass.R'), 
                            package = 'nCompiler')))
})


test_that("Saving and loading mult nClasses across sessions works", {
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testutil_save_multiple.R'), 
                            package = 'nCompiler')))
  system(paste0("Rscript ",
                system.file(file.path('tests', 'testthat', 
                                      'serialization_test_utils', 
                                      'testutil_read_multiple.R'), 
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


