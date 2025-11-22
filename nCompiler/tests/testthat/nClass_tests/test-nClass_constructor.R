#library(nCompiler)
library(testthat)

test_that("nClass constructor works", {
  # ditto
  nc <- nClass(
    classname = "ctor_test",
    Cpublic = list(
      v = 'numericScalar',
      w = 'numericScalar',
      hw = nFunction(function() {nCpp('Rprintf("hello world\\n");')}),
      ctor1 = nFunction(function(v_ = 'numericScalar',
                                 w_ = 'numericScalar') {
        v <- v_;
      },
      compileInfo = list(constructor=TRUE,
                         initializers = list('w(w_)')))
    )
  )
  cppDef <- nCompile(nc, control=list(return_cppDefs=TRUE))
  out <- capture.output(writeCode(cppDef[[1]]$generate(TRUE)))
  expect_true(sum(grepl(" ctor_test \\( double", out))==1)
  out <- capture.output(writeCode(cppDef[[1]]$generate()))
  expect_true(sum(grepl("ctor_test::ctor_test \\( double", out))==1)
  expect_true(sum(grepl("w\\(w_\\)", out))==1)
  expect_no_error(test <- nCompile(nc))
  obj <- test$new()
  rm(obj)
  gc()
})

# This test is a little bit half-baked. Not thorough on feature coverage.
test_that("nClass replacing default constructor works", {
  # ditto
  nc <- nClass(
    classname = "ctor_test",
    Cpublic = list(
      v = 'numericScalar',
      w = 'numericScalar',
      hw = nFunction(function() {nCpp('Rprintf("hello world\\n");')}),
      # same name as class indicates this will replace default
      ctor_test = nFunction(function(v_ = 'numericScalar',
                                 w_ = 'numericScalar') {
        v <- v_;
      },
      compileInfo = list(constructor=TRUE,
                         initializers = list('w(w_)'))
    )),
    compileInfo = list(createFromR = FALSE) # will error if TRUE b/c we are replacing default ctor
  )
  nc2 <- nClass(
    classname = "simple",
    Cpublic = list(
      hw = nFunction(function() {nCpp('Rprintf("hello world from simple\\n");')})
    ),
    compileInfo = list(interface = "generic", createFromR=FALSE)
  )
  #debug(nCompiler:::nCompile_finish_nonpackage)
  expect_no_error(Cnc2 <- nCompile(nc2))
  expect_true(is.null(Cnc2))

  cppDef <- nCompile(nc, control=list(return_cppDefs=TRUE))
  out <- capture.output(writeCode(cppDef[[1]]$generate(TRUE)))
  expect_true(sum(grepl(" ctor_test \\( double", out))==1)
  out <- capture.output(writeCode(cppDef[[1]]$generate()))
  expect_true(sum(grepl("ctor_test::ctor_test \\( double", out))==1)
  expect_true(sum(grepl("w\\(w_\\)", out))==1)
  #debug(nCompiler:::nCompile_finish_nonpackage)
  #test <- nCompile(nc)
  expect_no_error(test <- nCompile(nc))
  expect_true(is.null(test))
#  rm(obj)
#  gc()
})

test_that("manual initialize works and Cpp ctor call is inserted", {
  nc <- nClass(
    classname = "methods_test",
    Rpublic = list(
      Ra = 0,
      initialize = function() {
        print("calling initialize")
        self$Ra <- 1
      },
      get_Ra = function() {
        self$Ra
      },
      get_Ca = function() {
        self$Ca
      }
    ),
    Cpublic = list(
      Ca = 'numericScalar',
      methods_test = nFunction(
        function() {
          nCpp('Rprintf("calling c++ constructor\\n")')
          Ca <- 2
        },
        compileInfo = list(constructor=TRUE)
      )
    )
  )

  obj <- nc$new()
  expect_equal(obj$Ra, 1)
  expect_equal(obj$get_Ra(), 1)
  #obj$Ca
  #obj$get_Ca()
  # Need initialization of uncompiled Cpublic variables?

  Cnc <- nCompile(nc)
  out <- capture_output(Cobj <- Cnc$new())
  # the C++ initializer output should appear BEFORE the R initializer msg
  expect_true(regexpr("initialize", out) > regexpr("constructor", out))

  expect_equal(Cobj$Ra, 1)
  expect_equal(Cobj$get_Ra(), 1)
  expect_equal(Cobj$Ca, 2)
  expect_equal(Cobj$get_Ca(), 2)
  rm(Cobj); gc()
})


test_that("manual initialize with hand-coded C++ initialization works", {
  nc <- nClass(
    classname = "methods_test",
    Rpublic = list(
      Ra = 0,
      initialize = function() {
        print("calling initialize")
        if(isCompiled()) initializeCpp()
        self$Ra <- 1
      },
      get_Ra = function() {
        self$Ra
      },
      get_Ca = function() {
        self$Ca
      }
    ),
    Cpublic = list(
      Ca = 'numericScalar',
      methods_test = nFunction(
        function() {
          nCpp('Rprintf("calling c++ constructor\\n")')
          Ca <- 2
        },
        compileInfo = list(constructor=TRUE)
      )
    ),
    compileInfo=list(omit_automatic_Cpp_construction=TRUE)
  )

  obj <- nc$new()
  expect_equal(obj$Ra, 1)
  expect_equal(obj$get_Ra(), 1)
  expect_true(isFALSE(obj$isCompiled()))
  #obj$Ca
  #obj$get_Ca()
  # Need initialization of uncompiled Cpublic variables?

  Cnc <- nCompile(nc)
  out <- capture_output(Cobj <- Cnc$new())
  # the C++ initializer output should now appear AFTER the R initializer msg
  expect_true(regexpr("initialize", out) < regexpr("constructor", out))
  expect_true(isTRUE(Cobj$isCompiled()))
  expect_equal(Cobj$Ra, 1)
  expect_equal(Cobj$get_Ra(), 1)
  expect_equal(Cobj$Ca, 2)
  expect_equal(Cobj$get_Ca(), 2)
  rm(Cobj); gc()
})


test_that("manual initialize OMITTED with hand-coded C++ initialization compiles but is correctly broken", {
  nc <- nClass(
    classname = "methods_test",
    Rpublic = list(
      Ra = 0,
      initialize = function() {
        print("calling initialize")
        # if(isCompiled()) initializeCpp() # OMITTED!
        self$Ra <- 1
      },
      get_Ra = function() {
        self$Ra
      },
      get_Ca = function() {
        self$Ca
      }
    ),
    Cpublic = list(
      Ca = 'numericScalar',
      methods_test = nFunction(
        function() {
          nCpp('Rprintf("calling c++ constructor\\n")')
          Ca <- 2
        },
        compileInfo = list(constructor=TRUE)
      )
    ),
    compileInfo=list(omit_automatic_Cpp_construction=TRUE)
  )

  obj <- nc$new()
  expect_equal(obj$Ra, 1)
  expect_equal(obj$get_Ra(), 1)
  expect_true(isFALSE(obj$isCompiled()))
  #obj$Ca
  #obj$get_Ca()
  # Need initialization of uncompiled Cpublic variables?

  Cnc <- nCompile(nc)
  out <- capture_output(Cobj <- Cnc$new())
  # the C++ initializer output should now appear AFTER the R initializer msg
  expect_true(regexpr("constructor", out)==-1)
  expect_true(isTRUE(Cobj$isCompiled()))
  expect_equal(Cobj$Ra, 1)
  expect_equal(Cobj$get_Ra(), 1)
  expect_error(Cobj$Ca)
  expect_error(Cobj$get_Ca())

  out2 <- capture_output(Cobj$initializeCpp())
  expect_true(regexpr("constructor", out2)>0)
  expect_true(isTRUE(Cobj$isCompiled()))
  expect_equal(Cobj$Ra, 1)
  expect_equal(Cobj$get_Ra(), 1)
  expect_equal(Cobj$Ca, 2)
  expect_equal(Cobj$get_Ca(), 2)

  rm(Cobj); gc()
})
