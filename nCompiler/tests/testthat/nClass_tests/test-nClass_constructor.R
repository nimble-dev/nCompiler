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

test_that("manual initialize works and Cpp ctor call is made", {
  # Requirement is that users include "super$initialize()".
  # That seems reasonable.
  # Manual alternative is below.
  nc <- nClass(
    classname = "methods_test",
    Rpublic = list(
      Ra = 0,
      initialize = function() {
        super$initialize()
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


test_that("manual initialize with hand-coded Cpublic initialization works", {
  # two distinct steps on display here:
  # 1. manual alternative to calling super$initialize() is initialize_Cpublic()
  # 2. If the auto_include of C++ constructor is turned off, it can be constructed manually
  #    at whatever step of initialize one wants.
  nc <- nClass(
    classname = "methods_test",
    Rpublic = list(
      Ra = 0,
      initialize = function() {
        print("calling initialize")
        initialize_Cpublic() # step 1, for uncompiled or compiled
        if(isCompiled()) initializeCpp() # step 2, only for compiled
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
  expect_equal(obj$Ca, "numericScalar")
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


test_that("warning issued when Rpublic initialize lacks super$initialize or initialize_Cpublic", {
  # Should warn: no super$initialize or initialize_Cpublic
  expect_warning(
    nClass(
      classname = "warn_test",
      Rpublic = list(
        initialize = function() {
          self$Ra <- 1
        },
        Ra = 0
      ),
      Cpublic = list(
        Ca = 'numericScalar'
      )
    ),
    "super\\$initialize"
  )

  # Should NOT warn: has super$initialize
  expect_no_warning(
    nClass(
      classname = "no_warn_super",
      Rpublic = list(
        initialize = function(...) {
          super$initialize(...)
          self$Ra <- 1
        },
        Ra = 0
      ),
      Cpublic = list(
        Ca = 'numericScalar'
      )
    )
  )

  # Should NOT warn: has initialize_Cpublic (manual-control pattern)
  expect_no_warning(
    nClass(
      classname = "no_warn_init_cpublic",
      Rpublic = list(
        initialize = function() {
          initialize_Cpublic()
          if(isCompiled()) initializeCpp()
          self$Ra <- 1
        },
        Ra = 0
      ),
      Cpublic = list(
        Ca = 'numericScalar'
      ),
      compileInfo = list(omit_automatic_Cpp_construction = TRUE)
    )
  )

  # Should NOT warn: no Rpublic initialize at all
  expect_no_warning(
    nClass(
      classname = "no_warn_no_init",
      Rpublic = list(Ra = 0),
      Cpublic = list(Ca = 'numericScalar')
    )
  )
})

test_that("manual initialize OMITTED with hand-coded C++ initialization compiles but is correctly broken", {
  nc <- nClass(
    classname = "methods_test",
    Rpublic = list(
      Ra = 0,
      initialize = function() {
        print("calling initialize")
        super$initialize()
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
  expect_equal(obj$Ca, "numericScalar")
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

test_that("nClass returned from nFunction connects to correct C++ object when initialize has args before ...", {
  # This tests the fix where $new(CppObj = LOE) uses a named argument so that
  # CppObj ends up in ... rather than binding to a positional parameter.
  # Without the named CppObj, the LOE would bind to Ra_init and be dropped.
  nc <- nClass(
    classname = "return_test",
    Rpublic = list(
      Ra = 0,
      initialize = function(Ra_init = 0, ...) {
        super$initialize(...)   # CppObj = LOE flows through ... when returning from nFunction
        self$Ra <- Ra_init
      }
    ),
    Cpublic = list(
      Ca = 'numericScalar'
    )
  )

  nf <- nFunction(
    function() {
      obj <- nc$new()
      obj$Ca <- 42
      return(obj)
    },
    returnType = 'nc'
  )

  Cnc <- nCompile(nc, nf)
  # Normal user-facing construction: Ra_init is used, Ca should be default (0)
  user_obj <- Cnc$nc$new(Ra_init = 7)
  expect_equal(user_obj$Ra, 7)
  expect_equal(user_obj$Ca, 0)

  # Object returned from compiled nFunction: must be connected to the C++ object
  # that had Ca set to 42, NOT a freshly default-initialized one.
  returned_obj <- Cnc$nf()
  expect_equal(returned_obj$Ca, 42)

  rm(user_obj, returned_obj); gc()
})
