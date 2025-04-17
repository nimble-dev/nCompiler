library(nCompiler)
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
