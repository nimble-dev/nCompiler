#library(nCompiler)
#library(testthat)

test_that("nClass destructor works", {
  # It does not work to name a method finalize
  # because we'd need to hold it out of the interface class.
  nc <- nClass(
    classname = "dtor_test",
    Cpublic = list(
      hw = nFunction(function() {nCpp('Rprintf("hello world\\n");')}),
      destructor = nFunction(function() {}, compileInfo = list(destructor=TRUE))
    )
  )
  cppDef <- nCompile(nc, control=list(return_cppDefs=TRUE))
  out <- capture.output(writeCode(cppDef[[1]]$generate(TRUE)))
  expect_true(sum(grepl(" ~dtor_test", out))==1)
  out <- capture.output(writeCode(cppDef[[1]]$generate()))
  expect_true(sum(grepl("dtor_test::~dtor_test", out))==1)
  expect_no_error(test <- nCompile(nc))
  obj <- test$new()
  rm(obj)
  gc()
})
