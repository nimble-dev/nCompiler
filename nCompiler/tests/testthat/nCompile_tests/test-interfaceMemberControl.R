library(nCompiler)
library(testthat)

test_that("compileInfo$interfaceInclude and interfaceExclude work", {
  nc <- nClass(
    Cpublic = list(
      a = 'numericScalar',
      b = 'numericScalar',
      foo = nFunction(
        function(){}
      ),
      foo2 = nFunction(
        function() {}
      )
    ),
    compileInfo = list(interfaceInclude = c('a','foo'))
  )
  cppDef <- nCompile(nc, control = list(return_cppDefs = TRUE))
  RcppPacket <- nCompiler:::cppDefs_2_RcppPacket(cppDef[[1]], "filename")
  expect_true(grepl('\"a\"',RcppPacket$cppContent$body)|>sum() == 1)
  expect_true(grepl('\"b\"',RcppPacket$cppContent$body)|>sum() == 0)
  expect_true(grepl('\"foo\"',RcppPacket$cppContent$body)|>sum() == 1)
  expect_true(grepl('\"foo2\"',RcppPacket$cppContent$body)|>sum() == 0)

  nc <- nClass(
    Cpublic = list(
      a = 'numericScalar',
      b = 'numericScalar',
      foo = nFunction(
        function(){}
      ),
      foo2 = nFunction(
        function() {}
      )
    ),
    compileInfo = list(interfaceExclude = c('b','foo2'))
  )
  cppDef <- nCompile(nc, control = list(return_cppDefs = TRUE))
  RcppPacket <- nCompiler:::cppDefs_2_RcppPacket(cppDef[[1]], "filename")
  expect_true(grepl('\"a\"',RcppPacket$cppContent$body)|>sum() == 1)
  expect_true(grepl('\"b\"',RcppPacket$cppContent$body)|>sum() == 0)
  expect_true(grepl('\"foo\"',RcppPacket$cppContent$body)|>sum() == 1)
  expect_true(grepl('\"foo2\"',RcppPacket$cppContent$body)|>sum() == 0)

  nc <- nClass(
    Cpublic = list(
      a = 'numericScalar',
      b = 'numericScalar',
      foo = nFunction(
        function(){}
      ),
      foo2 = nFunction(
        function() {}
      )
    ),
    compileInfo = list(interfaceExclude = c('b','foo2'),
                       interfaceInclude = c('a', 'foo'))
  )
  cat("Expecting error that interfaceExclude and interfaceInclude cannot both be non-null.\n")
  expect_error(cppDef <- nCompile(nc, control = list(return_cppDefs = TRUE)))
})
