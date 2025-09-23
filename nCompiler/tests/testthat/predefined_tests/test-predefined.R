library(testthat)
library(nCompiler)

# nFunction tests
test_that("saving and loading RcppPacket works for nFunction",
{
  foo <- nFunction(
    name = "test_RcppPacket_foo",
    function(x=double(1)) {return(x+1); returnType(double(1))}
  )
  cppDef <- nCompile(foo, control=list(return_cppDefs=TRUE))
  RcppPacket <- nCompiler:::cppDefs_2_RcppPacket(cppDef[[1]])

  dir <- file.path(tempdir(), "test_RcppPacket_dir")

  dir.create(dir, showWarnings=FALSE, recursive=FALSE)
  nCompiler:::saveRcppPacket(RcppPacket, dir, "test_RcppPacket")
  restored_packet <- nCompiler:::loadRcppPacket(dir, "test_RcppPacket")
  expect_true(identical(RcppPacket, restored_packet))
  unlink(dir, recursive = TRUE)

  dir.create(dir, showWarnings=FALSE, recursive=FALSE)
  nCompiler:::saveRcppPacket(RcppPacket, dir)
  restored_packet <- nCompiler:::loadRcppPacket(dir)
  expect_true(identical(RcppPacket, restored_packet))
  unlink(dir, recursive = TRUE)
})

test_that("generating and compiling a predefined nFunction works",
{
  foo <- nFunction(
    name = "test_predefined_nFunction",
    function(x=double(1)) {return(x+1); returnType(double(1))},
    predefined=TRUE
  )
  expect_error(nCompile(foo))


  foo <- nFunction(
    name = "test_predefined_nFunction",
    function(x=double(1)) {return(x+1); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_nFunction_dir")
  )
  dir <- file.path(tempdir(), "use_predefined_testdir")
  cfoo <- nCompile(foo, dir=dir, control=list(generate_predefined=TRUE))
  expect_equal(cfoo(1:3), 2:4)
  dir2 <- file.path(tempdir(), "use_predefined_testdir2")
  loading_output <- capture_output(cfoo2 <- nCompile(foo, dir=dir2))
  expect_true(grepl("^Loading RcppPacket", loading_output))
  expect_equal(cfoo2(1:3), 2:4)
  unlink(dir, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(NFinternals(foo)$predefined, recursive=TRUE)
})

test_that("generating and compiling a predefined nFunction works via packaging",
{
  foo <- nFunction(
    name = "test_predefined_nFunction",
    function(x=double(1)) {return(x+1); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_nFunction_dir")
  )
  dir <- file.path(tempdir(), "use_predefined_testdir")
  cfoo <- nCompile(foo, dir=dir, control=list(generate_predefined=TRUE), package=TRUE)
  expect_equal(cfoo(1:3), 2:4)
  dir2 <- file.path(tempdir(), "use_predefined_testdir2")
  loading_output <- capture_output(cfoo2 <- nCompile(foo, dir=dir2, package=TRUE))
  expect_true(grepl("^Loading RcppPacket", loading_output))
  expect_equal(cfoo2(1:3), 2:4)
  unlink(dir, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(NFinternals(foo)$predefined, recursive=TRUE)
})

test_that("One predefined nFunctions can use another predefined",
{
  foo <- nFunction(
    name = "test_predefined_foo_nF",
    function(x=double(1)) {return(x+1); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_foo_dir")
  )
  bar <- nFunction(
    name = "test_predefined_bar_nF",
    function(x=double(1)) {return(foo(x+1)); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_bar_dir")
  )
  dir <- file.path(tempdir(), "use_predefined_testdir")
  comp <- nCompile(foo, bar, dir=dir, control=list(generate_predefined=TRUE))
  expect_equal(comp$bar(1:3), 3:5)
  dir2 <- file.path(tempdir(), "use_predefined_testdir2")
  loading_output <- capture_output(comp2 <- nCompile(foo, bar, dir=dir2))
  text_matches <- gregexpr("Loading RcppPacket", loading_output)[[1]]
  expect_true(length(text_matches)==2)
  expect_equal(comp2$bar(1:3), 3:5)
  unlink(dir, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(NFinternals(foo)$predefined, recursive=TRUE)
})

test_that("One predefined nFunctions can use another predefined via packaging",
{
  foo <- nFunction(
    name = "test_predefined_foo_nF",
    function(x=double(1)) {return(x+1); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_foo_dir")
  )
  bar <- nFunction(
    name = "test_predefined_bar_nF",
    function(x=double(1)) {return(foo(x+1)); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_bar_dir")
  )
  dir <- file.path(tempdir(), "use_predefined_testdir")
  comp <- nCompile(foo, bar, dir=dir, control=list(generate_predefined=TRUE), package=TRUE)
  expect_equal(comp$bar(1:3), 3:5)
  dir2 <- file.path(tempdir(), "use_predefined_testdir2")
  loading_output <- capture_output(comp2 <- nCompile(foo, bar, dir=dir2, package=TRUE))
  text_matches <- gregexpr("Loading RcppPacket", loading_output)[[1]]
  expect_true(length(text_matches)==2)
  expect_equal(comp2$bar(1:3), 3:5)
  unlink(dir, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(NFinternals(foo)$predefined, recursive=TRUE)
})

############
# nClass tests
test_that("saving and loading RcppPacket works for nClass",
{
  foo <- nClass(
    classname = "test_RcppPacket_foo_nC",
    Cpublic = list(
      bar = nFunction(
        function(x=double(1)) {return(x+1); returnType(double(1))}
      )
    )
  )
  cppDef <- nCompile(foo, control=list(return_cppDefs=TRUE))
  RcppPacket <- nCompiler:::cppDefs_2_RcppPacket(cppDef[[1]])

  dir <- file.path(tempdir(), "test_RcppPacket_dir")

  dir.create(dir, showWarnings=FALSE, recursive=FALSE)
  nCompiler:::saveRcppPacket(RcppPacket, dir, "test_RcppPacket")
  restored_packet <- nCompiler:::loadRcppPacket(dir, "test_RcppPacket")
  expect_true(identical(RcppPacket, restored_packet))
  unlink(dir, recursive = TRUE)

  dir.create(dir, showWarnings=FALSE, recursive=FALSE)
  nCompiler:::saveRcppPacket(RcppPacket, dir)
  restored_packet <- nCompiler:::loadRcppPacket(dir)
  expect_true(identical(RcppPacket, restored_packet))
  unlink(dir, recursive = TRUE)
})

cat("add unload of DLLs to test-predefined\n")

test_that("generating and compiling a predefined nClass works",
{
  foo <- nClass(
    classname = "test_predefined_nC",
    Cpublic = list(
      bar = nFunction(
        function(x=double(1)) {return(x+1); returnType(double(1))}
      )
    ),
    predefined=TRUE
  )
  expect_error(nCompile(foo))

  foo <- nClass(
    classname = "test_predefined_nC",
    Cpublic = list(
      bar = nFunction(
        function(x=double(1)) {return(x+1); returnType(double(1))}
      )
    ),
    predefined=file.path(tempdir(), "test_predefined_nC_dir")
  )

  dir <- file.path(tempdir(), "use_predefined_nC_testdir")
  cfoo <- nCompile(foo, dir=dir, control=list(generate_predefined=TRUE))
  obj <- cfoo$new()
  expect_equal(obj$bar(1:3), 2:4)
  dir2 <- file.path(tempdir(), "use_predefined_nC_testdir2")
  loading_output <- capture_output(cfoo2 <- nCompile(foo, dir=dir2))
  obj2 <- cfoo2$new()
  expect_true(grepl("^Loading RcppPacket", loading_output))
  expect_equal(obj2$bar(1:3), 2:4)
  rm(obj, obj2); gc()
  unlink(dir, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(NCinternals(foo)$predefined, recursive=TRUE)
})

test_that("generating and compiling a predefined nClass works through packaging",
{
  foo <- nClass(
    classname = "test_predefined_nC",
    Cpublic = list(
      bar = nFunction(
        function(x=double(1)) {return(x+1); returnType(double(1))}
      )
    ),
    predefined=file.path(tempdir(), "test_predefined_nC_dir")
  )

  dir <- file.path(tempdir(), "use_predefined_nC_testdir")
  cfoo <- nCompile(foo, dir=dir, control=list(generate_predefined=TRUE), package=TRUE)
  obj <- cfoo$new()
  expect_equal(obj$bar(1:3), 2:4)
  dir2 <- file.path(tempdir(), "use_predefined_nC_testdir2")
  loading_output <- capture_output(cfoo2 <- nCompile(foo, dir=dir2, package=TRUE))
  obj2 <- cfoo2$new()
  expect_true(grepl("^Loading RcppPacket", loading_output))
  expect_equal(obj2$bar(1:3), 2:4)
  rm(obj, obj2); gc()
  unlink(dir, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(NCinternals(foo)$predefined, recursive=TRUE)
})

cat("type declaration in code of returnType(foo()) needs fixing\n")

test_that("One predefined nClass can use another, separately and by inheritance",
{
  for(package in c(FALSE, TRUE)) {
    foo_base <- nClass(
      classname = "test_predefined_nC_foo_base",
      Cpublic = list(
        give_one = nFunction(
          function() {
            return(1.0); returnType(double())
          }
        )
      )
    , compileInfo = list(interface='none', createFromR = FALSE)
    , predefined=file.path(tempdir(), "test_predefined_nC_foo_base_dir")
    )

    foo <- nClass(
      classname = "test_predefined_nC_foo",
      inherit = foo_base,
      Cpublic = list(
        bar = nFunction(
          function(x=double(1)) {return(x+1); returnType(double(1))}
        )
      )
    , predefined=file.path(tempdir(), "test_predefined_nC_foo_dir")
    )

    use_foo <- nClass(
      classname = "test_predefined_nC_usefoo",
      Cpublic = list(
        make_foo = nFunction(
          function() {return(foo$new()); returnType('foo')}
        )
      )
    , predefined=file.path(tempdir(), "test_predefined_nC_use_foo")
    )

    dir <- file.path(tempdir(), "use_predefined_nC_testdir2")

    comp <- nCompile(foo, foo_base, use_foo, dir=dir, control=list(generate_predefined=TRUE),package=package)
    obj <- comp$use_foo$new()
    expect_equal(obj$make_foo()$bar(1:3), 2:4)
    dir2 <- file.path(tempdir(), "use_predefined_nC_testdir2")
    loading_output <- capture_output(comp2 <- nCompile(foo, foo_base, use_foo, dir=dir2,package=package))
    obj2 <- comp2$use_foo$new()
    expect_true(grepl("^Loading RcppPacket", loading_output))
    expect_equal(obj2$make_foo()$bar(1:3), 2:4)
    rm(obj, obj2); gc()
    unlink(dir, recursive = TRUE)
    unlink(dir2, recursive = TRUE)
    unlink(NCinternals(foo)$predefined, recursive=TRUE)
    unlink(NCinternals(foo_base)$predefined, recursive=TRUE)
    unlink(NCinternals(use_foo)$predefined, recursive=TRUE)
  }
})
