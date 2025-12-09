# Tests of nCompile's ability to automatically include needed units (nClasses and nFunctions)

# library(testthat)
# library(nCompiler)

# Rather than running all of the below tests in both non-package and
# package modes of compilation, I will alternate.

test_that("nFunction auto-including nFunction works and can be controlled", {
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  fn1 <- nFunction(
    function(x=double()) {return(x+1); returnType(double())}
  )
  nf2 <- nFunction(
    function(x=double()) {return(fn1(x)); returnType(double())}
  )
  comp <- nCompile(fn1, nf2)
  expect_equal(comp$fn2(1), 2)

  comp <- nCompile(nf2)
  expect_true(is.function(comp))
  expect_equal(comp(1), 2)

  expect_error(
    nCompile(nf2, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(nf2)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

})

test_that("nClass auto-including nFunction works and can be controlled", {
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  fn1 <- nFunction(
    function(x=double()) {return(x+1); returnType(double())}
  )
  nc2 <- nClass(
    Cpublic = list(
      fn2 = nFunction(
        function(x=double()) {return(fn1(x)); returnType(double())}
      )
    )
  )
  comp <- nCompile(fn1, nc2, package = TRUE)
  obj <- comp$nc2$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  comp <- nCompile(nc2)
  expect_true(inherits(comp, "R6ClassGenerator"))
  obj <- comp$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  expect_error(
    nCompile(nc2, package=TRUE, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(nc2, package=TRUE)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")
})

test_that("nClass auto-including nClass works and can be controlled", {
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  nc1 <- nClass(
    Cpublic = list(
      fn1 = nFunction(
        function(x=double()) {return(x+1); returnType(double())}
      )
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      mync1 = "nc1",
      fn2 = nFunction(
        function(x=double()) {
          mync1 <- nc1$new()
          return(mync1$fn1(x)); returnType(double())}
      )
    )
  )
  comp <- nCompile(nc2, nc1)
  obj <- comp$nc2$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  comp <- nCompile(nc2)
  expect_true(inherits(comp, "R6ClassGenerator"))
  obj <- comp$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  expect_error(
    nCompile(nc2, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(nc2)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")
})

# The next test is a very minor tweak and could perhaps be reduced in the future
test_that("nClass auto-including nClass works (non-member) and can be controlled", {
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  nc1 <- nClass(
    Cpublic = list(
      fn1 = nFunction(
        function(x=double()) {return(x+1); returnType(double())}
      )
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      #mync1 = "nc1",
      fn2 = nFunction(
        function(x=double()) {
          mync1 <- nc1$new() # local object only
          return(mync1$fn1(x)); returnType(double())}
      )
    )
  )
  comp <- nCompile(nc2, nc1, package=TRUE)
  obj <- comp$nc2$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  comp <- nCompile(nc2, package=TRUE)
  expect_true(inherits(comp, "R6ClassGenerator"))
  obj <- comp$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  expect_error(
    nCompile(nc2, package=TRUE, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(nc2, package=TRUE)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")
})


# The next test is a very minor tweak and could perhaps be reduced in the future
test_that("nClass auto-including nClass works (member only) and can be controlled", {
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  nc1 <- nClass(
    Cpublic = list(
      fn1 = nFunction(
        function(x=double()) {return(x+1); returnType(double())}
      )
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      mync1 = "nc1", # not used, needs still to be seed as needed
      fn2 = nFunction(
        function(x=double()) {
          return(x+1); returnType(double())}
      )
    )
  )
  comp <- nCompile(nc2, nc1)
  obj <- comp$nc2$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  comp <- nCompile(nc2)
  expect_true(inherits(comp, "R6ClassGenerator"))
  obj <- comp$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  expect_error(
    nCompile(nc2, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(nc2)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")
})

# The next test is a very minor tweak and could perhaps be reduced in the future
test_that("nClass auto-including nClass works (member only, in parent_env) and can be controlled", {
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  nc1 <- nClass(
    Cpublic = list(
      fn1 = nFunction(
        function(x=double()) {return(x+1); returnType(double())}
      )
    )
  )
  nc2 <- nClass(
    Cpublic = list(
      mync1 = "nc1", # not used, needs still to be seed as needed
      fn2 = nFunction(
        function(x=double()) {
          return(x+1); returnType(double())}
      )
    )
  )
  nc2$parent_env$nc1 <- nc1
  rm(nc1)
  comp <- nCompile(nc2, returnList=TRUE)
  obj <- comp$nc2$new()
  expect_equal(obj$fn2(1), 2)
  rm(obj); gc()

  expect_error(
    nCompile(nc2, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(nc2)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")
})

test_that("nFunction auto-including nClass works (non-member) and can be controlled", {
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  nc1 <- nClass(
    Cpublic = list(
      fn1 = nFunction(
        function(x=double()) {return(x+1); returnType(double())}
      )
    )
  )
  fn2 <- nFunction(
    function(x=double()) {
      mync1 <- nc1$new() # local object only
      return(mync1$fn1(x)); returnType(double())}
  )
  comp <- nCompile(fn2, nc1, package=TRUE)
  expect_equal(fn2(1), 2)

  comp <- nCompile(fn2, package=TRUE)
  expect_true(is.function(comp))
  expect_equal(fn2(1), 2)

  expect_error(
    nCompile(fn2, package=TRUE, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(fn2, package=TRUE)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")
})

test_that("auto-including from inherited nClass works and can be controlled", {
  # Adapted from test-nClass_inherit
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  ncBase <- nClass(
    classname = "ncBase",
    Cpublic = list(
      x = 'numericScalar',
      add_x = nFunction(function(v = 'numericScalar') {
        return(v + x); returnType('numericScalar');
      },
      name = "add_x"),
      add_2x_virt = nFunction(function(v = 'numericScalar') {
        return(v + 2*x); returnType('numericScalar');
      })
    ),
    compileInfo = list(interface = "none",createFromR=FALSE)
  )

  ncMid <- nClass(
    inherit = ncBase,
    classname = "ncMid",
    compileInfo = list(interface = "none",createFromR=FALSE),
    Cpublic = list(x2 = 'numericScalar')
  )

  ncDer <- nClass(
    inherit = ncMid,
    Cpublic = list(x3 = 'numericScalar')
  )

  ncUseBase <- nClass(
    classname = "ncUseBase",
    Cpublic = list(
      myBase = 'ncBase',
      call_add_x = nFunction(
        fun = function(v = 'numericScalar') {
          return(myBase$add_x(v)); returnType('numericScalar')
        }
      )
    )
  )

  comp <- nCompile(ncUseBase, ncBase, ncMid, ncDer)
  Cobj <- comp$ncDer$new()
  Cobj$x <- 10
  expect_equal(Cobj$add_x(15), 25)
  expect_equal(method(Cobj$private$CppObj, "add_x")(15), 25)
  expect_equal(Cobj$add_2x_virt(15), 35)
  Cobj2 <- comp$ncUseBase$new()
  expect_true(is.null(Cobj2$myBase))
  Cobj2$myBase <- Cobj
  expect_equal(Cobj2$call_add_x(15), 25)
  rm(Cobj, Cobj2); gc()

  comp <- nCompile(ncUseBase, ncDer, returnList=TRUE)
  Cobj <- comp$ncDer$new()
  Cobj$x <- 10
  expect_equal(Cobj$add_x(15), 25)
  expect_equal(method(Cobj$private$CppObj, "add_x")(15), 25)
  expect_equal(Cobj$add_2x_virt(15), 35)
  Cobj2 <- comp$ncUseBase$new()
  expect_true(is.null(Cobj2$myBase))
  Cobj2$myBase <- Cobj
  expect_equal(Cobj2$call_add_x(15), 25)
  rm(Cobj, Cobj2); gc()

  expect_error(
    nCompile(ncUseBase, ncDer, control=list(nCompile_include_units=FALSE))
  )

  set_nOption("nCompile_include_units", FALSE, "compilerOptions")
  expect_error(
    nCompile(ncUseBase, ncDer)
  )
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")
})

# modified from test-predefined
test_that("One predefined nFunction can use another via auto-include",
{
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

  ## It is recommended to provide an exportName.
  ## Otherwise, the built-in export name will be set
  ## when the predefined is generated, and that will
  ## not necessarily match when it is auto-included
  ## include the name happens to match the export name
  ## given when calling nCompile (ie. the name of the ... entry)
  foo <- nFunction(
    name = "test_predefined_foo_nF",
    function(x=double(1)) {return(x+1); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_foo_dir"),
    compileInfo = list(exportName = "foo_export")
  )
  bar <- nFunction(
    name = "test_predefined_bar_nF",
    function(x=double(1)) {return(foo(x+1)); returnType(double(1))},
    predefined=file.path(tempdir(), "test_predefined_bar_dir"),
    compileInfo=list(needed_units="foo")
  )
  dir <- file.path(tempdir(), "use_predefined_testdir")
  comp <- nCompile(bar, dir=dir, control=list(generate_predefined=TRUE), returnList=TRUE)
  expect_true(dir.exists(NFinternals(bar)$predefined))
  # auto-included unit does NOT get predefined files written.
  # It must be manually included in the nCompile call to write files.
  expect_false(dir.exists(NFinternals(foo)$predefined))
  expect_equal(comp$bar(1:3), 3:5)
  # now write the next one
  comp <- nCompile(foo, dir=dir, control=list(generate_predefined=TRUE), returnList=TRUE)
  expect_true(dir.exists(NFinternals(foo)$predefined))
  expect_true(names(comp)=="foo_export")
  expect_equal(comp$foo_export(1), 2)
  dir2 <- file.path(tempdir(), "use_predefined_testdir2")

  loading_output <- capture_output(comp2 <- nCompile(bar, dir=dir2, returnList=TRUE))

  text_matches <- gregexpr("Loading RcppPacket", loading_output)[[1]]
  expect_true(length(text_matches)==2)
  expect_equal(comp2$bar(1:3), 3:5)
  unlink(dir, recursive = TRUE)
  unlink(dir2, recursive = TRUE)
  unlink(NFinternals(foo)$predefined, recursive=TRUE)
  unlink(NFinternals(bar)$predefined, recursive=TRUE)
})

test_that("One predefined nClass can use another, separately and by inheritance, via auto-include",
{
  opt <- nOptions("compilerOptions")$nCompile_include_units
  on.exit(set_nOption("nCompile_include_units", opt, "compilerOptions"))
  set_nOption("nCompile_include_units", TRUE, "compilerOptions")

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
    , compileInfo=list(needed_units = "foo_base")
    )

    use_foo <- nClass(
      classname = "test_predefined_nC_usefoo",
      Cpublic = list(
        make_foo = nFunction(
          function() {return(foo$new()); returnType('foo')}
        )
      )
    , predefined=file.path(tempdir(), "test_predefined_nC_use_foo")
    , compileInfo=list(needed_units = "foo")
    )

    dir <- file.path(tempdir(), "use_predefined_nC_testdir2")

    comp <- nCompile(use_foo, dir=dir, control=list(generate_predefined=TRUE), package=package, returnList=TRUE)
    obj <- comp$use_foo$new()
    expect_equal(obj$make_foo()$bar(1:3), 2:4)
    dir2 <- file.path(tempdir(), "use_predefined_nC_testdir2")
    loading_output <- capture_output(comp2 <- nCompile(use_foo, dir=dir2,package=package, returnList=TRUE))
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
