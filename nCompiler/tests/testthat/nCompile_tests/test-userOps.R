# test user-defined operator definitions
# These can be:
# 1. provided globally to define (or take over) a new keyword via registerOpDef
#    - precedence goes to a user-defined opDef over a built-in one
# 2. provided as an opDef list to an nFunction
# 3. provided as an opDef list (within compileInfo) to an nFunction that is an nClass method
# 4. provided as a list of opDef lists (within compileInfo) to an nClass

# The definition "closest" to the nFunction takes precedence.
#   e.g. if an nFunction has an opDef list, that takes precedence over
#     an opDef list provided to the nClass that contains the nFunction.

test_that("registering a global user-defined operator definition (opDef) works", {
  ## first version: provide a function
  nimArrayHandler <- function(code,...) {
    code$name <- 'nArray'
    NULL
  }
  # This test works by:
  # providing a handler to relpace "nimArray" with "nArray"
  # and a handler to replace "nimArray2" with "nArray" to
  # check on handling multiple cases.
  registerOpDef(
    list(nimArray =
           list(
             matchDef = function(value=0, dim=c(1,1), init=TRUE,
                                 fillZeros=TRUE, recycle=TRUE, nDim,
                                 type="double") {},
             simpleTransformations=list(handler = nimArrayHandler))))
  expect_equal(ls(`:::`("nCompiler", "operatorDefUserEnv")), "nimArray")

  registerOpDef(
    list(nimArray2 =
           list(
             matchDef = function(value=0, dim=c(1,1), init=TRUE,
                                 fillZeros=TRUE, recycle=TRUE, nDim,
                                 type="double") {},
             simpleTransformations=list(handler = 'replace',
                                        replacement = 'nArray'))))
  expect_equal(ls(`:::`("nCompiler", "operatorDefUserEnv")), c("nimArray", "nimArray2"))

  nc <- nClass(
    Cpublic = list(
      foo = nFunction(
        function() {
          ans <- nimArray( 6, dim = 2)
          ans2 <- nArray(value = 5, dim = 2)
          return(ans)
          returnType('double(1)')
        }
      ),
      foo2 = nFunction(
        function() {
          ans <- nimArray2(3,dim = 2)
          return(ans)
          returnType('double(1)')
        })
    ))
  Cnc <- nCompile(nc)
  obj <- Cnc$new()
  expect_identical(obj$foo(), c(6, 6))
  expect_identical(obj$foo2(), c(3, 3))
  rm(obj); gc()
  #
  deregisterOpDef("nimArray")
  deregisterOpDef("nimArray2")
  expect_equal(length(ls(`:::`("nCompiler", "operatorDefUserEnv"))), 0)
})

cat("User opDef could be dangerous prior to genCpp because it won't update cachedOpDef\n")

test_that("nFunction custom opDef works through a sequence of changes and handlers", {
  # We set up a series of messages and function renamings to track
  # the custom opDef through each one and also
  # show that it is being updated at each step.
  # Note the manual updating during eigenImpl.
  #
  # foo is an nFunction with compileArgs and a simpleTrans handler that
  # renames it foo2 and emits a msg.
  check_V <- function(code, ...) {
    cat("MSG1: check_V was called. ")
    if(code$aux$compileArgs$V == "W")
      cat("MSG2: compile arg V was found. ")
    code$name <- "foo2"
  }
  custom_opDef <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef$matchDef <- function(V) {}
  custom_opDef$compileArgs <- "V"
  custom_opDef$simpleTransformations <- list(
    handler = check_V
  )
  foo <- nFunction(
    fun = function() {
      return(1.2)
    }, argTypes=list(), returnType=quote(double()),
    compileInfo=list(opDef=custom_opDef)
  )

  # foo2 is an nFunction with custom labelAbstractTypes handler
  # that emits a msg and renames it to foo3.
  check_LAT <- function(code, symTab, auxEnv, handlingInfo) {
    cat("MSG3: LAT for foo2 was used. ")
    handler <- nCompiler:::getOperatorDef("nFunction_default")$labelAbstractTypes$handler
    ans <- eval(call(handler, code,symTab,auxEnv,handlingInfo),
                envir=nCompiler:::labelAbstractTypesEnv)
    code$name <- "foo3"
    ans
  }
  custom_opDef2 <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef2$labelAbstractTypes <- list(
    handler = check_LAT
  )
  foo2 <- nFunction(
    fun = function() {
      return(2.3)
    }, argTypes=list(), returnType=quote(double()),
    compileInfo=list(opDef=custom_opDef2)
  )

  # foo3 is an nFunction with a custom eigenImpl handler
  # that emits, a msg, renames it, and updated the cachedOpInfo
  check_EIG <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    cat("MSG4: LAT for foo3 was used. ")
    code$name <- "foo4"
    nCompiler:::update_cachedOpInfo(code, auxEnv$where)
    NULL
  }
  custom_opDef3 <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef3$eigenImpl <- list(
    handler = check_EIG
  )
  foo3 <- nFunction(
    fun = function() {
      return(3.4)
    }, argTypes=list(), returnType=quote(double()),
    compileInfo=list(opDef=custom_opDef3)
  )

  ## foo4 is an nFunction with a custom cppOutput handler
  # that emits a msg and pastes "10.0*3.4 + " in front of the
  # default output.
  check_GenCpp <- function(code, symTab) {
#    cat("MSG5: GenCpp for foo4 was used.")
    handler <- nCompiler:::getOperatorDef("nFunction_default")$cppOutput$handler
    base_out <- eval(call(handler, code, symTab),
                     envir=nCompiler:::genCppEnv)
    paste0("10.0*3.4 + ", base_out, collapse = "")
  }
  custom_opDef4 <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef4$cppOutput <- list(
    handler = check_GenCpp
  )
  foo4 <- nFunction(
    fun = function() {
      return(4.5)
    }, argTypes=list(), returnType=quote(double()),
    compileInfo=list(opDef=custom_opDef4)
  )

  call_foo <- nFunction(
    fun = function() {
      ans <- foo(V = "W"); return(ans)
    }, argTypes=list(), returnType=quote(double())
  )
  out <- capture_output(
    cppDefs <- nCompile(call_foo, control=list(return_cppDefs=TRUE))
  )
  check <- grepl("^MSG1", out)
  expect_true(check)
  check <- grepl("MSG2", out)
  expect_true(check)
  check <- grepl("MSG3", out)
  expect_true(check)
  check <- grepl("MSG4", out)
  expect_true(check)

  out_code <- cppDefs[[1]]$generate() |> unlist()
  check <- grepl("10\\.0\\*3\\.4", out_code) |> sum()
  expect_true(check==1)

  cat("#include needs are not cleaned up if an nFunction is renamed by a handler to another nFunction. All must be included in nCompile.")
  comp <- nCompile(foo, foo2, foo3, foo4, call_foo)
  expect_equal(comp$call_foo(), 10*3.4 + 4.5)

})

test_that("nClass custom opDefs of 3 kinds works through a sequence of changes and handlers", {

  # foo is an nFunction with compileArgs and a simpleTrans handler that
  # renames it foo2 and emits a msg.
  check_V <- function(code, ...) {
    cat("MSG1: check_V was called. ")
    if(code$aux$compileArgs$V == "W")
      cat("MSG2: compile arg V was found. ")
    code$name <- "foo2"
  }
  custom_opDef <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef$matchDef <- function(V) {}
  custom_opDef$compileArgs <- "V"
  custom_opDef$simpleTransformations <- list(
    handler = check_V
  )
  foo <- nFunction(
    fun = function() {
      return(1.2)
    }, argTypes=list(), returnType=quote(double()),
    compileInfo=list(opDef=custom_opDef)
  )

  # foo2 is an nFunction with custom labelAbstractTypes handler
  # that emits a msg and renames it to foo3.
  # It's handler will be put at the nClass level
  check_LAT <- function(code, symTab, auxEnv, handlingInfo) {
    cat("MSG3: LAT for foo2 was used. ")
    handler <- nCompiler:::getOperatorDef("nFunction_default")$labelAbstractTypes$handler
    ans <- eval(call(handler, code,symTab,auxEnv,handlingInfo),
                envir=nCompiler:::labelAbstractTypesEnv)
    code$name <- "foo3"
    ans
  }
  custom_opDef2 <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef2$labelAbstractTypes <- list(
    handler = check_LAT
  )
  foo2 <- nFunction(
    fun = function() {
      return(2.3)
    }, argTypes=list(), returnType=quote(double()),
#    compileInfo=list(opDef=custom_opDef2)
  )

  # foo3 is an nFunction with a custom eigenImpl handler
  # that emits, a msg, renames it, and updated the cachedOpInfo
  # This will be treated as a keyword opDef, only, without an
  # actual nFunction.
  check_EIG <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    cat("MSG4: LAT for foo3 was used. ")
    code$name <- "foo4"
    nCompiler:::update_cachedOpInfo(code, auxEnv$where)
    NULL
  }
  custom_opDef3 <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef3$eigenImpl <- list(
    handler = check_EIG
  )
#   foo3 <- nFunction(
#     fun = function() {
#       return(3.4)
#     }, argTypes=list(), returnType=quote(double()),
#     compileInfo=list(opDef=custom_opDef3)
#   )

  ## foo4 is an nFunction with a custom cppOutput handler
  # that emits a msg and pastes "10.0*3.4 + " in front of the
  # default output.
  # This will remain an nFunction outside the nClass.
  check_GenCpp <- function(code, symTab) {
#    cat("MSG5: GenCpp for foo4 was used.")
    handler <- nCompiler:::getOperatorDef("nFunction_default")$cppOutput$handler
    base_out <- eval(call(handler, code, symTab),
                     envir=nCompiler:::genCppEnv)
    paste0("10.0*3.4 + ", base_out, collapse = "")
  }
  custom_opDef4 <- nCompiler:::getOperatorDef("nFunction_default")
  custom_opDef4$cppOutput <- list(
    handler = check_GenCpp
  )
  foo4 <- nFunction(
    fun = function() {
      return(4.5)
    }, argTypes=list(), returnType=quote(double()),
    compileInfo=list(opDef=custom_opDef4)
  )

  call_foo <- nFunction(
      fun = function() {
          ans <- foo(V = "W"); return(ans)
      }, argTypes=list(), returnType=quote(double())
  )

  foo_class <- nClass(
    Cpublic = list(
      foo = foo,
      foo2 = foo2,
      call_foo = call_foo
    ),
    compileInfo = list(
      opDefs = list(
        foo2 = custom_opDef2,
        foo3 = custom_opDef3
      )
    )
  )

  out <- capture_output(
    cppDefs <- nCompile(foo_class, control=list(return_cppDefs=TRUE))
  )
  check <- grepl("^MSG1", out)
  expect_true(check)
  check <- grepl("MSG2", out)
  expect_true(check)
  check <- grepl("MSG3", out)
  expect_true(check)
  check <- grepl("MSG4", out)
  expect_true(check)

  out_code <- cppDefs[[1]]$generate() |> unlist()
  check <- grepl("10\\.0\\*3\\.4", out_code) |> sum()
  expect_true(check==1)

  comp <- nCompile(foo4, foo_class)
  obj <- comp$foo_class$new()
  expect_equal(obj$call_foo(), 10*3.4 + 4.5)
  rm(obj); gc()
})
