# Test new symbols provided by a user.
# These can include overloadDefs.

test_that("user type works", {
  mySymbol_Bracket_LAT <- function(code, symTab, auxEnv, handlingInfo) {
    cat("MSG1\n")
    nCompiler:::labelAbstractTypesEnv$recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    code$type <- nCompiler:::type2symbol("numericScalar")
  }
  mySymbol_Bracket_EIG <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    cat("MSG2\n")
    nCompiler:::eigenizeEnv$eigenCast(code, 2, "integer")
    NULL
  }

  # This example demonstrates how to create a simple double*.

  mySymbolClass <- R6::R6Class(
    classname = "mysymbolclass",
    inherit = nCompiler:::symbolBase,
    public = list(
      initialize = function(...) {
        super$initialize(type = 'mySymbolClass', ...)
        self$overloadDefs <- list(
          "[" = list(
            labelAbstractTypes = list(
              handler = mySymbol_Bracket_LAT
            ),
            eigenImpl = list(
              handler = mySymbol_Bracket_EIG
            ),
            cppOutput = list(
              handler = nCompiler:::genCppEnv$IndexingBracket
            )
          )
        )
      },
      shortPrint = function() "mySymbolClass",
      uniqueID = function() "mySymbolClass",
      print = function() writeLines(paste0(self$name, ": mySymbolClass")),
      genCppVar = function() {
        nCompiler:::cppVarFullClass$new(baseType = "double",
                                        name = self$name,
                                        ptr = TRUE,
                                        ref = FALSE)
      }
    )
  )

  sym_handler <- function(...) {
    mySymbolClass$new()
  }
  nCompiler:::registerTypeDeclaration("mySym", sym_handler)
  on.exit(nCompiler:::deregisterTypeDeclaration("mySym"))

  obj <- nCompiler:::type2symbol("mySym")
  expect_true(inherits(obj, "mysymbolclass"))
  expect_true(R6::is.R6(obj))
  obj <- nCompiler:::type2symbol("mySym()")
  expect_true(inherits(obj, "mysymbolclass"))
  expect_true(R6::is.R6(obj))

  foo <- nFunction(
    function(x = 'numericVector') {
      nCpp("xptr = &x[0];", type = list(xptr = "mySym"))
      x2 <- xptr[2] + 2
      return(x2)
      returnType(double())
    }
  )

  output <- capture_output(cfoo <- nCompile(foo))
  expect_true(grepl("MSG1", output))
  expect_true(grepl("MSG2", output))
  expect_equal(cfoo(3:4), 6)

  nCompiler:::deregisterTypeDeclaration("mySym")
  obj <- nCompiler:::type2symbol("mySym")
  expect_true(inherits(obj, "symbolTBD"))

  cat("A\n")

  # provide the type as a symbol
  foo2 <- nFunction(
    function(x = 'numericVector') {
      nCpp("xptr = &x[0];", type = list(xptr = mySymbolClass$new()))
      x2 <- xptr[2] + 2
      return(x2)
      returnType(double())
    }
  )
  output <- capture_output(cfoo2 <- nCompile(foo2))
  expect_true(grepl("MSG1", output))
  expect_true(grepl("MSG2", output))
  expect_equal(cfoo2(3:4), 6)

  cat("B\n")

  # provide the type as a symbol in two steps
  mySymType <- mySymbolClass$new()
  foo3 <- nFunction(
    function(x = 'numericVector') {
      nCpp("xptr = &x[0];", type = list(xptr = "T(mySymType)"))
      x2 <- xptr[2] + 2
      return(x2)
      returnType(double())
    }
  )
  output <- capture_output(cfoo3 <- nCompile(foo3))
  expect_true(grepl("MSG1", output))
  expect_true(grepl("MSG2", output))
  expect_equal(cfoo3(3:4), 6)
})
