context("Testing NF_CompilerClass steps")

test_that("NF_CompilerClass steps",
{

  ## hello world
  foo <- nFunction(
    fun = function(a = numericScalar()) {
      return(a + 1)
      returnType(numericScalar())
    }
  )
  NFC <- nCompiler:::NF_CompilerClass$new(f = foo)
  expect_equal(NFC$origRcode,
               quote({
                 return(a+1)
               })
               )
  NFC$setupSymbolTable()
  expect_identical(NFC$symbolTable$getSymbolNames(),
                   "a")
  expect_identical(NFinternals(foo)$argSymTab$getSymbolNames(),
                   "a")
  ## check that symbols have the same content...
  ## ... but are actually different objects, so a clone was made
  
  ## Go through compiler stages
  ## 1. setInputOutputTypes
  expect_equal(NFC$initialTypeInferenceDone,
               FALSE)
  NFC$process(
    control = list(
      startStage = 1,
      endStage = 1
    )
  )
  expect_equal(NFC$initialTypeInferenceDone,
               TRUE)

  ## 2. substituteMangledArgumentNames
  expect_equal(all.vars(NFC$newRcode),
               "a")
  NFC$process(
    control = list(
      startStage = 2,
      endStage = 2
    )
  )
  expect_equal(
    all.vars(NFC$newRcode)
   ,
    "a"
  )

  ## 3. initializeCode
  expect_true(is.null(NFC$code))
  NFC$process(
    control = list(
      startStage = 3,
      endStage = 3
    )
  )
  expect_false(is.null(NFC$code))
  expect_true(inherits(NFC$code, 'exprClass'))
  expect_equal(nDeparse(NFC$code, toR = TRUE),
               NFC$newRcode)

  ## 4. simpleTransformations
  NFC$process(
    control = list(
      startStage = 4,
      endStage = 4
    )
  )
  ## Nothing really to test; This step is mostly empty.

  ## 5. simpleIntermediates
  NFC$process(
    control = list(
      startStage = 5,
      endStage = 5
    )
  )
  ## Nothing interesting to test here either
  
  ## 6. initializeAuxiliaryEnvironment    
  NFC$process(
    control = list(
      startStage = 6,
      endStage = 6
    )
  )
  expect_true(is.list(NFC$auxEnv[['needed_nFunctions']]))

  cat("\nCompiler steps only tested through stage 6\n")
  
  ## 7. labelAbstractTypes
  NFC$process(
    control = list(
      startStage = 7,
      endStage = 7
    )
  )
  
})
