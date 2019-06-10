# not working
context("cppDefs for TBB")

## cppFunctionClass::constructorContent
## should be a list of cppCodeBlockClass objects 
## with symbolTable set to the function symbolTable.

nc <- nClass(
  Cpublic = list(
    z = 'numericScalar',
    go = nFunction(
      fun = function(x = 'numericScalar') {
        ans <- x + 1
        return(ans)
      },
      returnType = 'numericScalar'
    ),
    c = nConstructor(
      fun = function(){},
      initializerList = list(quote(x(x_)))
    )
  )
)

Cnc <- nCompile_nClass(nc, control = list(endStage = 'makeCppDef'))
class(Cnc)
Cnc$cppFunctionDefs[['constructor_']] <- 
Cnc$initializerContent <- lapply(list(quote(x(x_)), quote(y(y_))), nParse)
Cnc$generate()

initCode <- quote(x(x_))
ninitCode <- nParse(initCode)
nCompiler:::compile_generateCpp(ninitCode, Cnc$symbolTable)

initializerContent <- list(quote(x(x_)))
initializerContent <- lapply(initializerContent, nParse)
initializers <- lapply(initializerContent, nCompiler:::compile_generateCpp)
result <- paste0(':\n',
                 paste(initializers, collapse = ",\n"))
result

