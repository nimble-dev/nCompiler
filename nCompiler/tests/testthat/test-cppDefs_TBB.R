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
    )
  )
)

Cnc <- nCompile_nClass(nc, control = list(endStage = 'makeCppDef'))
class(Cnc)
