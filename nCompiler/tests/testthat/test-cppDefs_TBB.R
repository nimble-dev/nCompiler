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

## Set up an nFunction for purposes of having a body of a for loop
## to explore how to construct parallel_loop_body

forloop <- nFunction(
  fun = function(x = 'numericVector') {
    y <- x
    for(i in 1:10) {
      y[i] <- 2.* x[i]
    }
    return(y)
  },
  returnType = 'numericVector'
)
cpp_forloop <- nCompile_nFunction(forloop,
                                  control = list(endStage = 'makeCppDef'))
body_code <- cpp_forloop$cppDef$code$code
orig_loop_code <- body_code$args[[2]]$args[[1]]

debug(nCompiler:::cppParallelBodyClass_init_impl)
test <- nCompiler:::cppParallelBodyClass_init_impl(cppDef=NULL,
                                                   name = "parallel_loop_body",
                                                   orig_loop_code = orig_loop_code,
                                                   symbolTable = cpp_forloop$cppDef$code$symbolTable,
                                                   copyVars = c("ARG1_x_"),
                                                   noncopyVars = c("y"))
