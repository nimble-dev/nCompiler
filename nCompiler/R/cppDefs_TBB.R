# not working
## cppDefs for parallel loop bodies for TBB

cppParallelBodyClass <- R6::R6Class(
  'cppParallelBodyClass',
  inherit = cppClassClass,
  portable = FALSE,
  public = list(
    initialize = function(loop_body, 
                          loop_range,
                          symbolTable,
                          copyVars = character(),
                          noncopyVars = character()) {
      cppParallelBodyClass_init_impl(self,
                                     loop_body,
                                     loop_range,
                                     symbolTable,
                                     copyVars,
                                     noncopyVars)
    }
  )
)

cppParallelBodyClass_init_impl <- function(cppDef,
                                           orig_loop_code = orig_loop_code,
                                           loop_body = orig_loop_code$args[[3]],
                                           loop_range = orig_loop_code$args[[2]],
                                           loop_var = orig_loop_code$args[[1]],
                                           symbolTable,
                                           copyVars,
                                           noncopyVars) {
  ## 1. Create symbolTable for copyVars + noncopyVars
  ## 2. Create operator()
  ## 3. Create constructor
  #####
  ## Create symbolTable. Assume we are using cpp symbol tables
  newSymTab <- symbolTableClass$new()
  newLocalSymTab <- symbolTableClass$new()
  for(v in copyVars) {
    sym <- symbolTable$getSymbol(v, inherits = TRUE)$clone()
    localSym <- sym$clone()
    sym$ref <- TRUE
    sym$name <- paste0(sym$name, "_orig_")
    newSymTab$addSymbol(sym)
    if(!inherits(localSym, 'cppVarFullClass'))
      localSym <- cppVar2cppVarFull(localSym)
    localSym$constructor <- paste0("(", sym$name, ")")
    newLocalSymTab$addSymbol(localSym) ## put in local symbol table
  }
  for(v in noncopyVars) {
    sym <- symbolTable$getSymbol(v, inherits = TRUE)$clone()
    sym$ref <- TRUE
    newSymTab$addSymbol(sym)  
  }
  ## Create operator()
  browser()
  generalForExpr <- exprClass$new(name = 'GeneralFor', isCall = TRUE,
                                  isName = FALSE, isAssign = FALSE, isLiteral = FALSE)
  indexName <- loop_var$name
  setArg(generalForExpr, 1, nParse(quote(cppLiteral(paste(indexName,"= r__.begin()")))))
  setArg(generalForExpr, 2, nParse(quote(cppLiteral(paste(indexName,"!= r__.end()")))))
  setArg(generalForExpr, 3, nParse(quote(cppLiteral(paste0("++", indexName)))))
  setArg(generalForExpr, 4, loop_body)
  body_code_block <- cppCodeBlockClass$new(code = generalForExpr,
                                           symbolTable = newLocalSymTab)
  argSymTab <- symbolTableClass$new()
  argSymTab$addSymbol(cppVarFullClass$new(name = 'r__',
                                          baseType = "blocked_range<int>",
                                          ref = TRUE, 
                                          const = TRUE))
  `operator()` <- cppFunctionClass$new(name = "operator()", 
                                       args = argSymTab, 
                                       code = body_code_block,
                                       const = TRUE,
                                       returnType = cppVoid())
  ## create constructor
#  argSymTab ## Should take by reference all copy and noncopy vars
#  initializerList ## Should include initializer code for each one
#  emptycode ## Should be an empty code block
#  emptyLocalSymTab ## should be an empty local symbolTable
#  constructor <- cppFunctionClass$new(name = 'parallel_loop_body' ##FIXME
#                                      )
}
#environment(cppParallelBodyClass_init_impl) <- environment(nCompiler::isNF)
