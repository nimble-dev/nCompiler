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
                                           loop_body,
                                           loop_range,
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
    sym <- symbolTable$getSymbol(v)$clone()
    localSym <- sym$clone()
    sym$isRef <- TRUE
    sym$name <- paste0(oldSym$name, "_orig_")
    newSymTab$addSymbol(sym)
    if(!inherits(localSym, 'cppVarFullClass'))
      localSym <- cppVar2cppVarFull(localSym)
    localSym$constructor <- paste0("(", sym$name, ")")
    newLocalSymTab$addSymbol() ## put in local symbol table
  }
  for(v in noncopyVars) {
    sym <- symbolTable$getSymbol(v)$clone()
    sym$isRef <- TRUE
    newSymTab$addSymbol(sym)  
  }
  ## Create operator()
  browser()
  code <- loop_body ## add wrapping of for loop
  body_code_block <- cppCodeBlockClass$new(code = code,
                                           symbolTable = newLocalSymTab)
  cppDef$cppFunctionDefs
  ## create constructor
}