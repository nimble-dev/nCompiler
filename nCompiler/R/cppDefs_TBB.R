# not working
## cppDefs for parallel loop bodies for TBB

cppParallelBodyClass <- R6::R6Class(
  'cppParallelBodyClass',
  inherit = cppClassClass,
  portable = FALSE,
  public = list(
    initialize = function(loop_body,
                          loop_var,
                          symbolTable,
                          copyVars = character(),
                          noncopyVars = character()) {
      cppParallelBodyClass_init_impl(self,
                                     loop_body = loop_body,
                                     loop_var = loop_var,
                                     symbolTable = symbolTable,
                                     copyVars = copyVars,
                                     noncopyVars = noncopyVars)
    },
    generate = function(declaration = FALSE, ...) {
      ## This version of generate creates a fully inlined version
      ## when declaration is TRUE and returns an empty string
      ## when declaration is FALSE.
      if(declaration) {
        symbolsToUse <- if(inherits(symbolTable, 'symbolTableClass'))
          symbolTable$getSymbols()
        else
          list()
        
        output <- c(generateClassHeader(name, inheritance),
                    list('public:'), ## In the future we can separate public and private
                    lapply(generateObjectDefs(symbolsToUse),
                           function(x)
                             if(length(x)==0)
                               ''
                           else
                             pasteSemicolon(x, indent = '  ')),
                    generateAll(cppFunctionDefs),
                    '};'
        )
        unlist(output)
      } else
        ""
    }
  )
)

cppParallelBodyClass_init_impl <- function(cppDef,
                                           name = "parallel_loop_body",
                                           orig_loop_code = orig_loop_code,
                                           loop_body = orig_loop_code$args[[3]],
                                           loop_var = orig_loop_code$args[[1]],
                                           symbolTable,
                                           copyVars,
                                           noncopyVars) {
  ## 1. Create symbolTable for copyVars + noncopyVars
  ## 2. Create operator()
  ## 3. Create constructor
  #####
  ## Create symbolTables. Assume we are using cpp symbol tables
  ## newSymTab is the symbolTable for the class.
  newSymTab <- symbolTableClass$new()
  ## newLocalSymTab is the symbolTable for the body of operator()
  newLocalSymTab <- symbolTableClass$new()
  ## Put loop_var in newLocalSymTab
  indexName <- loop_var$name
  sym <- symbolTable$getSymbol(indexName, inherits = TRUE)
  if(is.null(sym)) {
    stop(paste0("No variable named: ", indexName),
         call. = FALSE)
  }
  indexSym <- sym$clone(deep = TRUE)
  newLocalSymTab$addSymbol(indexSym)
  ## Make symbol table entries and code for the copyVars
  for(v in copyVars) {
    sym <- symbolTable$getSymbol(v, inherits = TRUE)
    if(is.null(sym)) {
      stop(paste0("No variable named: ", v),
           call. = FALSE)
    }
    sym <- sym$clone(deep = TRUE)
    localSym <- sym$clone(deep = TRUE)
    sym$ref <- TRUE
    sym$name <- paste0(sym$name, "_orig_")
    newSymTab$addSymbol(sym)
    if(!inherits(localSym, 'cppVarFullClass'))
      localSym <- cppVar2cppVarFull(localSym)
    localSym$constructor <- paste0("(", sym$name, ")")
    newLocalSymTab$addSymbol(localSym) ## put in local symbol table
  }
  for(v in noncopyVars) {
    sym <- symbolTable$getSymbol(v, inherits = TRUE)
    if(is.null(sym)) {
      stop(paste0("No variable named: ", v),
           call. = FALSE)
    }
    sym <- sym$clone(deep = TRUE)
    sym$ref <- TRUE
    newSymTab$addSymbol(sym)  
  }
  ## Create operator()
  generalForExpr <- exprClass$new(name = 'GeneralFor', isCall = TRUE,
                                  isName = FALSE, isAssign = FALSE, isLiteral = FALSE)
  indexName <- loop_var$name
  ## TO-DO: Make "=" work same as "<-"
  ## Make fun() work
  ## 
  setArg(generalForExpr, 1, nParse(paste0("cppLiteral(\"", indexName, " = r__.begin()\" )")))
  setArg(generalForExpr, 2, nParse(paste0("cppLiteral(\"", indexName, " != r__.end()\" )")))
  setArg(generalForExpr, 3, nParse(paste0("cppLiteral(\"++", indexName, "\")")))
  setArg(generalForExpr, 4, loop_body)
  body_code_block <- cppCodeBlockClass$new(code = generalForExpr,
                                           symbolTable = newLocalSymTab)
  ## argSymTab is the symbolTable for the arguments to operator()
  argSymTab <- symbolTableClass$new()
  argSymTab$addSymbol(cppVarFullClass$new(name = 'r__',
                                          baseType = "tbb::blocked_range<int>",
                                          ref = TRUE, 
                                          const = TRUE))
  `operator()` <- cppFunctionClass$new(name = "operator()", 
                                       args = argSymTab, 
                                       code = body_code_block,
                                       const = TRUE,
                                       returnType = cppVoid())
  ## create constructor
  ctorArgSymTab <- newSymTab$clone(deep = TRUE)
  initializerList <- list()
  for(iSym in seq_along(ctorArgSymTab$symbols)) {
    thisSymName <- ctorArgSymTab$symbols[[iSym]]$name
    thisArgName <- paste0(thisSymName, '_')
    ctorArgSymTab$symbols[[iSym]]$name <- thisArgName
    initializerList[[iSym]] <- nParse(substitute(X(X_), 
                                                 list(X = as.name(thisSymName),
                                                      X_ = as.name(thisArgName))))
  }
  constructor <- cppFunctionClass$new(name = name,
                                      args = ctorArgSymTab,
                                      code = cppCodeBlockClass$new(
                                        code = nParse(quote({})),
                                        symbolTable = symbolTableClass$new()
                                      ),
                                      initializerList = initializerList,
                                      returnType = cppBlank())
  cppDef$name <- name
  cppDef$cppFunctionDefs <- list(`operator()` = `operator()`,
                                 constructor = constructor)
  cppDef$symbolTable <- newSymTab
  invisible(NULL)
}
