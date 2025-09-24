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
        
        output <- c(generateClassHeader(name, inheritance, nClass_inheritance),
                    list('public:'), ## In the future we can separate public and private
                    lapply(generateObjectDefs(symbolsToUse),
                           function(x)
                             if(length(x)==0)
                               ''
                           else
                             pasteSemicolon(x, indent = '  ')),
                    generateAll(memberCppDefs),
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
  cppDef$memberCppDefs <- list(`operator()` = `operator()`,
                                 constructor = constructor)
  cppDef$symbolTable <- newSymTab
  invisible(NULL)
}

cppParallelReduceBodyClass <- R6::R6Class(
  'cppParallelReduceBodyClass',
  inherit = cppClassClass,
  portable = FALSE,
  public = list(
    initialize = function(loop_body,
                          loop_var,
                          symbolTable,
                          copyVars = character(),
                          noncopyVars = character()) {
      cppParallelReduceBodyClass_init_impl(self,
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

        output <- c(generateClassHeader(name, inheritance, nClass_inheritance),
                    list('public:'), ## In the future we can separate public and private
                    lapply(generateObjectDefs(symbolsToUse),
                           function(x)
                             if(length(x)==0)
                               ''
                           else
                             pasteSemicolon(x, indent = '  ')),
                    generateAll(memberCppDefs),
                    '};'
        )
        unlist(output)
      } else
        ""
      ## TODO: C++ generation of the original nFunction where parallel_reduce appears gets
      ## 'int i__;' and 'double value__;', seemingly because of symbol table sharing.
    }
  )
)

cppParallelReduceBodyClass_init_impl <- function(cppDef,
                                                 name = "parallel_reduce_body",
                                                 orig_loop_code = orig_loop_code,
                                                 loop_body = orig_loop_code$args[[3]],
                                                 loop_var = orig_loop_code$args[[1]],
                                                 symbolTable,
                                                 copyVars,
                                                 noncopyVars) {
  ## 1. call cppParallelBodyClass_init_impl which creates GeneralFor
  ## 2. make some minor alterations to the body of `operator()`
  ## 3. Create split constructor
  ## 4. Create join method

  ## need to save this here because cppParallelBodyClass_init_impl will change
  ## loop_body's caller
  orig_caller <- loop_body$caller

  cppParallelBodyClass_init_impl(cppDef, name, orig_loop_code, loop_body,
                                 loop_var, symbolTable, copyVars, noncopyVars)

  ## get the local aggregation var copy variable
  val_expr <- copyExprClass(loop_body$args[[1]])
  ## make a new exprClass instance for the aggregation variable
  value_name <- orig_caller$args[[5]] ## should be a string
  value_expr <- exprClass$new(name = value_name, isCall = FALSE, isName = TRUE,
                              isAssign = FALSE, isLiteral = FALSE)
  ## remove '&' from the aggregation value member of parallel_reduce_body
  cppDef$symbolTable$symbols[[value_name]]$ref <- FALSE
  cppDef$memberCppDefs[['constructor']]$args$symbols[[
    value_name]]$ref <- FALSE
  ## create the assignment expr `val__ = value`
  val_assign <- newAssignmentExpression()
  setArg(val_assign, 1, val_expr)
  setArg(val_assign, 2, value_expr)
  ## create the assignment back to the aggregation value member
  value_assign <- newAssignmentExpression()
  setArg(value_assign, 1, copyExprClass(value_expr))
  setArg(value_assign, 2, copyExprClass(val_expr))
  ## edit `operator()`'s body (loop_body$caller)
  cppDef$memberCppDefs[['operator()']]$code$code <- newBracketExpr(
    list(val_assign, loop_body$caller, value_assign))
  ## add val__ to `operator()`'s symbolTable
  cppDef$memberCppDefs[['operator()']]$code$symbolTable$addSymbol(
    cppVarClass$new(name = val_expr$name, baseType = val_expr$type$type))
  ## remove 'const' from the `operator()` declaration
  cppDef$memberCppDefs[['operator()']]$const <- FALSE

  ## get the reduce op's identity element which is guaranteed to be a literal
  ## by the labelAbstractTypes ParallelReduce handler
  init_arg <- copyExprClass(orig_caller$caller$caller$args[[1]]$args[[2]])

  split_ctor_symTab <- symbolTableClass$new()
  split_ctor_symTab$addSymbol(cppVarClass$new(name = 'parent', baseType = name,
                                              ref = TRUE))
  split_ctor_symTab$addSymbol(cppVarClass$new(name = 'tbb::split'))
  ## Get the name of the vector we're working with, which together with the
  ## aggregation var is used in the initializerList of the split constructor.
  vector_name <- orig_caller$args[[4]] ## should be a string
  initializerList <- list()
  initializerList[[1]] <- nParse(
    substitute(X(X_), list(X = as.name(value_name),
                           X_ = as.name(init_arg$name))))
  initializerList[[2]] <- nParse(
    substitute(X(X_), list(X = as.name(vector_name),
                           X_ = as.name(paste0('parent.', vector_name)))))

  split_constructor <- cppFunctionClass$new(name = name,
                                            args = split_ctor_symTab,
                                            code = cppCodeBlockClass$new(
                                              code = nParse(quote({})),
                                              symbolTable = symbolTableClass$new()
                                            ),
                                            initializerList = initializerList,
                                            returnType = cppBlank())

  ## join_symTab is the symbolTable for the arguments to join
  join_symTab <- symbolTableClass$new()
  join_symTab$addSymbol(cppVarFullClass$new(name = 'target',
                                            baseType = name,
                                            ref = TRUE,
                                            const = TRUE))
  ## make the reduce code
  reduce_op <- exprClass$new(name = loop_body$args[[2]]$name, isCall = TRUE,
                             isName = FALSE, isAssign = FALSE,
                             isLiteral = FALSE)
  setArg(reduce_op, 1, copyExprClass(value_expr))
  setArg(reduce_op, 2, nParse(paste0('cppLiteral("target.', value_name, ';")')))
  join_code <- newAssignmentExpression()
  setArg(join_code, 1, copyExprClass(value_expr))
  setArg(join_code, 2, reduce_op)
  ## create the join cppFunctionClass definition
  join_body <- cppCodeBlockClass$new(code = join_code,
                                     ## TODO: any symbols ever needed?
                                     symbolTable = symbolTableClass$new())
  join <- cppFunctionClass$new(name = "join",
                               args = join_symTab,
                               code = join_body,
                               const = FALSE,
                               returnType = cppVoid())

  cppDef$memberCppDefs <- c(cppDef$memberCppDefs,
                              list(split_constructor = split_constructor,
                                   join = join))
  invisible(NULL)
}
