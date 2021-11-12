#########################
## Add debugging marks ##
#########################

debuggingEnv <- new.env()
debuggingEnv$.debug <- FALSE
inDebuggingEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = debuggingEnv)
}

compile_addDebug <- function(code,
                                  symTab,
                                  auxEnv,
                                  flowDepth,
                                  ## It is unclear if workEnv will be needed.
                                  workEnv = new.env()) {
  nErrorEnv$stateInfo <- paste0("handling debugging marks for ",
                                code$name,
                                ".")
  setupExprs <- list()
  if(code$isLiteral) {
    return(list())
  }
  if(code$isName) {
    return(list())
  }
  if(code$isCall) {
    if(code$name == '{') {
      if(is.null(code$caller)) {
        # push function name for top-level expressions
        nameExpr <- exprClass$new(name = workEnv$name, isName = FALSE, 
                                  isCall = FALSE, isAssign = FALSE, 
                                  isLiteral = TRUE)
        pushExpr <- exprClass$new(name = 'PUSH_DEBUGFUN', isName = FALSE, 
                                  isCall = TRUE, isAssign = FALSE, 
                                  args = list(nameExpr))
        insertArg(code, 1, pushExpr)
      }
      for(i in seq_along(code$args)) {
        recurse <- TRUE
        if(recurse) {
          setupCalls <- unlist(
            compile_addDebug(code$args[[i]],
                                  symTab,
                                  auxEnv,
                                  flowDepth = flowDepth + 1,
                                  workEnv = new.env())) ## a new line
          if(length(setupCalls) > 0) {
            newExpr <- newBracketExpr(args = c(setupCalls,
                                               code$args[[i]])
                                      )
            setArg(code, i, newExpr)
          }
        }
      }
      if(is.null(code$caller)) {
        # pop function name from top-level expressions
        popExpr <- exprClass$new(name = 'POP_DEBUGFUN', isName = FALSE, 
                                 isCall = TRUE, isAssign = FALSE)
        insertArg(code, length(code$args)+1, popExpr)
      }
      return(invisible(NULL))
    }
    if(code$name == 'for') {
      compile_addDebug(code$args[[3]], symTab, auxEnv, flowDepth = 0)
      return(invisible(NULL))
    }
    if(code$name %in% ifOrWhile) {
      compile_addDebug(code$args[[2]], symTab, auxEnv, flowDepth = 0)
      if(length(code$args)==3)
        compile_addDebug(code$args[[3]], symTab, auxEnv, flowDepth = 0)
      return(invisible(NULL))
    }
    
    if(flowDepth == 2) {
      if(code$name == 'return') {
        popExpr <- exprClass$new(name = 'POP_DEBUGFUN', isName = FALSE, 
                                 isCall = TRUE, isAssign = FALSE)
        insertArg(code$caller, code$callerArgID, popExpr)
      } else {
        setExpr <- wrapExprClassOperator(code = code, funName = 'SET_DEBUG_MSG')
        labelExpr <- exprClass$new(name = nDeparse(code), isName = FALSE, 
                                   isCall = FALSE, isAssign = FALSE, 
                                   isLiteral = TRUE)
        insertArg(setExpr, 2, labelExpr)
      }
    }
    
  }
  return(NULL)
}
