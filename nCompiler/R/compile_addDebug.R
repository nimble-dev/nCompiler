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
      for(i in seq_along(code$args)) {
        recurse <- TRUE
        if(recurse) {
          setupCalls <- unlist(
            # recursively add debugging marks to the steps in expression '{'
            compile_addDebug(code$args[[i]],
                                  symTab,
                                  auxEnv,
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
        # push function name onto debugging stacktrace in C++ by manipulating 
        # AST at the top level of an nFunction
        # 
        # this will add an exprClass object to be the first argument of the 
        # current AST element---this will be the new first line of a function.  
        # the new exprClass will add "PUSH_DEBUGFUN([nameExpr])" to the AST, 
        # where PUSH_DEBUGFUN is an nCompiler C++ macro that adds the name of 
        # the currently executing function to the stack trace.  The argument 
        # nameExpr contains the name of the currently executing function.
        nameExpr <- exprClass$new(name = workEnv$name, isName = FALSE, 
                                  isCall = FALSE, isAssign = FALSE, 
                                  isLiteral = TRUE)
        pushExpr <- exprClass$new(name = 'PUSH_DEBUGFUN', isName = FALSE, 
                                  isCall = TRUE, isAssign = FALSE, 
                                  args = list(nameExpr))
        insertArg(code, 1, pushExpr)

        # pop function name off of debugging stacktrace in C++ by manipulating 
        # AST at the top level of an nFunction
        # 
        # this will add an exprClass object to be the last argument of the 
        # current AST element---this will be the new last line of code in 
        # a function.  the new exprClass will add "POP_DEBUGFUN()" to the AST, 
        # where POP_DEBUGFUN is an nCompiler C++ macro that removes the name of 
        # the currently executing function from the stack trace.
        popExpr <- exprClass$new(name = 'POP_DEBUGFUN', isName = FALSE, 
                                 isCall = TRUE, isAssign = FALSE)
        insertArg(code, length(code$args)+1, popExpr)
      }
      return(invisible(NULL))
    }
    if(code$name == 'for') {
      # recursively add debugging marks to the steps in a "for" loop
      compile_addDebug(code$args[[3]], symTab, auxEnv)
      return(invisible(NULL))
    }
    if(code$name %in% ifOrWhile) {
      # recursively add debugging marks to the steps in "if" or "while" blocks
      compile_addDebug(code$args[[2]], symTab, auxEnv)
      if(length(code$args)==3)
        compile_addDebug(code$args[[3]], symTab, auxEnv)
      return(invisible(NULL))
    }
    if(code$name == 'return') {
      # pop function name off of debugging stacktrace in C++ by manipulating AST
      # 
      # this will add an exprClass object immediately before a "return" 
      # statement in the AST.  the new exprClass will add 
      # "POP_DEBUGFUN()" to the AST, where POP_DEBUGFUN is an
      # nCompiler C++ macro that removes the name of the currently executing 
      # function from the stack trace.
      popExpr <- exprClass$new(name = 'POP_DEBUGFUN', isName = FALSE, 
                               isCall = TRUE, isAssign = FALSE)
      insertArg(code$caller, code$callerArgID, popExpr)
    } else {
      # set name of code being evaluated in C++ stacktrace by manipulating AST
      # 
      # this will add an exprClass object that wraps the AST element "code", 
      # replacing "code" with "SET_DEBUG_MSG([code], [label])" in the AST. 
      # SET_DEBUG_MSG is an nCompiler C++ macro that uses the string [label] to 
      # update the state of the stacktrace before executing [code].  The string
      # [label] is the result of "nDeparse(code)", which is a debugging message
      # that associates the C++ code generated for [code] with its 
      # representation in R.
      setExpr <- wrapExprClassOperator(code = code, funName = 'SET_DEBUG_MSG')
      escapedName <- gsub(
        pattern = '"', replacement = '\\\\"', x = nDeparse(code)
      )
      labelExpr <- exprClass$new(name = escapedName, isName = FALSE, 
                                 isCall = FALSE, isAssign = FALSE, 
                                 isLiteral = TRUE)
      insertArg(setExpr, 2, labelExpr)
      insertExprClassLayer(setExpr, 1, "CODE__")
    }
  }
  return(NULL)
}
