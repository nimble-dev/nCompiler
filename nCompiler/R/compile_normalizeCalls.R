normalizeCallsEnv <- new.env()
normalizeCallsEnv$.debug <- FALSE

inNormalizeCallsEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = normalizeCallsEnv)
}

compile_normalizeCalls <- function(code,
                                   symTab,
                                   auxEnv) {
  nErrorEnv$stateInfo <- paste0("handling normalizeCalls for ",
                                code$name,
                                ".")

  logging <- get_nOption('compilerOptions')[['logging']]
  if (logging) appendToLog(paste('###', nErrorEnv$stateInfo, '###'))

  if(code$isLiteral) return(NULL)
  if(code$isName) return(NULL)
  if(code$isCall) {
    if(code$name == '{') {
      ## recurse over lines
      for(i in seq_along(code$args)) {
        compile_normalizeCalls(code$args[[i]], symTab, auxEnv)
      }
      return(NULL)
    }
    opInfo <- getOperatorDef(code$name)
    ## Check for nFunctions or nClass methods (also nFunctions)
    if(is.null(opInfo)) {
      obj <- NULL
      if(isNCgenerator(auxEnv$where)) {## We are in a class method (by direct call within another class method, no `$` involved)
        obj <- NC_find_method(auxEnv$where, code$name, inherits=TRUE)
        if(!is.null(obj)) {
          code$aux$obj_internals <- NFinternals(obj)
          if(isNF(obj)) {
            opInfo <- operatorDefEnv[['NCMETHOD_']]
          } else {
            stop(exprClassProcessingErrorMsg(code,
                                             paste0('method ', code$name, 'is being called, but it is not a nFunction.')),
                 call. = FALSE)
          }
        }
      }
      ## Next we check if code$name exists in the where.
      ## Note that if we are in a method, auxEnv$where will be the
      ## generator, which is an environment.  However, we need
      ## to use nGet instead of exists and get.
      if(is.null(obj)) {
        obj <- nGet(code$name, where = auxEnv$where)
        ## An nFunction will be transformed to
        ## have code$name 'NFCALL_' during simpleTransformations,
        ## but that hasn't happened yet, so we manually use it here.
        ## To-do: This could be cleaned up by either making that change here
        ## when first detected or making a separate compiler stage just for that.
        if(!is.null(obj)) {
          if(isNF(obj)) {
            code$aux$obj_internals <- NFinternals(obj)
            opInfo <- operatorDefEnv[['NFCALL_']]
            uniqueName <- NFinternals(obj)$uniqueName
            if(length(uniqueName)==0)
              stop(
                exprClassProcessingErrorMsg(
                  code,
                  paste0('nFunction ', code$name, 'is being called, ',
                         'but it is malformed because it has no internal name.')),
                call. = FALSE)
            if(is.null(auxEnv$needed_nFunctions[[uniqueName]])) {
              ## We could put the nFunction itself in the needed_nFunctions list,
              ## but we do not as a way to avoid having many references to R6 objects
              ## in a blind attempt to facilitate garbage collection based on past experience.
              ## Instead, we provide what is needed to look up the nFunction again later.
              auxEnv$needed_nFunctions[[uniqueName]] <- list(code$name, auxEnv$where)
            }
          } else
            stop(exprClassProcessingErrorMsg(
              code,
              paste0(code$name, ' is being used as a function, but it is not a nFunction.')),
              call. = FALSE)
        }
      }
    }
    # There is also the nFunctionRef to think about. That is a bit more of a type however, not a call.
    if(!is.null(opInfo)) {
      matchDef <- opInfo[["matchDef"]]
      if(!is.null(matchDef)) {
        matched_code <- exprClass_put_args_in_order(matchDef, code)
        code <- replaceArgInCaller(code, matched_code)
      }
      handlingInfo <- opInfo[["normalizeCalls"]]
      if(!is.null(handlingInfo)) {
        handler <- handlingInfo[['handler']]
        if(!is.null(handler)) {
          if (logging)
            appendToLog(paste('Calling handler', handler, 'for', code$name))
          ans <- eval(call(handler, code, symTab, auxEnv, handlingInfo),
                      envir = normalizeCallsEnv)
          nErrorEnv$stateInfo <- character()
          if (logging) {
            appendToLog(paste('Finished handling', handler, 'for', code$name))
            logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
          }
          return(ans)
        }
      }
    }
    # default behavior if there is no handler (which will be for many or most calls)
    normalizeCallsEnv$recurse_normalizeCalls(code, symTab, auxEnv, handlingInfo)
  }
  # Where to put a generic recursion call?
  nErrorEnv$stateInfo <- character()
  invisible(NULL)
}

inNormalizeCallsEnv(
  recurse_normalizeCalls <-
    function(code, symTab, auxEnv, handlingInfo,
             useArgs = rep(TRUE, length(code$args))) {
      ## won't be here unless code is a call.  It will not be a {
      for(i in seq_along(code$args)) {
        if(useArgs[i]) {
          if(inherits(code$args[[i]], 'exprClass')) {
            compile_normalizeCalls(code$args[[i]],
                                   symTab,
                                   auxEnv)
          }
        }
      }
      NULL
    }
)

## inNormalizeCallsEnv(
##   skip <-
##     function(code, symTab, auxEnv, handlingInfo,
##              useArgs = rep(TRUE, length(code$args))) {
##       NULL
##     }
## )

inNormalizeCallsEnv(
  convert_nFunction_or_method_AST <-
    function(code) {
      nFunctionName <- code$name
      obj_internals <- code$aux$obj_internals
      code$aux$obj_internals <- NULL
      matched_code <- exprClass_put_args_in_order(obj_internals$template, code)
      code <- replaceArgInCaller(code, matched_code)
      ## Note that the string `NFCALL_` matches the operatorDef entry.
      ## Therefore the change-of-name here will automatically trigger use of
      ## the 'NFCALL_' operatorDef in later stages.
      code$name <- 'NFCALL_'
      cpp_code_name <- obj_internals$cpp_code_name
      fxnNameExpr <- exprClass$new(name = cpp_code_name, isName = TRUE,
                                   isCall = FALSE, isLiteral = FALSE, isAssign = FALSE)
      fxnNameExpr$aux$obj_internals <- obj_internals
      fxnNameExpr$aux$nFunctionName <- nFunctionName
      ## We may need to add content to this symbol if
      ## necessary for later processing steps.
      insertArg(code, 1, fxnNameExpr, "FUN_")
      obj_internals <- NULL
      invisible(NULL)
    }
)

inNormalizeCallsEnv(
  nFunction_or_method_call <-
    function(code, symTab, auxEnv, handlingInfo) {
      recurse_normalizeCalls(code, symTab, auxEnv,
                             handlingInfo)
      convert_nFunction_or_method_AST(code)
      NULL
    }
)
