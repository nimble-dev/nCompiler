## Many cases handled here are simple replacements, e.g. ^ to pow.
## These could easily be done elsewhere, but it works cleanly to do them here.

####################################
## System to processSpecificCalls ##
####################################

compile_simpleTransformations <- function(code,
                                          symTab, 
                                          auxEnv, 
                                          opInfoName = "simpleTransformations",
                                          handlerEnv = simpleTransformationsEnv) {
    nErrorEnv$stateInfo <- paste0("handling simpleTransformations for ", code$name, ".")
    if(code$isName) return(invisible())
    if(code$isCall) {
        for(i in seq_along(code$args)) {
            if(inherits(code$args[[i]], 'exprClass')) {
                compile_simpleTransformations(code$args[[i]], 
                                              symTab, 
                                              auxEnv,
                                              opInfoName,
                                              handlerEnv)
            }
        }
        
        opInfo <- operatorDefEnv[[code$name]]
        ## TO-DO: Check for methods or nFunctions.
        if(is.null(opInfo) && exists(code$name, envir = auxEnv$closure)) {
          obj <- get(code$name, envir = auxEnv$closure)
          if(isNF(obj)) {
            uniqueName <- NFinternals(obj)$uniqueName
            if(length(uniqueName)==0)
              stop(
                exprClassProcessingErrorMsg(
                  code,
                  paste0('nFunction ', 
                         code$name, 
                         'is being called, but it is malformed because it has no internal name.')),
                call. = FALSE)
            if(is.null(auxEnv$needed_nFunctions[[uniqueName]])) {
              ## Avoid many references to R6 objects in a blind attempt to 
              ## facilitate garbage collection.
              auxEnv$needed_nFunctions[[uniqueName]] <- list(code$name, 
                                                             auxEnv$closure)
            }
            opInfo <- operatorDefEnv[['nFunction']]
          }
        }
        
        if(!is.null(opInfo)) {
            handlingInfo <- opInfo[[ opInfoName ]]
            if(!is.null(handlingInfo)) {
                handler <- handlingInfo[['handler']]
                if(!is.null(handler))
                    eval(call(handler, code, symTab, auxEnv, handlingInfo),
                         envir = handlerEnv)
            }
        }
    }
    nErrorEnv$stateInfo <- character()
    invisible(NULL)
}

simpleTransformationsEnv <- new.env()
simpleTransformationsEnv$.debug <- FALSE

simpleTransformationsEnv$Generic_nFunction <- 
  function(code, symTab, auxEnv, info) {
    nFunctionName <- code$name
    ## Note that the string `nFunction` matches the operatorDef entry.
    ## Therefore the change-of-name here will automatically trigger use of
    ## the 'nFunction' operatorDef.  On the other hand, if there is an nFunction
    ## that is not transformed here, it will also trigger `nFunction` operatorDef
    ## after being found by scoping in later compiler stages.
    code$name <- 'nFunction'
    obj <- get(nFunctionName, envir = auxEnv$closure)
    code$aux$nFunctionInfo <- list(nFunctionName = nFunctionName,
                                   cpp_code_name = NFinternals(obj)$cpp_code_name,
                                   where = auxEnv$closure)
    invisible(NULL)
  }

## for min(V), no change.  for min(v1, v2), change to pairmin(v1, v2)
simpleTransformationsEnv$minMax <-
  function(code, symTab, auxEnv, info) {
    if(length(code$args) == 2) code$name <- paste0('pair',code$name)
  }

simpleTransformationsEnv$replace <-
  function(code, symTab, auxEnv, info) {
    repl <- info$replacement
    if(is.null(repl))
      stop(paste0("No valid replacement for ",
                  code$name),
           call. = FALSE)
    code$name <- repl
  }
