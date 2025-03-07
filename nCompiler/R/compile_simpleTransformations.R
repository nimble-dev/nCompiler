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
  nErrorEnv$stateInfo <- paste0("handling ", opInfoName, " for ", code$name, ".")
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
    if(!is.null(opInfo)) {
      handlingInfo <- opInfo[[opInfoName]]
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

simpleTransformationsEnv$Literal <-
  function(code, symTab, auxEnv, info) {
    if(!is.null(code$aux$compileArgs$text)) {
      code$aux$compileArgs$text <- eval(code$aux$compileArgs$text, envir = auxEnv$where)
    }
  }
