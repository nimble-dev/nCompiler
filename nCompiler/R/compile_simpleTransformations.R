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
    
    opInfo <- check_cachedOpInfo(code, where=auxEnv$where, update=TRUE, allowFail = TRUE)
    handlingInfo <- getOperatorField(opInfo$opDef, opInfoName)
    # handlingInfo <- getOperatorDef(code$name, opInfoName)
    # opInfo <- getOperatorDef(code$name) #operatorDefEnv[[code$name]]
    # if(!is.null(opInfo)) {
    #   handlingInfo <- opInfo[[opInfoName]]
      if(!is.null(handlingInfo)) {
        handler <- handlingInfo[['handler']]
        if(!is.null(handler))
          if(is.function(handler)) {
            handler(code, symTab, auxEnv, handlingInfo)
          } else {
            eval(call(handler, code, symTab, auxEnv, handlingInfo),
                 envir = handlerEnv)
          }
      }
    # }
  }
  nErrorEnv$stateInfo <- character()
  invisible(NULL)
}

simpleTransformationsEnv <- new.env()
simpleTransformationsEnv$.debug <- FALSE

inSimpleTransformationsEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = simpleTransformationsEnv)
}

## for min(V), no change.  for min(v1, v2), change to pairmin(v1, v2)
inSimpleTransformationsEnv(
  minMax <-
    function(code, symTab, auxEnv, info) {
      if(length(code$args) == 2) code$name <- paste0('pair',code$name)
    }
)

## Used e.g. for invisible(foo(x)) --> foo(x)
inSimpleTransformationsEnv(
  RemoveLayer <-
    function(code, symTab, auxEnv, info) {
      removeExprClassLayer(code)
    }
)

inSimpleTransformationsEnv(
  replace <-
    function(code, symTab, auxEnv, info) {
      repl <- info$replacement
      if(is.null(repl))
        stop(paste0("No valid replacement for ",
                    code$name),
             call. = FALSE)
      code$name <- repl
    }
)

inSimpleTransformationsEnv(
  replaceAndNormalize <-
    function(code, symTab, auxEnv, info) {
      repl <- info$replacement
      if(is.null(repl))
        stop(paste0("No valid replacement for ",
                    code$name),
             call. = FALSE)
      code$name <- repl
      compile_normalizeCalls(code, symTab, auxEnv)
    }
)

inSimpleTransformationsEnv(
  Literal <-
    function(code, symTab, auxEnv, info) {
      if(!is.null(code$aux$compileArgs$text)) {
        code$aux$compileArgs$text <- eval(code$aux$compileArgs$text, envir = auxEnv$closure)
      }
    }
)
inSimpleTransformationsEnv(
  CheckOpAssignment <-
    function(code, symTab, auxEnv, info) {
      arg1 <- code$args[[1]]
      arg1_opInfo <- check_cachedOpInfo(arg1, where=auxEnv$where, update=TRUE, allowFail = TRUE)
      arg1_useOpAssign <- isTRUE(arg1_opInfo$opDef$useOpAssign)
      if(!arg1_useOpAssign) return(invisible(NULL))

      if(isTRUE(arg1$isCall)) {
        # change `<-`(length(x), 5) to `length<-`(x, 5)
        name <- arg1$name


        assign_name <- paste0(name, "<-")
        code$name <- assign_name
        arg2 <- code$args[[2]]
        code$args <- list()
        arg1names <- names(arg1$args)
        code$args <- list() # rebuild from scratch
        for(i in seq_along(arg1$args)) {
          insertArg(code, i, arg1$args[[i]], arg1names[i])
        }
        insertArg(code, length(code$args) + 1, arg2, "value")
        compile_normalizeCalls(code, symTab, auxEnv)
      } 
    }
)

inSimpleTransformationsEnv(
  EvalDrop <-
    function(code, symTab, auxEnv, info) {
      # The drop argument to brackets has weird behavior in R
      # It is TRUE unless it is FALSE, "FALSE", or "F", or 0
      # These are all results from as.logical.
      # NA and NaN result in TRUE, so we go through isFALSE.
      drop_arg <- code$aux$compileArgs$drop
      drop_arg <- eval(drop_arg, envir = auxEnv$where)
      drop_arg <- as.logical(drop_arg)
      drop_arg <- !isFALSE(drop_arg)
      code$aux$compileArgs$drop <- drop_arg
    }
)