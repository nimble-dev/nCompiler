finalTransformationsEnv <- new.env()
finalTransformationsEnv$.debug <- FALSE

inFinalTransformationsEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = finalTransformationsEnv)
}


inFinalTransformationsEnv(
  replace_nameSubList <- function(vars, nameSubList) {
    for(iv in seq_along(vars)) {
      ## Look for a mangled argument name in nameSubList.
      ## It is unfortunate to have to do this here instead of earlier
      ## when other names are replaced, but here the names are given
      ## as character objects (potentially from R evaluation).
      thisVar <- nameSubList[[vars[iv] ]]
      if(isTRUE(is.null(thisVar)))
        thisVar <- vars[iv]
      else
        thisVar <- deparse(thisVar)
      vars[iv] <- thisVar
    }
    vars
  }
)

inFinalTransformationsEnv(
  ParallelFor <- function(code, symTab, auxEnv, info) {
    copyVars <- eval(nDeparse(code$args[[4]], toR = TRUE), 
                     envir = auxEnv$closure)
    shareVars <- eval(nDeparse(code$args[[5]], toR = TRUE),
                      envir = auxEnv$closure)
    ## Look for a mangled argument name in nameSubList.
    ## It is unfortunate to have to do this here instead of earlier
    ## when other names are replaced, but here the names are given
    ## as character objects (potentially from R evaluation).
    copyVars <- replace_nameSubList(copyVars, auxEnv$nameSubList)
    shareVars <- replace_nameSubList(shareVars, auxEnv$nameSubList)

    code$args[[4]] <- copyVars ## This is no longer an exprClass
    code$args[[5]] <- shareVars ## Citto
    auxEnv$parallelContent <- c(auxEnv$parallelContent, code)
    ##  parallel_for(blocked_range<size_t>(0, n), parallel_loop_body(x));
    blocked_range_expr <- exprClass$new(name = "blocked_range<int>",
                                        isCall = TRUE,
                                        isName = FALSE, isLiteral = FALSE, isAssign = FALSE)
    setArg(blocked_range_expr, 1, copyExprClass(code$args[[2]]$args[[1]]))
    setArg(blocked_range_expr, 2, copyExprClass(code$args[[2]]$args[[2]]))
    parallel_for_expr <- exprClass$new(name = "parallel_for", 
                                       isCall = TRUE,
                                       isName = FALSE, isLiteral = FALSE, isAssign = FALSE)
    setArg(parallel_for_expr, 1, blocked_range_expr)
    loop_body_expr <- exprClass$new(name = "parallel_loop_body",
                                    isCall = TRUE,
                                    isName = FALSE, isLiteral = FALSE, isAssign = FALSE)
    allVars <- c(copyVars, shareVars)
    for(iv in seq_along(allVars)) {
      ## Look for a mangled argument name in nameSubList.
      ## It is unfortunate to have to do this here instead of earlier
      ## when other names are replaced, but here the names are given
      ## as character objects (potentially from R evaluation).
      thisVar <- allVars[iv]
      setArg(loop_body_expr, iv, 
             exprClass$new(name = thisVar, 
                           isCall = FALSE, isName = TRUE, isLiteral = FALSE, isAssign = FALSE))
    }
    setArg(parallel_for_expr, 2, loop_body_expr)
    setArg(code$caller, code$callerArgID, parallel_for_expr)
    NULL
  }
)
