
NFtry <- function(expr, 
                  stage = '(unknown stage)',
                  use_nCompiler_error_handling = TRUE) {
  expr <- substitute(expr)
  ## Use options(error = recover) for "normal" debugging
  ## set.nOption 'recover_on_untrapped_errors' for similar effect
  if(!use_nCompiler_error_handling)
    eval(expr, parent.frame())
  else ## This is to report useful information on errors
    withCallingHandlers(
      eval(expr, parent.frame()),
      error = function(e) {
        cat(paste0("An error occurred in compiler stage ",
                   stage, ":"),
            conditionMessage(e), 
            nStateInfo(),
            sep = "\n")
        stack <- sapply(sys.calls(), deparse)
        .GlobalEnv$.nCompiler.traceback <-
          capture.output(traceback(stack))
        if(isTRUE(get_nOption('error_recover')))
          evalq(recover(), sys.frame(length(stack)-4))
        invisible(NULL)
      }
    )
}

nStateInfo <- function() {
  ## In future, this will be populated during processing stages
  msg <- nErrorEnv[['stateInfo']]
  if(is.null(msg)) msg <- character()
  if(length(msg) > 0)
    msg <- paste0("This occurred while: ",
                  paste(msg, collapse = "\n"))
  msg
}
