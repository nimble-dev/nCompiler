nErrorEnv <- new.env()
nDebugEnv <- new.env()

## Set nErrorEnv$stateInfo to cache state information (character)
## for display in the event of an error.

getCompilerLog <- function(echo = FALSE) {
  out <- paste(nDebugEnv$compilerLog, collapse = '\n ')
  if (echo) cat(out)
  invisible(out)
}

logAST <- function(code, msg = "Full AST at end of stage:",
                   showType = TRUE, showImpl = TRUE) {
  nDebugEnv$compilerLog <- c(
    nDebugEnv$compilerLog, msg,
    '--------',
    capture.output(
      code$print(showType = showType, showImpl = showImpl)
    ),
    '--------\n'
  )
  invisible(NULL)
}

logBeforeStage <- function(stageName) {
  appendToLog(
    paste('*** Entering stage', stageName, '*** \n')
  )
  invisible(NULL)
}

logAfterStage <- function(stageName) {
  appendToLog(
    paste('*** Exiting stage', stageName, '*** \n')
  )
  invisible(NULL)
}

appendToLog <- function(msg, ...) {
  nDebugEnv$compilerLog <- c(
    nDebugEnv$compilerLog, msg, ...
  )
}

loggingIsOn <- function() {
  compilerOptions <- get_nOption('compilerOptions')
  logging <- compilerOptions$logging
  return(logging)
}
