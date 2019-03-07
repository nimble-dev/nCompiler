nErrorEnv <- new.env()
nDebugEnv <- new.env()

## Set nErrorEnv$stateInfo to cache state information (character)
## for display in the event of an error.

getCompilerLog <- function(echo = FALSE) {
  out <- paste(nDebugEnv$compilerLog, collapse = '\n ')
  if (echo) cat(out)
  invisible(out)
}
