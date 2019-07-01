## Class for constructors of nClasses

#' @export
nConstructor <- function(..., initializerList ) {
  ans <- nFunction(...)
  NFinternals(ans)$aux <- list(initializerList = initializerList)
  NFinternals(ans)$returnSym <- symbolBlank$new()
  ans
}
