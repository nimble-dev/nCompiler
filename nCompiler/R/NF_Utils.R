#' Check if an object is a nFunction
#' @param x Object to be checked.
#' @return \code{TRUE} or \code{FALSE}
#' @export
isNF <- function(x) inherits(x, 'nFunction')

#' Get or set internals of a nFunction (advanced use only).
#'
#' @aliases `NFinternals<-`
#' @param NF A \code{nFunction}.
#' @param value Object of class \code{NF_InternalsClass}.
#' @export
NFinternals <- function(NF) {
    NF@internals
}

#' @rdname NFinternals
#' @export
`NFinternals<-` <- function(NF, value) {
    NF@internals <- value
    NF
}

isConstructor <- function(NF) {
  if(!isNF(NF)) return(FALSE)
  if(is.null(NFinternals(NF)$aux)) return(FALSE)
  if(is.null(NFinternals(NF)$aux$initializerList)) return(FALSE)
  TRUE
}