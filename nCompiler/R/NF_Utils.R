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

## This utility function is like `get`, but it
## also handles the case that where is an R6Generator.
## In that case, it first looks in the R6Generator,
## which is itself an environment, and then looks up
## the parent_env element of the R6Generator, rather than
## its actual parent.env.  The parent_env element is what
## objects created by the generator have as their
## parent environment.  nClass sets that to the environment
## where nClass was called, thus creating lexical scoping.
## That is the same scoping of interest for nGet.
## There is no need for an "inherits" argument, because
## if inherits = FALSE there would be no need to call nGet
## (simply `get` would work).
## If the name is not found, NULL is returned.
nGet <- function(name, where) {
  if(inherits(where, "R6ClassGenerator")) {
    if(exists(name, envir = where, inherits = FALSE))
      return(get(name, envir = where, inherits = FALSE))
    ## If not found in the class generator,
    ## switch where to be the generator's parent_env
    where <- where$parent_env
  }
  if(exists(name, envir = where, inherits = TRUE))
    get(name, envir = where, inherits = TRUE)
  else
    NULL
}
