#' Determine if an object is a nClass object
#'
#' Determine if an object is a nClass object, returned by a nClass generator.
#'
#' @param x Object to be inspected.
#'
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @export
isNC <- function(x) inherits(x, 'nClass')

#' Determine if an object is a nClass generator
#'
#' Determine if an object is a nClass generator, returned by a call to \link{nClass}.
#'
#' @param x Object to be inspected
#'
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @export
isNCgenerator <- function(x) {
  if(inherits(x, "R6ClassGenerator"))
    exists(".nCompiler", x)
  else
    FALSE
}

#' Determine if an object is a compiled nClass generator
#'
#' Determine if an object is a compiled nClass generator, returned by a call to \link{nCompile_nClass} with \code{interface} set to \code{"full"} or \code{"both"}.
#'
#' @param x Object to be inspected
#'
#' @return \code{TRUE} or \code{FALSE}
#' 
#' @export
isCompiledNCgenerator <- function(x) {
  if(inherits(x, "R6ClassGenerator"))
    exists(".newCobjFun", x)
  else
    FALSE
}

#' Access internal information of a nClass object
#'
#' Only for advanced use.
#'
#' @param x A nClass object.
#' 
#' @export
NCinternals <- function(x) {
  if(isNC(x))
    stop("NCinternals for an nClass object is not supported.") # deprecated?: parent.env(x)$.NCinternals
  else if(isNCgenerator(x))
    x$.nCompiler
  else
    stop(paste('Invalid input to NCinternals.  Argument x is of invalid class',
               paste(class(x), collapse = ',')),
         call. = FALSE)
}

#' Set internal information of a nClass object
#'
#' Only for advanced use.
#'
#' @param x A nClass object.
#' 
#' @export
`NCinternals<-` <- function(x, value) {
  if(isNC(x))
    stop("NCinternals<- for an nClass object is not supported.") # deprecated?: parent.env(x)$.NCinternals <- value
  else if(isNCgenerator(x))
    x$.nCompiler <- value
  else
    stop(paste('Invalid input to NCinternals<-.  Argument x is of invalid class',
               paste(class(x), collapse = ',')),
         call. = FALSE)
  x
}
