## Interface functions that take an external pointer "obj" as an argument.
## NC_FullCompiledInterface for creation of a class that holds the "obj"
##   pointer and provides objects with active binds to use it.

#' @export
method <- function(obj, name) {
  if(inherits(obj, "CnClass"))
    obj <- obj$private$CppObj
  CnCenv <- get_CnCenv(obj)
  ans <- CnCenv[[name]]
  environment(ans) <- new.env(parent = environment(ans))
  environment(ans)$CppObj_ <- obj
  ans
  ## Previous version #1
  ## force(name)
  ## function(...) {
  ##   CnCenv[[name]](obj, ...)
  ## }
  ## Previous version #2
  ## DLLenv <- nCompiler:::get_DLLenv(obj)
  ## extptr <- nCompiler:::getExtptr(obj)
  ## function(...) {
  ##   DLLenv$call_method(extptr, name, environment())
  ##   # We switched from list(...) to environment() with the switch to having
  ##   # the C++ generic interface look up ... from the calling environment()
  ##   # in order to implement ref and blockRef behavior.
  ##   #
  ##   # We also switched to having each DLL hold its own R interface fxns
  ## }
}

#' @export
value <- function(obj, name) {
  if(inherits(obj, "CnClass"))
    obj <- obj$private$CppObj
  DLLenv <- get_DLLenv(obj)
  extptr <- getExtptr(obj)
  DLLenv$get_value(extptr, name)

  ## if(is.null(getExtptr(obj)))
  ##   stop("obj does not point to a C++ object.")
  ## nCompiler:::get_value(getExtptr(obj), name)
}

#' @export
`value<-` <- function(obj, name, value) {
  if(inherits(obj, "CnClass"))
    obj <- obj$private$CppObj
  DLLenv <- get_DLLenv(obj)
  extptr <- getExtptr(obj)
  DLLenv$set_value(extptr, name, value)
  obj
  ## if(is.null(getExtptr(obj)))
  ##   stop("obj does not point to a C++ object.")
  ## nCompiler:::set_value(getExtptr(obj), name, value)
  ## obj
}
