## Interface functions that take an external pointer "obj" as an argument.
## NC_FullCompiledInterface for creation of a class that holds the "obj"
##   pointer and provides objects with active binds to use it.

#' @export
method <- function(obj, name) {
  if(inherits(obj, "nClass"))
    if(obj$isCompiled())
      obj <- obj$private$Cpublic_obj$private$CppObj # obj$private$CppObj
    else 
      stop("method() can only be used on compiled nClass objects.")
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
  if(inherits(obj, "nClass"))
    if(obj$isCompiled())
      obj <- obj$private$Cpublic_obj$private$CppObj # obj$private$CppObj
    else 
      stop("value() can only be used on compiled nClass objects.")
  DLLenv <- get_DLLenv(obj)
  extptr <- getExtptr(obj)
  DLLenv$get_value(extptr, name)
}

#' @export
`value<-` <- function(obj, name = NULL, value) {
   if(inherits(obj, "nClass"))
    if(obj$isCompiled())
      obj <- obj$private$Cpublic_obj$private$CppObj # obj$private$CppObj
    else 
      stop("value<-() can only be used on compiled nClass objects.")
  DLLenv <- get_DLLenv(obj)
  extptr <- getExtptr(obj)
  DLLenv$set_value(extptr, name, value)
  obj
}
