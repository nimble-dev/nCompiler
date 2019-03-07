## Interface functions that take an external pointer "obj" as an argument.
## NC_FullCompiledInterface for creation of a class that holds the "obj"
##   pointer and provides objects with active binds to use it.

#' @export
method <- function(obj, name) {
  function(...) {
    if(is.null(getExtptr(obj)))
      stop("obj does not point to a C++ object.")
    nCompiler:::call_method(getExtptr(obj), name, list(...))
  }
}

#' @export
value <- function(obj, name) {
  if(is.null(getExtptr(obj)))
    stop("obj does not point to a C++ object.")
  nCompiler:::get_value(getExtptr(obj), name)
}

#' @export
`value<-` <- function(obj, name, value) {
  if(is.null(getExtptr(obj)))
    stop("obj does not point to a C++ object.")
  nCompiler:::set_value(getExtptr(obj), name, value)
  obj
}
