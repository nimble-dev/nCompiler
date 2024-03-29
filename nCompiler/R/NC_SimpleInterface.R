## Interface functions that take an external pointer "obj" as an argument.
## NC_FullCompiledInterface for creation of a class that holds the "obj"
##   pointer and provides objects with active binds to use it.

#' @export
method <- function(obj, name) {
  function(...) {
    if(is.null(getExtptr(obj)))
      stop("obj does not point to a C++ object.")
    # We switched from list(...) to environment() with the switch to having
    # the C++ generic interface look up ... from the calling environment()
    # in order to implement ref and blockRef behavior.
    nCompiler:::call_method(getExtptr(obj), name, environment())
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
