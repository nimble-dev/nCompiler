# Code for serializing and de-serializing loaded nComp objects

# Currently this only works for one DSL at a time.
# We need to manage multiple DSLs somehow.
# One approach would be to store a label in an environment enclosing all the loadedObjetEnv environments.

#' @export
serialize_nComp_object <- function(obj) {
  if(!is.loadedObjectEnv(obj))
    stop("obj must be a loadedObjectEnv.")
  if(is.null(getExtptr(obj)))
    warning("No nComp serialization to be done.")
  else {
    loadedObjectEnv_serialized(obj) <- nComp_serialize(getExtptr(obj))
    setExtptr(obj, NULL)
  }
  obj
}

#' @export
deserialize_nComp_object <- function(obj) {
  if(!is.loadedObjectEnv(obj))
    stop("obj must be a loadedObjectEnv.")
  if(!(class(loadedObjectEnv_serialized(obj))[1] == "raw"))
    stop("serialized content must have class 'raw'")
  newXptr <- nComp_deserialize(loadedObjectEnv_serialized(obj))
  setExtptr(obj, newXptr)
  ## Could be a little dangerous to clear the serialized data,
  ## but in a typical use case it would still be saved.
  loadedObjectEnv_serialized(obj) <- NULL
  obj
}
