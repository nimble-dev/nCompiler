# Code for serializing and de-serializing loaded nComp objects

# Currently this only works for one DSL at a time.
# We need to manage multiple DSLs somehow.
# One approach would be to store a label in an environment enclosing all the loadedObjetEnv environments.

serialize_nComp_object <- function(obj, nComp_serialize_fn) {
  if(!is.loadedObjectEnv(obj))
    stop("obj must be a loadedObjectEnv.")
  if(is.null(getExtptr(obj))) {
    warning("No nComp serialization to be done.")
    return(obj)
  } else {
    newObj <- new.loadedObjectEnv(extptr = obj$extptr, serialized = obj$serialized)
    loadedObjectEnv_serialized(newObj) <- nComp_serialize_fn(getExtptr(newObj))
    setExtptr(newObj, NULL)
    return(newObj)
  }
}

deserialize_nComp_object <- function(obj, nComp_deserialize_fn) {
  if(!is.loadedObjectEnv(obj))
    stop("obj must be a loadedObjectEnv.")
  if(!(class(loadedObjectEnv_serialized(obj))[1] == "raw"))
    stop("serialized content must have class 'raw'")
  newXptr <- nComp_deserialize_fn(loadedObjectEnv_serialized(obj))
  setExtptr(obj, newXptr)
  ## Could be a little dangerous to clear the serialized data,
  ## but in a typical use case it would still be saved.
  loadedObjectEnv_serialized(obj) <- NULL
  obj
}




#' @name save_nClass
#' @title Save an instance of an nClass object across sessions
#' @description TBW
#' @param ncObj A compiled instance of an nClass object. Right now this only
#'   works for
#' @param ncDef The uncompiled nClass generator used to define ncObj's nClass
#' @param file The filename to which the serialized object will be written as
#'   RDS, probably with extension ".rds"
#' @param package.name The name of the package that'll be generated to store the
#'   nClass definition (probably not going to be a part of the final product)
#' @param dir The directory in which the package will be written (probably not
#'   going to be a part of the final product)
#' @param lib The lib folder where the package will be quietly installed
#'   (probably not going to be a part of the final product)
#' @export
save_nClass <- function(ncObj, ncDef,
                        file, package.name,
                        dir = tempdir(), lib = .libPaths()[1]) {
  if (isFALSE(get_nOption("serialize"))) {
    set_nOption("serialize", TRUE)
    on.exit(set_nOption("serialize", FALSE))
  }
  
  if (missing(lib)) lib <- .libPaths()[1]
  
  serializeFn <- get(paste0("nComp_serialize_", ncDef$classname))
  serialized <- serialize_nComp_object(ncObj, serializeFn)
  serializedList <- mget(c("serialized", "extptr"), envir = serialized)
  saveRDS(serialized$serialized, file)
  
  writePackage(ncDef,
               package.name = package.name,
               dir = dir,
               control = list(export = FALSE), 
               modify = FALSE,
               memberData = list(classname = ncDef$classname))
  buildPackage(package.name = package.name, 
               dir = dir, lib = lib, load = FALSE)
  
  invisible(NULL)
}


#' @name read_nClass
#' @title Read an an nClass object saved by \code{save_nClass}
#' @param file The (probably .rds) file to which the nClass object was written
#' @param package.name The name of the package to which the nClass
#' @param dir The directory in which the package was written (probably not going
#'   to be a part of the final product)
#' @param lib The lib folder where the package will be quietly installed
#'   (probably not going to be a part of the final product)
#' @export
read_nClass <- function(file, package.name,
                        dir = tempdir(), lib = .libPaths()[1]) {
  
  serialized <- new.loadedObjectEnv(serialized = readRDS(file))
  
  library(package.name, character.only = TRUE, lib = lib)
  loadEnv <- new.env()
  data(list = "classname", package = package.name, envir = loadEnv)
  
  deserialize_fn <- utils::getFromNamespace(
    paste0("nComp_deserialize_", Rname2CppName(loadEnv$classname)), 
    package.name
  )
  deserialized <- deserialize_nComp_object(
    serialized, nComp_deserialize_fn = deserialize_fn)
  return(deserialized)
}
