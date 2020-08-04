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
#' @param ncDef The uncompiled nClass generator used to define ncObj's nClass.
#'   Leave NULL and use the argument \code{packageWithDefn} if the nClass is
#'   defined in an existing package.
#' @param file The filename to which the serialized object will be written as
#'   RDS, probably with extension ".rds"
#' @param packageWithDefn If one exists, the name of the previously defined
#'   package that defines the nClass (character string). Leave NULL if no such
#'   package exists in which case a small package will be created. If NULL, an
#'   ncDef object must be provided.
#' @param classname If no ncDef is provided, a character string giving the class
#'   name of the ncObj. If ncDef is provided, can be left NULL.
#' @param dir The directory in which the package will be written. Ignored if
#'   packageWithDefn is not NULL.
#' @param lib The lib folder where the package will be quietly installed.
#'   Ignored if packageWithDefn is not NULL.
#' @export
#' 
#' @details
#' 
#' There are two parameterizations of this argument.
#' If you've defined an nClass and want to save it as an instance, use:
#' \code{
#' save_nClass(ncObj, file, ncDef)
#' }
#' In this case, a package storing the nClass definition will be created. This
#' allows the nClass info to be restored with the specific instantiated object
#' upon reading.
#' 
#' If the nClass is defined in a package, and so will be found even in a new
#' (fresh) session, use:
#' \code{
#' save_nClass(ncObj, file, packageWithDefn, classname)
#' }
#'
#' In this case
save_nClass <- function(ncObj,
                        file,
                        ncDef = NULL,
                        packageWithDefn = NULL,
                        classname = NULL,
                        dir = tempdir(),
                        lib = .libPaths()[1]) {
  if (isFALSE(get_nOption("serialize"))) {
    set_nOption("serialize", TRUE)
    on.exit(set_nOption("serialize", FALSE))
  }
  if (missing(lib)) lib <- .libPaths()[1]
  if (is.null(packageWithDefn)) {
    if (is.null(ncDef)) {
      stop("In save_nClass, must provide either 'ncDef' or 'packageWithDefn'.")
    }
    package.name <- paste0("savePackageClass", 
                           gsub(pattern = "_", "", 
                                Rname2CppName(ncDef$classname)))
    createPackage <- TRUE
  } else if (is.character(packageWithDefn)) {
    package.name <- packageWithDefn
    createPackage <- FALSE
  } else {
    stop("In save_nClass, packageWithDefn must be a character string")
  }
  
  if (is.null(classname)) {
    if (is.null(ncDef))
    stop(paste0("In save_nClass, if no 'ncDef' is provided, the class name",
                " must be provided", "\n  as a character string"))
    else classname <- ncDef$classname
  }
  
  if (createPackage) {
    serialize_fn <- get(paste0("nComp_serialize_", 
                              Rname2CppName(classname)))
  } else {
    serialize_fn <- utils::getFromNamespace(
      paste0("nComp_serialize_", Rname2CppName(classname)), 
      package.name
    )
  }
  
  if (is.loadedObjectEnv(ncObj)) {
    serialized <- serialize_nComp_object(ncObj, serialize_fn)
    listToSerialize <- list(CppObj = serialized$serialized, 
                            constructedPackage = TRUE,
                            classname = classname,
                            package.name = package.name)
    saveRDS(listToSerialize, file)
    
  } else if (isNC(ncObj)) {
    serialized <- serialize_nComp_object(ncObj$private$CppObj, serialize_fn)
    listToSerialize <- list(full = ncObj, 
                            CppObj = serialized$serialized,
                            constructedPackage = TRUE,
                            classname = classname,
                            package.name = package.name)
    
    saveRDS(listToSerialize, file)
    
  } else {
    stop("Object to save, 'ncObj', must be an instance of an nClass.")
  }
  
  if (createPackage) {
    writePackage(ncDef,
                 package.name = package.name,
                 dir = dir,
                 control = list(export = FALSE), 
                 modify = FALSE,
                 memberData = list(classname = classname))
    buildPackage(package.name = package.name, 
                 dir = dir, lib = lib, load = FALSE)
  }
  invisible(NULL)
}


#' @name read_nClass
#' @title Read an an nClass object saved by \code{save_nClass}
#' @param file The (probably .rds) file to which the nClass object was written
#' @param lib The lib folder where the package defining the nClass is found
#' @export
read_nClass <- function(file, lib = .libPaths()[1]) {
  
  savedObj <- readRDS(file)
  if (!is.list(savedObj) || is.null(savedObj$CppObj)) {
    stop("Object in specified RDS file is not a saved nClass.")
  }
  
  if (!is.null(savedObj$full)) {
    serialized <- new.loadedObjectEnv(serialized = savedObj$CppObj)
    
    # library(savedObj$package.name, character.only = TRUE, lib = lib)
    # loadEnv <- new.env()
    # data(list = "classname", package = savedObj$package.name, envir = loadEnv)
    deserialize_fn <- utils::getFromNamespace(
      paste0("nComp_deserialize_", Rname2CppName(savedObj$classname)), 
      savedObj$package.name
    )
    deserialized <- deserialize_nComp_object(
      serialized, nComp_deserialize_fn = deserialize_fn)
    
    rtnObjFull <- savedObj$full
    rtnObjFull$private$CppObj <- deserialized
    
    return(rtnObjFull)
    
  } else {
    serialized <- new.loadedObjectEnv(serialized = savedObj$CppObj)
    
    # library(savedObj$package.name, character.only = TRUE, lib = lib)
    # loadEnv <- new.env()
    # data(list = "classname", package = savedObj$package.name, envir = loadEnv)
    
    deserialize_fn <- utils::getFromNamespace(
      paste0("nComp_deserialize_", Rname2CppName(savedObj$classname)), 
      savedObj$package.name
    )
    deserialized <- deserialize_nComp_object(
      serialized, nComp_deserialize_fn = deserialize_fn)
    return(deserialized)
  }
}


