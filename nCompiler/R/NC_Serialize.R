# Code for serializing and de-serializing loaded nComp objects

#' @export
nSerialize <- function(obj) {
  # obj might be a list or other compound object, but only one object

  DLLenv <- NULL
  ser_mgr <- NULL
  add_extptr <- NULL
  find_extptr <- NULL
  packageName <- NULL

  # function for refhook argument of R's serialize
  refhook <- function(ref_obj) {
    # ref_obj will typically be an environment or an externalptr
    # We catch loadedObjectEnv environment's and use them to find
    # the DLLenv and create a serialization manager object,
    # but then we return NULL to indicate this function
    # is not serializing them.
    # Then R's serialize will call the refhook again with the externalptr
    # which we WILL serialize.
    if(isTRUE(nCompiler:::is.loadedObjectEnv(ref_obj))) {
      # First time that we get a loadedObjectEnv, we'll use it for the DLLenv
      # to get the serialization-related calls to C++.
      # Future extension: Allow multiple DLLenvs in one nSerialize call.
      if(is.null(ser_mgr)) {
        DLLenv <<- nCompiler:::get_DLLenv(ref_obj)
        new_smgr_fn <- DLLenv$new_serialization_mgr
        if(!is.function(new_smgr_fn)) stop("nCompiler serialization manager not found.")
        ser_mgr <<- new_smgr_fn()
        # add_extptr will be used on subsequent calls to refhook for an externalptr
        add_extptr <<- function(extptr)
          DLLenv$call_method(ser_mgr$extptr, "add_extptr", list(extptr))
        find_extptr <<- function(ref_obj)
          DLLenv$call_method(ser_mgr$extptr, "find_extptr", list(ref_obj))
      }
      if(is.null(packageName)) {
        packageName <<- DLLpackageName(ref_obj)
      }
      ID <- add_extptr(ref_obj$extptr)
      NULL # must return NULL to say we are not serializing the environment
    } else if("externalptr" %in% class(ref_obj)) {
      if(is.null(ser_mgr))
        stop("encountered an externalptr before serialization manager set up.")
      ID <- find_extptr(ref_obj)
      if(is.null(ID)) NULL # return NULL if we are not serializing this
      else as.character(ID) # this is the returned object for R's serialize if we are,
      # which requires a string
    } else
      NULL # This tell's R's serialize that we don't want to handle ref_obj
  }
  # The steps are:
  # 1. Serialize the object, using the refhook to end up with a
  #    C++ serialization_mgr object that contains smart pointers to any
  #    C++ objects.
  Rside <- serialize(obj, connection=NULL, refhook = refhook)
  CPPside <- NULL
  if(!is.null(DLLenv)) {
    # 2a. If there were any C++ objects,
    #     serialize the serialization_mgr, which will include the pointed-to objects
    ser_fn <- DLLenv$nComp_serialize_
    if(!is.function(ser_fn)) stop("nCompiler serialization function not found")
    CPPside <- ser_fn(nCompiler:::getExtptr(ser_mgr))
    # 2b. Clear the serialization_mgr of its pointers (underlying objects are still fine)
    (\() DLLenv$call_method(ser_mgr$extptr, "clear", list()))()
  }
  # 3. We need one serialization result, so now serialized the two pieces together.
  combined <- serialize(list(Rside=Rside, CPPside=CPPside, packageName = packageName),
                        connection=NULL)
  # 4. Delete the serialization manager (will be finalized when R's gc() next runs)
  rm(ser_mgr)
  combined
}

# nUnserialize assumes the DLL is correctly loaded again,
# as from a pacakge.
#' @export
nUnserialize <- function(obj, pkgName, lib = NULL) {
  # The steps reverse those of nSerialize:
  # 1. separate the Rside and CPPside
  # pkgName can be an environment such as DLLenv or a package name. It's where to look for nComp_deserialize_

  combined <- unserialize(obj)

  if(missing(pkgName)) {
    pkgName <- combined$packageName
  }
  if(is.environment(pkgName)) DLLenv <- pkgName
  else {
    if(!is.character(pkgName) || (length(pkgName)>1))
      stop("pkgName must be an environment or one character string.")
    if(!isNamespaceLoaded(pkgName)) {
      load_ok <- requireNamespace(pkgName, quietly=TRUE)
      if(!load_ok)
        stop("Namespace ", pkgName, " was not already loaded and couldn't be found and loaded.")
    }
    DLLenv <- getNamespace(pkgName)
    ## pkgString <- pkgName
    ## if(!substr(pkgString, 1, 8) == "package:")
    ##   pkgString <- paste0("package:", pkgName)
    ## if(is.na(match(pkgString, search()))) {
    ##   load_ok <- require(pkgName, lib.loc = lib)
    ##   if(!load_ok)
    ##     stop("Package ", pkgName, " was not already loaded and couldn't be found and loaded.")
    ## }
    ## DLLenv <- as.environment(pkgString)
  }
  unser_fn <- DLLenv$nComp_deserialize_
  if(!is.function(unser_fn))
    stop("Could not find 'nComp_deserialize_' function in pkgName environment or package namespace.")
  if(!is.null(combined$CPPside)) {
    # 2. If there was anything on the C++ side, make a serialization_mgr
    ser_mgr <- unser_fn(combined$CPPside)
    # get_extptr will be used in refhook, when called by R's serialize()
    get_extptr <- \(i) DLLenv$call_method(ser_mgr$extptr, "get_extptr", list(i))

    refhook <- function(deref_obj) {
      # This will be called with a character string of an integer ID created
      # by the refhook function in nSerialize.
      # We turn it into a number and get the objects as an externalptr from C++
      ID <- as.numeric(deref_obj)
      res <- get_extptr(ID)
      res
    }
    # 3. Call R's unserialize with the refhook function to use the serialization_mgr
    #    to get C++ objects.
    result <- unserialize(combined$Rside, refhook = refhook)
    # 4. Clear the serialization_mgr (objects just created will persist because)
    #    They are now pointed to from externalptr's in R.
    (\() DLLenv$call_method(ser_mgr$extptr, "clear", list()))()
    # 5. Remove the serialization_mgr, so it will be finalized next time R's gc() is called.
    rm(ser_mgr)
  } else {
    # CPP side was empty, so just unserialize R side
    result <- unserialize(combined$Rside)
  }
  result
}

################################################################
### EVERYTHING BELOW HERE IS OLD AND PROBABLY WILL NOT BE USED
### I am leaving it in place during the development moment of adding
### above

# Internal function used by save_nClass() which calls the provided serialization
# function, then instantiates and returns a new loadedObjectEnv with the
# contents

get_serialize_fun <- function(obj) {
  parent.env(obj)$nComp_serialize_
}

get_deserialize_fun <- function(obj) {
  parent.env(obj)$nComp_deserialize_
}

serialize_nComp_object <- function(obj, serializer) {
  if(!is.loadedObjectEnv(obj))
    stop("obj must be a loadedObjectEnv.")
  if(is.null(getExtptr(obj))) {
    warning("No nComp serialization to be done.")
    return(obj)
  } else {
    if(missing(serializer)) {
      serializer <-get_serialize_fun(obj)
      if(!is.function(serializer))
        stop("Function for serializing not found not found.")
    }
    serial_data <- serializer(getExtptr(obj))
    
    newObj <- new.serialObjectEnv(serial_data, get_DLLenv(obj))
    return(newObj)
  }
}

# Internal function used by read_nClass() which calls the provided
# deserialization function and applies it to a loadedObjectEnv
deserialize_nComp_object <- function(obj, deserializer) {
  if(!is.serialObjectEnv(obj))
    stop("obj must be a serialObjectEnv.")
  if(!(class(obj$serial) == "raw"))
      stop("serialized content must have class 'raw'")
  if(missing(deserializer)) {
    deserializer <- get_deserialize_fun(obj)
    if(!is.function(deserializer))
      stop("Function for serializing not found not found.")
  }
  newXptr <- deserializer(obj$serial)
  newObj <- new.loadedObjectEnv(newXptr)
  parent.env(newObj) <- parent.env(obj)
  newObj
}

#' @name save_nClass
#' @title Save an instance of an nClass object across sessions
#' @description Saves an nClass, including its compiled components, across
#'   sessions using the `cereal` serialization library for C++.
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
#' @param dir The directory in which the package directory will be created and
#'   source code will be written. Ignored if packageWithDefn is not NULL.
#' @param lib The lib folder where the package will be quietly installed.
#'   Ignored if packageWithDefn is not NULL.
#' @export
#'
#' @details
#'
#' There are two parameterizations of this function. If you've defined an nClass
#' and want to save it as an instance, use:
#'
#' \code{ save_nClass(ncObj, file, ncDef) }
#'
#' In this case, a package storing the nClass definition will be created. This
#' allows the nClass info to be restored with the specific instantiated object
#' upon reading.
#'
#' If the nClass is defined in a package, and so will be found even in a new
#' (fresh) session, use:
#'
#' \code{ save_nClass(ncObj, file, packageWithDefn, classname) }
#'
#' @examples
#' set_nOption("serialize", TRUE)
#'
#' # Create a new nClass
#' nc1 <- nClass(
#'   classname = "nc1",
#'   Cpublic = list(
#'     Cv = 'numericScalar',
#'     Cx = 'integerScalar',
#'     Cfoo = nFunction(
#'       fun = function(x) {
#'         return(x+1)
#'       },
#'       argTypes = list(x = 'numericScalar'),
#'       returnType = 'numericScalar')
#'   )
#' )
#' 
#' # Compile the nClass
#' Cnc1 <- nCompile_nClass(nc1, interface = "full")
#' 
#' # Instantiate an object of the nClass
#' my_nc1_instance <- Cnc1$new()
#' my_nc1_instance$Cv <- 10
#' 
#' # Save the instance (along with its class definition)
#' save_nClass(my_nc1_instance, file = "example_save.Rds", ncDef = nc1)
#' 
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
  
  ## if (createPackage) {
  ##   serialize_fn <- get_serialize_fun(ncObj)
  ## } else {
  ##   serialize_fn <- utils::getFromNamespace(
  ##     "nComp_serialize_" 
  ##     package.name
  ##   )
  ## }
  
  if (is.loadedObjectEnv(ncObj)) {
    serialized <- serialize_nComp_object(ncObj)
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
#' @title Read an nClass object saved by \code{save_nClass}
#' @description Read an instance of an nClass saved by \code{save_nClass}.
#'   The class definition and deserialization tools (using the \code{cereal}
#'   library for C++) are loaded from the relevant package, whether it was
#'   written or generated automatically from \code{save_nClass}.
#' @param file The (probably .rds) file to which the nClass object was written
#' @param lib The lib folder where the package defining the nClass is found
#' @export
#' 
#' @examples
#' set_nOption("serialize", TRUE)
#'
#' # Create a new nClass
#' nc1 <- nClass(
#'   classname = "nc1",
#'   Cpublic = list(
#'     Cv = 'numericScalar',
#'     Cx = 'integerScalar',
#'     Cfoo = nFunction(
#'       fun = function(x) {
#'         return(x+1)
#'       },
#'       argTypes = list(x = 'numericScalar'),
#'       returnType = 'numericScalar')
#'   )
#' )
#' 
#' # Compile the nClass
#' Cnc1 <- nCompile_nClass(nc1, interface = "full")
#' 
#' # Instantiate an object of the nClass
#' my_nc1_instance <- Cnc1$new()
#' my_nc1_instance$Cv <- 10
#' 
#' # Save the instance (along with its class definition)
#' save_nClass(my_nc1_instance, file = "example_save.Rds", ncDef = nc1)
#' 
#' ### A new session can be started here
#' my_nc1_read <- read_nClass("example_save.Rds")
#' my_nc1_read$Cv
#' my_nc1_read$Cfoo(10)
#' 
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


