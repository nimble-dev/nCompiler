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
