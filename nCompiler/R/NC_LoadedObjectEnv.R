## A loadedObjectEnv is simply an environment that holds
## an external pointer to a C++ object and/or a serialization
## of that object.  This facilitates saving and loading via
## R's serialization mechanisms.  One will first need to 
## explicitly serialize the C++ objects via a simple
## interface.  Then they can be saved as regular R
## objects.  Since R's saving and loading serialization
## manages environments correctly (i.e. without redundantly 
## saving multiple copies), the same will be true for 
## the serialized contents of nClass objects.

## The system could be organized using R6 classes or the like,
## but we do it simply with environments to keep it as light
## and low-level as possible for speed and memory efficiency.

new.loadedObjectEnv <- function(extptr = NULL) {
  ans <- new.env()
  ans$extptr <- extptr
  class(ans) <- "loadedObjectEnv"
  ans
}

is.loadedObjectEnv <- function(env) {
  ## The checks here may be over-kill.
  ## We may be able to rely solely on the class label.
  if(!is.environment(env)) return(FALSE)
  if(!exists("extptr", where = env)) return(FALSE)
  if(class(env) != "loadedObjectEnv") return(FALSE)
  TRUE
}

getExtptr <- function(env) {
    ## If env$extptr ever changes, the C++ code for as< std::shared_ptr< T > > should also be changed.
    ## This is written as a custom Exporter added to namespace Rcpp::traits
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$extptr
}

setExtptr <- function(env, xptr) {
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$extptr <- xptr
  env
}

make_DLLenv <- function() {
  ans <- new.env(parent = getNamespace("nCompiler"))
  class(ans) <- "nC_DLL_env"
  ans
}

get_DLLenv <- function(obj) {
  parent.env(obj)
}


# Filters compiled function list for specified names of helper functions pertaining to DLL.
# Filtered-out functions moved to new DLL environment.
# Returns filtered list or singleton.
setup_DLLenv <- function(ans, newDLLenv, returnList = FALSE) {
  if (!is.list(ans))
    return(ans) # Why return on singleton?

  # Serialization-specific:  should be initialized elsewhere, according to options.
  namesForDLLenv <- getSerialFunNames()

  keep <- rep(TRUE, length(ans))
  for(DLLname in namesForDLLenv) {
    found <- grepl(DLLname, names(ans))
    if(any(found)) {
      i <- which(found)
      if(length(i) != 1)
        stop(paste("Compilation produces multiple instances of ", DLLname));
      keep[i] <- FALSE
      newDLLenv[[DLLname]] <- ans[[i]]
    }
  }

  if (!all(keep)) # unneeded guard
    ans <- ans[keep]

  if (length(ans) != 1 || returnList)
    ans
  else
    ans[[1]]
}


## returns a wrapper function causing the LOE returned by 'newObjFun' to
## receive 'newDLLenv' as its parent environment.
wrapNCgenerator_for_DLLenv <- function(newObjFun, newDLLenv) {
  force(newDLLenv)
  force(newObjFun)
  if(!is.function(newObjFun))
    stop(paste0("newObjFun is not a function. It is a ", 
                paste0(class(newObjFun), collapse = " ")))

  # Wrapper function assigning the parent environment of the returned LOE to be 'newDLLenv'
  wrappedNewObjFun <- function() {
    ans <- newObjFun()
    parent.env(ans) <- newDLLenv
    ans
  }
  wrappedNewObjFun
}

new.serialObjectEnv <- function(serial_data = NULL, dll_env) {
  ans <- new.env()
  if(!missing(dll_env)) parent.env(ans) <- dll_env
  ans$serial <- serial_data
  class(ans) <- "serialObjectEnv"
  ans
}

is.serialObjectEnv <- function(env) {
  ## The checks here may be over-kill.
  ## We may be able to rely solely on the class label.
  if(!is.environment(env)) return(FALSE)
  if(!exists("serial", where = env)) return(FALSE)
  if(class(env) != "serialObjectEnv") return(FALSE)
  TRUE
}

getSerial <- function(env) {
  ## If env$extptr ever changes, the C++ code for as< std::shared_ptr< T > > should also be changed.
  ## This is written as a custom Exporter added to namespace Rcpp::traits
  if(!is.serialObjectEnv(env))
    stop("env should be a serialObjectEnv")
  env$serial
}

setSerial <- function(env, serial_data) {
  if(!is.serialObjectEnv(env))
    stop("env should be a serialObjectEnv")
  env$serial <- serial_data
  env
}

## Next two will be deprecated

loadedObjectEnv_serialized <- function(env) {
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$serialized
}

`loadedObjectEnv_serialized<-` <- function(env, value) {
  if(!is.loadedObjectEnv(env))
    stop("env should be a loadedObjectEnv")
  env$serialized <- value
  env
}
  
