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

new.loadedObjectEnv <- function(extptr = NULL, parentEnv = NULL) {
  ans <- new.env()
  if(!is.null(parentEnv)) 
    if(!identical(parentEnv, .GlobalEnv)) #.GlobalEnv is default of an Rcpp::Environment object
      parent.env(ans) <- parentEnv
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
  class(ans) <- "nComp_DLL_env"
  ans
}

get_DLLenv <- function(obj) {
  parent.env(obj)
}

setup_CnC_environments <- function(compiledFuns,
                                   newDLLenv,
                                   nC_names = character(),
                                   R6interfaces,
                                   returnList = FALSE) {
  compiledFuns <- setup_DLLenv(compiledFuns, newDLLenv, returnList = FALSE)
  for(nC_name in nC_names) {
    compiledFuns <- setup_CnClass_env(nC_name, compiledFuns, R6interfaces, newDLLenv)
  }
  if(is.list(compiledFuns)) compiledFuns
  else if(returnList) list(compiledFuns)
  else compiledFuns
}

setup_CnClass_env <- function(nC_name,
                              compiledFuns,
                              R6interfaces,
                              DLLenv) {
  set_nClass_env_name <- paste0("set_CnClass_env_", nC_name)
  fun_names <- c(set_nClass_env_name)
  
  CnClass_env <- new.env(parent = DLLenv)
  class(CnClass_env) <- "CnClass_env"
  
  compiledFuns <- move_funs_from_list_to_env(fun_names, compiledFuns,
                                             CnClass_env)
  if(exists(set_nClass_env_name, envir = CnClass_env, inherits=FALSE)) {
    CnClass_env[[set_nClass_env_name]](CnClass_env)
  }
  CnClass_env$.R6interface <- R6interfaces[[nC_name]]
  compiledFuns
}

# This function takes as input the results of a call to sourceCpp (compiledFuns)
# and an environment to use as the DLL environment.
# 
# The DLL environment will hold DLL-level compiled functions, such as those
# for serialization and deserializtion.
#
# compiled nClass environments (CnCenv) will have a DLL env as their 
# parent environment.
#
# Individual nClass object extptr's will be in a loadedObjectEnv,
# which will have a CnCenv as the parent environment.
#
# What this function does is remove known DLL-level functions from
# the compiledFuns and place them in the newDLLenv.
#
# It returns an updated compiledFuns list, or function
# if there is only one left after removing DLL functions
# and if returnList == FALSE.
setup_DLLenv <- function(compiledFuns, 
                         newDLLenv,
                         returnList = FALSE) {
  # If there is any serialization etc., compiledFuns must be a list.
  # Hence, if compiledFuns is not a list, there can't be any DLL funs
  # to move to the DLL env.
  if(!is.list(compiledFuns)) return(compiledFuns) 
  namesForDLLenv <- c("nComp_serialize_",
                      "nComp_deserialize_",
                      "new_serialization_mgr")
  
  compiledFuns <- move_funs_from_list_to_env(namesForDLLenv,
                                             compiledFuns, newDLLenv)
  
  # keep <- rep(TRUE, length(compiledFuns))
  # for(DLLname in namesForDLLenv) {
  #   found <- grepl(DLLname, names(compiledFuns))
  #   if(any(found)) {
  #     i <- which(found)
  #     if(length(i) != 1)
  #       stop("Something is wrong with names returned from compilation.")
  #     keep[i] <- FALSE
  #     newDLLenv[[DLLname]] <- compiledFuns[[i]]
  #   }
  # }
  # if(!all(keep)) compiledFuns <- compiledFuns[keep]
  if(!returnList)
      if(length(compiledFuns) == 1) compiledFuns[[1]]
      else compiledFuns
  else compiledFuns
}

move_funs_from_list_to_env <- function(funNames, funList, env) {
  keep <- rep(TRUE, length(funList))
  for(funName in funNames) {
    found <- grepl(funName, names(funList))
    if(any(found)) {
      i <- which(found)
      if(length(i) != 1)
        stop("Something is wrong with names returned from compilation.")
      keep[i] <- FALSE
      env[[funName]] <- funList[[i]]
    }
  }
  if(!all(keep)) funList <- funList[keep]
  funList
}

wrapNCgenerator_for_DLLenv <- function(newObjFun, newDLLenv) {
  force(newDLLenv)
  force(newObjFun)
  if(!is.function(newObjFun))
    stop(paste0("newObjFun is not a function. It is a ", 
                paste0(class(newObjFun), collase = " ")))
  wrappedNewObjFun <- function() {
    ans <- newObjFun()
    parent.env(ans) <- newDLLenv
    ans
  }
  wrappedNewObjFun
}

new.serialObjectEnv <- function(serial_data = NULL, parent_env) {
  ans <- new.env()
  if(!missing(parent_env)) parent.env(ans) <- parent_env
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
  
