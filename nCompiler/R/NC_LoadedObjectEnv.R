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

new.loadedObjectEnv <- function(extptr = NULL, serialized = NULL) {
  if(!xor(is.null(extptr), is.null(serialized)))
    stop("One and only one of extptr or serialized should be non-null.")
  ans <- new.env()
  ans$extptr <- extptr
  ans$serialized <- serialized
  class(ans) <- "loadedObjectEnv"
  ans
}

is.loadedObjectEnv <- function(env) {
  ## The checks here may be over-kill.
  ## We may be able to rely solely on the class label.
  if(!is.environment(env)) return(FALSE)
  if(!exists("extptr", where = env)) return(FALSE)
  if(!exists("serialized", where = env)) return(FALSE)
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
  
