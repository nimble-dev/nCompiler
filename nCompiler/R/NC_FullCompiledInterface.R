## Function to create a "full interface" to a compiled nClass.
## By "full interface", we mean an R6 class with active bindings
## for Cpublic member data and functions for Cpublic methods that will
## call the corresponding compiled methods.

# build_compiled_nClass generates an R6 class to interface to a compiled nClass.
# This commented code shows a protoype of the kind of class definition created.
#
# If nc1 is created as follows:
# nc1 <- nClass(
#   Rpublic = list(
#     Rv = NULL,
#     Rfoo = function(x) x+1
#   ),
#   Cpublic = list(
#     Cv = 'numericScalar',
#     Cfoo = nFunction(
#       fun = function(x) {
#         return(x+1)
#       },
#       argTypes = list(x = 'numericScalar'),
#       returnType = 'numericScalar')
#   )
# )
#
# then build_nClassInterface would be like the following:
#
# FI <- R6::R6Class(
#   private = list(
#     CppObj = NULL
#   ),
#   public = list(
#     initialize = function(CppObj) {
#       if(missing(CppObj))
#         stop("Cannot create a nClass full interface object without a CppObj.")
#       private$CppObj <- CppObj
#     },
#     Rv = NULL,                 #R fields  (Cgenerator$public_fields[RfieldNames])
#     Rfoo = function(x) x+1,    #R methods (NCgenerator$public_methods[RmethodNames])
#     Cfoo = function(x) {       #C methods (CinterfaceMethods)
#       nCompiler:::call_method(getExtptr(private$CppObj), 'Cfoo', list(x))
#     }
#   ),
#   active = list(
#     Cv = function(value) {     #C fields (activeBindings)
#       if(missing(value))
#         nCompiler:::get_value(getExtptr(private$CppObj), 'Cv')
#       else
#         nCompiler:::set_value(getExtptr(private$CppObj), 'Cv', value)
#     }
#   )
# )
# 

#' @export
build_compiled_nClass <- function(NCgenerator,
                                  newCobjFun,
                                  env = parent.frame(),
                                  quoted = FALSE) {
  NCI <- NCinternals(NCgenerator)
  CmethodNames <- NCI$methodNames
  RmethodNames <- setdiff(names(NCgenerator$public_methods), 
                          c(CmethodNames, 'clone'))
  CinterfaceMethods <- mapply(buildMethod_for_compiled_nClass,
                              NCgenerator$public_methods[CmethodNames],
                              CmethodNames)
  enableDerivs <- unlist(NCinternals(NCgenerator)$enableDerivs)
  if (length(enableDerivs > 0)) {
    ## add *_derivs_ method for methods in enableDerivs
    derivsMethods <- mapply(buildMethod_derivs_for_compiled_nClass,
                            NCgenerator$public_methods[enableDerivs],
                            enableDerivs)
    names(derivsMethods) <- paste0(enableDerivs, '_derivs_')
    CinterfaceMethods <- c(CinterfaceMethods, derivsMethods)
  }
  CfieldNames <- NCI$fieldNames
  RfieldNames <- setdiff(names(NCgenerator$public_fields),
                         CfieldNames)
  activeBindingResults <- buildActiveBinding_for_compiled_nClass(NCI)
  activeBindings <- activeBindingResults$activeBindings
  internal_fields <- activeBindingResults$newFields
  ## activeBindings <- lapply(CfieldNames,
  ##                          buildActiveBinding_for_compiled_nClass,
  ##                          NCI$symbolTable)
  names(activeBindings) = CfieldNames
  classname <- paste0(NCgenerator$classname, '_compiled')

  ans <- substitute(
    expr = R6::R6Class(
      classname = CLASSNAME,
      private = list(
        CppObj = NULL,
        DLLenv = NULL
      ),
      public = c(
        list(initialize = function(CppObj) {
          if(missing(CppObj)) {
            newCobjFun <- NEWCOBJFUN
            if(is.null(newCobjFun))
              stop("Cannot create a nClass full interface object without a newCobjFun or a CppObj argument.")
            CppObj <- newCobjFun()
          }
          private$CppObj <- CppObj
          private$DLLenv <- nCompiler:::get_DLLenv(CppObj)
        }),
        RPUBLIC,
        RFIELDS,
        CINTERFACE),
      active = ACTIVEBINDINGS,
      portable = FALSE,
      inherit = nCompiler:::CnClassClass,
      parent_env = NULL ## when quoted = TRUE, env argument is not used
    ),
    env = list(
      CLASSNAME = classname,
      NEWCOBJFUN = parse(text = paste0('new_', NCgenerator$classname),
                         keep.source = FALSE)[[1]],
      RPUBLIC = parse(text = deparse(
        NCgenerator$public_methods[RmethodNames]
      ), keep.source = FALSE)[[1]],
      RFIELDS = parse(text = deparse(
        c(NCgenerator$public_fields[RfieldNames], internal_fields)
      ), keep.source = FALSE)[[1]],
      CINTERFACE = parse(text = deparse(
        CinterfaceMethods
      ), keep.source = FALSE)[[1]],
      ACTIVEBINDINGS = parse(text = deparse(activeBindings))[[1]]
    )
  )

  if (quoted) return(ans)

  ans <- eval(ans)
  ans$public_methods$initialize <- function(CppObj) {
    if(missing(CppObj)) {
      newCobjFun <- parent.env(parent.env(self))$.newCobjFun
      if(is.null(newCobjFun))
        stop("Cannot create a nClass full interface object without a newCobjFun or a CppObj argument.")
      CppObj <- newCobjFun()
    }
    private$CppObj <- CppObj
    private$DLLenv <- nCompiler:::get_DLLenv(CppObj)
  }

  new_env <- new.env(parent = env)
  ans$parent_env <- new_env

  if(!missing(newCobjFun)) {
    # Similar to .internals in nClass,
    # we put newCobjFun in two places:
    # 1. In the generator
    # 2. In the environment that every object will have as its parent.env
    if (is.list(newCobjFun)) {
      ans$.newCobjFun <- newCobjFun[[1]]
      new_env$.newCobjFun <- newCobjFun[[1]]
    } else {
      ans$.newCobjFun <- newCobjFun
      new_env$.newCobjFun <- newCobjFun
    }
  } else {
    ans$.newCobjFun <- NULL
    new_env$.newCobjFun <- NULL
  }
  ans
}

buildActiveBinding_for_compiled_nClass <- function(NCI) {
  fieldNames <- NCI$fieldNames
  symTab <- NCI$symbolTable
  activeBindings <- list()
  newFields <- list()
  for(name in fieldNames) {
    ans <- function(value) {}
    sym <- symTab$getSymbol(name)
    if(is.null(sym)) {
      warning(paste0("Could not find a way to build active binding for field ", name, "."))
      return(ans)
    }
    
    if(inherits(sym, "symbolTBD")) { # This is a putative nClass type
      internal_name <- paste0(name, "_internal")
      body(ans) <- substitute(
      {
        if(missing(value))
          self$INTERNAL_NAME
        else {
          if(!isCNC(value)) {
            if(nCompiler:::is.loadedObjectEnv(value)) {
              value <- nCompiler:::to_full_interface(value)
            } else if(isNC(value))
              stop("Assigning an uncompiled nClass object to a Cpublic field, where a compiled object is needed.")
            else
              stop("Assigning an invalid object to a Cpublic nClass field")          
          }
          self$INTERNAL_NAME <- value
          private$DLLenv$set_value(nCompiler:::getExtptr(private$CppObj), NAME, value$private$CppObj)
        }
      },
      list(NAME = name, INTERNAL_NAME = as.name(internal_name))
      )
      activeBindings[[name]] <- ans
      newFields[internal_name] <- list(NULL)
      next
    }
    
    body(ans) <- substitute(
    {
      if(missing(value))
        private$DLLenv$get_value(nCompiler:::getExtptr(private$CppObj), NAME)
      else
        private$DLLenv$set_value(nCompiler:::getExtptr(private$CppObj), NAME, value)
    },
    list(NAME = name)
    )
    activeBindings[[name]] <- ans
  }
  list(activeBindings = activeBindings,
       newFields = newFields)
}

buildMethod_for_compiled_nClass <- function(fun, name) {
  if(is.null(fun)) return(NULL) ## convenient for how this is used from mapply
  argNames <- names(formals(fun))
  ans <- fun
  environment(ans) <- new.env()
  ## The internet says that R6 methods are assigned their environments
  ## during a call to methodGenerator$new().  We put a new.env()
  ## here anyway as insurance against the possibility of quirky 
  ## environment problems.
  ##
  ## We used to make the third argument like list(arg1, arg2, arg3)
  ## Now we just provide the environment() and from C++ look up the
  ## inputs for arg1, arg2, and arg3.  This allows us to capture
  ## them in lazy-evaluation (promise) form and impement ref and
  ## blockRef behavior. This is similar to what rlang's quosures do.
  ##  listcode <- quote(list())
  ##  for(i in seq_along(argNames))
  ##    listcode[[i+1]] <- as.name(argNames[i])
  body(ans) <- substitute(
    private$DLLenv$call_method(nCompiler:::getExtptr(private$CppObj), NAME, environment()),
    list(NAME = name)
  )
  ans
}

buildMethod_derivs_for_compiled_nClass <- function(fun, name) {
  if(is.null(fun)) return(NULL) ## convenient for how this is used from mapply
  ans <- fun
  ## add the 'order' and 'wrt' args to the function's formals
  formals(ans) <- c(formals(ans), list(order = c(0, 1, 2), wrt = NULL))
  argNames <- names(formals(ans))
  environment(ans) <- new.env()
  listcode <- quote(list())
  for(i in seq_along(argNames))
    listcode[[i+1]] <- as.name(argNames[i])
  body(ans) <- substitute({
    obj_env <- private$DLLenv$call_method(
      nCompiler:::getExtptr(private$CppObj), NAME, LISTCODE
    )
    C_nC_derivClass$new(obj_env)
  }, list(NAME = paste0(name, '_derivs_'), LISTCODE = listcode))
  ans
}
