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
# To-Do: This example is out of date and should be updated
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
build_compiled_nClasses <- function(units,
                                    unitTypes,
                                    interfaces,
                                    exportNames,
                                    returnNames,
                                    newCobjFuns = NULL,
                                    package = FALSE) {
  numUnits <- length(units)
  ans <- vector("list", numUnits)
  inherit_indices <- vector("list", numUnits)
  for(i in seq_along(units)) {
    if(unitTypes[i]=="nCgen") {
      if(isTRUE(interfaces[[i]]%in%c("full", "generic"))) {
# Find inheritance
        NCgenerator <- units[[i]]
        inherit_NCgen <- NCgenerator$get_inherit()
        match <- rep(FALSE, numUnits)
        inherit_returnName <- NULL
        inherit_indices[[i]] <- integer()
        if(identical(inherit_NCgen, nCompiler::nClassClass)) 
          inherit_NCgen <- NULL
        else {
          for(j in seq_along(units)) {
            if(identical(units[[j]], inherit_NCgen)) match[j] <- TRUE
          }
          if(sum(match)>1)
            stop("When building compiled interface for ", exportNames[i], ", there were multiple matches for inherited nClass generator.")
          inherit_indices[[i]] <- which(match)
          inherit_returnName <- returnNames[inherit_indices[[i]] ]
        }

        ans[[i]] <- try(build_compiled_nClass(NCgenerator = units[[i]],
                                        newCobjFun = if(is.null(newCobjFuns)) NULL else newCobjFuns[[i]],
                                        inherit_NCgen = inherit_NCgen,
                                        inherit_returnName = inherit_returnName,
                                        package = package))
        if(inherits(ans[[i]], "try-error")) {
          warning(paste0("There was a problem building a full nClass interface for ", exportNames[i], "."))
          ans[[i]] <- NULL
        }
      }
    }
  }
  
  if(package) return(ans)

  for(i in seq_along(units)) {
    if(length(inherit_indices[[i]])==0) next
    Cpub_class <- ans[[i]]$parent_env$.Cpub_class
    Cpub_class$parent_env$.Cpub_base_class <- ans[[inherit_indices[[i]] ]]$parent_env$.Cpub_class
  }
  ans
}

#' @export
build_compiled_nClass <- function(NCgenerator,
                                  newCobjFun,
                                  inherit_NCgen = NULL,
                                  inherit_returnName = NULL,
                                  package = FALSE) {
  

  compiled_Cpub_class_code <- make_compiled_Cpub_class_code(
    NCgenerator = NCgenerator,
    inheritName = inherit_returnName,
    package = package,
    newCobjFun = newCobjFun
  )

  CnCgenerator_code <- make_compiled_nClass_code(NCgenerator)

  if(package) return(list(CncGen_code = CnCgenerator_code,
                          Cpub_comp_code = compiled_Cpub_class_code))

  CnCgenerator <- eval(CnCgenerator_code)
  Cpub_comp_generator <- eval(compiled_Cpub_class_code)
  env <- NCgenerator$parent_env
  connect_nClass_envs(CnCgenerator, Cpub_comp_generator, env, .NCgenerator = NCgenerator)

  # Note there is a circular relationship with
  # new_env$.Cpub_class$parent_env == new_env
  # I don't think it's a problem

  if(!missing(newCobjFun) && !is.null(newCobjFun)) {
    # Similar to .internals in nClass,
    # we put newCobjFun in two places:
    # 1. In the generator
    # 2. In the environment that every object will have as its parent.env
    if (is.list(newCobjFun)) {
      Cpub_comp_generator$.newCobjFun <- newCobjFun[[1]]
      CnCgenerator$parent_env$.newCobjFun <- newCobjFun[[1]]
    } else {
      Cpub_comp_generator$.newCobjFun <- newCobjFun
      CnCgenerator$parent_env$.newCobjFun <- newCobjFun
    }
  } else {
    Cpub_comp_generator$.newCobjFun <- NULL
    CnCgenerator$parent_env$.newCobjFun <- NULL
  }
  CnCgenerator
} 

# buildActiveBinding_for_compiled_nClass <- function(NCI, fieldNames) {
  #fieldNames <- NCI$fieldNames
#   symTab <- NCI$symbolTable
#   activeBindings <- list()
#   newFields <- list()
#   for(name in fieldNames) {
#     ans <- function(value) {}
#     sym <- symTab$getSymbol(name)
#     if(is.null(sym)) {
#       warning(paste0("Could not find a way to build active binding for field ", name, "."))
#       return(ans)
#     }
#     body(ans) <- substitute(
#     {
#       if(missing(value))
#         private$DLLenv$get_value(`:::`("nCompiler", "getExtptr")(private$CppObj), NAME)
#       else
#         private$DLLenv$set_value(`:::`("nCompiler", "getExtptr")(private$CppObj), NAME, value)
#     },
#     list(NAME = name)
#     )
#     activeBindings[[name]] <- ans
#   }
#   list(activeBindings = activeBindings,
#        newFields = newFields)
# }

make_compiled_nClass_code <- function(NCgenerator) {
  classname <- NCinternals(NCgenerator)$compileInfo$classname
  substitute(
    R6::R6Class(
      classname = CLASSNAME,
      public =list(isCompiled = \() TRUE),
      portable = FALSE,
      inherit = INHERIT,
      parent_env = new.env()
    ),
    env = list(
      CLASSNAME = classname,
      INHERIT = quote(.NCgenerator)
    )
  )
}

make_compiled_Cpub_class_code <- function(NCgenerator,
                                          inheritName = NULL,
                                          package = FALSE,
                                          newCobjFun = NULL
                                          ) {
  classname <- NCgenerator$classname
  Cpub_classname <- paste0(classname, "_Cpub_compiled")

  NCI <- NCinternals(NCgenerator)
  # Make C interface methods

  CfieldNames <- NCI$fieldNames
  symTab <- NCI$symbolTable
  for(name in CfieldNames) {
    sym <- symTab$getSymbol(name)
    if(is.null(sym)) {
      warning(paste0("Could not find a way to build active binding for field ", name, "."))
      CfieldNames <- setdiff(CfieldNames, name)
    }
  }
  CmethodNames <- NCI$methodNames
  Cmethods <- NC_get_Cpub_class(NCgenerator)$public_methods[CmethodNames]
  omit_automatic_Cpp_construction <- isTRUE(NCI$compileInfo$omit_automatic_Cpp_construction)

  Rmethods_code_list <- list()

  Rmethods_code_list[["initializeCpp"]] <- substitute(
    function(CppObj) {
      if(missing(CppObj)) {
        newCobjFun <- NEWCOBJFUN
        if(is.null(newCobjFun))
          stop("Cannot create a nClass full interface object without a newCobjFun or a CppObj argument.")
        CppObj <- newCobjFun()
      }
      private$CppObj <- CppObj
      private$DLLenv <- `:::`("nCompiler", "get_DLLenv")(CppObj) # workaround static code scanning for nCompiler:::get_DLLenv(CppObj)
    },
    list(
      NEWCOBJFUN = if(package) as.name(newCobjFun)
                   else quote(parent.env(parent.env(self))$.newCobjFun)
    )
  )

  Rmethods_code_list[["isCompiled"]] <- quote(\() TRUE)

  initialize_fun <- NC_get_Cpub_class(NCgenerator)$public_methods[["initialize"]]
  if(is.null(initialize_fun)) {
    if(!omit_automatic_Cpp_construction) {
      # The ... argument to initialize is important because it will be called
      # with ... from the outer R6generator, which might have other arguments embedded in the ...
      Rmethods_code_list[["initialize"]] <- quote(
        function(CppObj, ...) {self$initializeCpp(CppObj)}
      )
    }
  } else {
    parsedcopy <- \(f) {ans <- substitute(\() BODY, list(BODY=body(f))) |> removeSource(); if(!is.null(formals(f))) ans[[2]] <- formals(f); ans}
    new_init_code <- substitute(
      {if(is.null(private$CppObj)) self$initializeCpp() # when this is inherited, the derived class should have populated private$CppObj
          OLDBODY},
         list(OLDBODY = body(initialize_fun))
    ) |> removeSource()
    if(!is.null(formals(initialize_fun))) new_init_code[[2]] <- formals(initialize_fun)
    Rmethods_code_list[["initialize"]] <- new_init_code
  }

  Rmethods_code <- do.call("call", c("list",
                            Rmethods_code_list))

  activeBindings_code_list <- list()
  for(name in CfieldNames) {
    ABcode <- substitute(
    \(value) {
      if(missing(value))
        private$DLLenv$get_value(`:::`("nCompiler", "getExtptr")(private$CppObj), NAME)
      else
        private$DLLenv$set_value(`:::`("nCompiler", "getExtptr")(private$CppObj), NAME, value)
    },
    list(NAME = name)
    ) |> removeSource()
    activeBindings_code_list[[name]] <- ABcode
  }
  activeBindings_code <- do.call("call", c("list",
                            activeBindings_code_list))


  CinterfaceMethods_code_list <-  mapply(make_method_code_for_compiled_nClass,
                                        Cmethods,
                                        CmethodNames)
  CinterfaceMethods_code <- do.call("call", c("list",
                            CinterfaceMethods_code_list))
  
  Cpub_inherit_arg <- if(package) {
    if(is.null(inheritName)) quote(nCompiler::CpubClass)
    else substitute(IRN$parent_env$.Cpub_class, list(IRN=as.name(paste0(".", inheritName, "_CnCgenerator_CpubGen"))))
  } else {
    if(is.null(inheritName)) quote(nCompiler::CpubClass)
    else quote(.Cpub_base_class)
  }

  Cpub_comp_code <- substitute(
    R6::R6Class(
      classname = CLASSNAME,
      private = list(
        CppObj = NULL,
        DLLenv = NULL
      ),
      public = c(
        RMETHODS,
        CINTERFACE),
      active = ACTIVEBINDINGS,
      portable = FALSE,
      inherit = INHERIT, # Default. May be updated at next pass,
      parent_env = NULL ## when quoted = TRUE, env argument is not used
    ),
    env = list(
      CLASSNAME = Cpub_classname,
      RMETHODS = Rmethods_code,
      CINTERFACE = CinterfaceMethods_code,
      ACTIVEBINDINGS = activeBindings_code,
      INHERIT = Cpub_inherit_arg
    )
  )
}

make_method_code_for_compiled_nClass <- function(fun, name) {
  if(is.null(fun)) return(NULL) ## convenient for how this is used from mapply
  if(!NFinternals(fun)$compileInfo$callFromR) {
    ans <- substitute(
      function(...) {
        stop("method ", NAME, " cannot be called directly from R.")
      },
      list(NAME = name)
    )
    return(ans)
  }
  argNames <- names(formals(fun))

  parsedcopy <- \(f) {ans <- substitute(\() BODY, list(BODY=body(f))) |> removeSource(); if(!is.null(formals(f))) ans[[2]] <- formals(f); ans}
  listcode <- quote(list())
  for(i in seq_along(argNames))
    listcode[[i+1]] <- as.name(argNames[i])
  body_ans <- substitute(
    private$DLLenv$call_method(`:::`("nCompiler", "getExtptr")(private$CppObj), NAME, LISTCODE),
    list(NAME = name,
         LISTCODE = listcode)
  )
  refArgs <- NFinternals(fun)$refArgs
  blockRefArgs <- NFinternals(fun)$blockRefArgs
  body_ans <- passByReferenceIntoC(body_ans, refArgs, blockRefArgs)
  ans <- substitute(
    \() BODY,
    list(BODY = body_ans)
  ) |> removeSource()
  if(!is.null(formals(fun))) ans[[2]] <- formals(fun)
  ans
}

buildMethod_for_compiled_nClass <- function(fun, name) {
  if(is.null(fun)) return(NULL) ## convenient for how this is used from mapply
  if(!NFinternals(fun)$compileInfo$callFromR) {
    ans <- function(...) {}
    environment(ans) <- new.env()
    body(ans) <- substitute(
      stop("method ", NAME, " cannot be called directly from R."),
      list(NAME = name)
    )
    return(ans)
  }

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
  ## them in lazy-evaluation (promise) form and implement ref and
  ## blockRef behavior. This is similar to what rlang's quosures do.
  ##
  ## Then for a time we experimented with passing the environment()
  ## instead of the arguments and using the environment to look up
  ## the arguments. This allowing managing ref and blockRef from the C++
  ## but also required that we imitate R's match.def-type behavior.
  ## The other big motivations for it was the generic interface call_method version
  ## that only has ... as input, so there isn't anything to match on.
  ## That worked until we realized we could get byte code versions of promises.
  ## Then we decided it was not very wise to imitate R's behavior.
  ## So now we go back to passing a list and modifying as needed for ref and blockRef.
  ## And now for the generic ... case, we
  ## place a method-specific function in the CnClass_env.##
  ##
  listcode <- quote(list())
  for(i in seq_along(argNames))
    listcode[[i+1]] <- as.name(argNames[i])
  body_ans <- substitute(
    private$DLLenv$call_method(`:::`("nCompiler", "getExtptr")(private$CppObj), NAME, LISTCODE),
    list(NAME = name,
         LISTCODE = listcode)
  )
  refArgs <- NFinternals(fun)$refArgs
  blockRefArgs <- NFinternals(fun)$blockRefArgs
  body(ans) <- passByReferenceIntoC(body_ans, refArgs, blockRefArgs)
  ans
}

# This builds functions that are placed in the DLLenv to call C functions exported from that DLL
build_generic_fns_for_compiled_nClass <- function(NCgenerator) {
  NCI <- NCinternals(NCgenerator)
  # Make C interface methods
  CmethodNames <- NCI$methodNames
  recurse_make_Cmethods <- function(NCgenerator, CmethodNames,
                                    derivedNames = character()) {
    interfaceFns <-  mapply(build_generic_fn_for_compiled_nClass_method,
                                NC_get_Cpub_class(NCgenerator)$public_methods[CmethodNames],
                                CmethodNames)
    inherit_obj <- NCgenerator$get_inherit()
    if(isNCgenerator(inherit_obj)) {
      derivedNames <- c(derivedNames, CmethodNames)
      baseNCgen <- inherit_obj
      baseCmethodNames <- NCinternals(baseNCgen)$methodNames
      baseCmethodNames <- setdiff(baseCmethodNames, derivedNames)
      baseInterfaceFns <- recurse_make_Cmethods(baseNCgen,
                                                baseCmethodNames,
                                                derivedNames)
      interfaceFns <- c(interfaceFns, baseInterfaceFns)
    }
    interfaceFns
  }
  CinterfaceFns <- recurse_make_Cmethods(NCgenerator, CmethodNames)
  CinterfaceFns
}

build_generic_fn_for_compiled_nClass_method <- function(fun, name) {
  if(is.null(fun)) return(NULL) ## convenient for how this is used from mapply
  if(!NFinternals(fun)$compileInfo$callFromR) {
    ans <- function(...) {}
    environment(ans) <- new.env()
    body(ans) <- substitute(
      stop("method ", NAME, " cannot be called directly from R."),
      list(NAME = name)
    )
    return(ans)
  }

  argNames <- names(formals(fun))
  ans <- fun
  environment(ans) <- new.env() # will be reset later to the CnClass_env
  listcode <- quote(list())
  for(i in seq_along(argNames))
    listcode[[i+1]] <- as.name(argNames[i])
  body_ans <- substitute(
    call_method(`:::`("nCompiler", "getExtptr")(CppObj_), NAME, LISTCODE),
    list(NAME = name,
         LISTCODE = listcode)
  )
  # The generic method() will give a copy of the function a new closure
  # and put CppObj_ in it.
  # We used to instead have method return a function that call the generic
  # function, but this did not work for ref and blockRef argument passing.
  formals(ans) <- formals(fun) #c(formals(function(CppObj_){}), formals(fun))
  refArgs <- NFinternals(fun)$refArgs
  blockRefArgs <- NFinternals(fun)$blockRefArgs
  body(ans) <- passByReferenceIntoC(body_ans, refArgs, blockRefArgs)
  ans

}
