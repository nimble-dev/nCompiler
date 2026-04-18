normalizeCallsEnv <- new.env()
normalizeCallsEnv$.debug <- FALSE

inNormalizeCallsEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = normalizeCallsEnv)
}

# This stage does two things:
# 1. It normalizes the arguments of calls according to a matchDef, if found via an opDef.
#.   This includes separating the compileTime arguments into the aux list of the call.
# 2. It caches information about the nFunction or nClass method being called,
#.   if relevant. Future stages should check if the call name still matches what was cached, 
#.   they should look up from scratch. This is because handlers can change a name and then
#.   then opDef should be found in future steps for the new name.
#.   The caching also allows future stages to be more efficient in determining an nFunction or nClass method case.
compile_normalizeCalls <- function(code,
                                   symTab,
                                   auxEnv) {
  nErrorEnv$stateInfo <- paste0("handling normalizeCalls for ",
                                code$name,
                                ".")

  logging <- get_nOption('compilerOptions')[['logging']]
  if (logging) appendToLog(paste('###', nErrorEnv$stateInfo, '###'))

  if(code$isLiteral) return(NULL)
  if(code$isName) return(NULL)
  if(code$isCall) {
    if(code$name == '{') {
      ## recurse over lines
      for(i in seq_along(code$args)) {
        compile_normalizeCalls(code$args[[i]], symTab, auxEnv)
      }
      return(NULL)
    }
    # If we are in an nClass:
    #  look for opDef in the nFunction method;
    #  next look for an opDef in the nClass, which might not even correspond to a method.
    # If we are in an nFunction (not a method):
    #  look for an opDef in the nFunction.
    # If we are in an nFunction or nClass method and no opDef has been found,
    #.   assign nFunction_default to be the opDef.
    # Next look for a global opDef, which will look first for user-defined opDefs.
    # If no opDef is defined anywhere, it is an error.
    # If an opDef is found, check for a handler.
    # If a handler is found, call it.
    # If no handler is found, look for a matchDef and normalize the arguments by default.
    #
    # What gets cached in the aux of the exprClass for the call:
    #   cachedOpInfo = list(opDef, name, obj_internals, case)
    #   We defer: uniqueName, cpp_code_name
    cachedOpInfo <- update_cachedOpInfo(code, auxEnv$where)
    if(cachedOpInfo$case == "nFunction") {
      uniqueName <- cachedOpInfo$obj_internals$uniqueName2
      if(length(uniqueName)==0)
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0('nFunction ', code$name, 'is being called, ',
                      'but it is malformed because it has no internal name.')),
            call. = FALSE)
      if(is.null(auxEnv$needed_nFunctions[[uniqueName]])) {
        ## We could put the nFunction itself in the needed_nFunctions list,
        ## but we do not as a way to avoid having many references to R6 objects
        ## in a blind attempt to facilitate garbage collection based on past experience.
        ## Instead, we provide what is needed to look up the nFunction again later.
        auxEnv$needed_nFunctions[[uniqueName]] <- list(code$name, auxEnv$where)
      }
    }
    # In the case of an nClassBuilder call, we evaluate that in R right here, right now,
    # and replace it with a type name.
    # This is a candidate for a new compilation stage, but it might work fine to bundle it here.
    if(cachedOpInfo$case == "nClassBuilder") {
      this_builder <- cachedOpInfo$obj_internals
      type_res <- check_built_types(this_builder, code$Rexpr, auxEnv$project_env)
      cpp_classname <- names(type_res)[1]
      new_code <- exprClass$new(name = cpp_classname, isName = TRUE, isCall = FALSE, isLiteral = FALSE, isAssign = FALSE)
      replaceArgInCaller(code, new_code)
      code$aux$cachedOpInfo <- NULL
      return(NULL) # Now we have made it a name, not a call.
    }

    opDef <- cachedOpInfo$opDef
    matchDef <- opDef[["matchDef"]]
    if(is.null(matchDef))
      matchDef <- cachedOpInfo$obj_internals$default_matchDef
    if(!is.null(matchDef)) {
      exprClass_put_args_in_order(matchDef, code, opDef$compileArgs)
      # code <- replaceArgInCaller(code, matched_code)
    }
    normalizeCallsEnv$recurse_normalizeCalls(code, symTab, auxEnv, handlingInfo)
  }
  nErrorEnv$stateInfo <- character()
  invisible(NULL)
}

inNormalizeCallsEnv(
  recurse_normalizeCalls <-
    function(code, symTab, auxEnv, handlingInfo,
             useArgs = rep(TRUE, length(code$args))) {
      ## won't be here unless code is a call.  It will not be a {
      for(i in seq_along(code$args)) {
        if(useArgs[i]) {
          if(inherits(code$args[[i]], 'exprClass')) {
            compile_normalizeCalls(code$args[[i]],
                                   symTab,
                                   auxEnv)
          }
        }
      }
      NULL
    }
)

check_cachedOpInfo <- function(code, where=NULL, update=TRUE, allowFail=FALSE) {
  cachedOpInfo <- code$aux$cachedOpInfo
  up_to_date <- !is.null(cachedOpInfo) && cachedOpInfo$name == code$name
  if(up_to_date) return(cachedOpInfo)
  if(update) {
    if(!is.environment(where))
      stop(
        exprClassProcessingErrorMsg(
          code,
          "Internal error: check_cachedOpInfo called with update=TRUE but non-environment 'where' argument.")
        , call. = FALSE)
    update_cachedOpInfo(code, where, allowFail=allowFail)
  } else {
    NULL
  }
}

update_cachedOpInfo <- function(code, where, allowFail=FALSE) {
  opDef <- NULL
  cachedOpInfo <- list(name = code$name)
    ## Note that if we are in a method, auxEnv$where will be the
    ## generator, which is an environment.
  is_NCgenerator <- isNCgenerator(where)
  if(is.null(opDef)) { # for good form, in case there a prior case is added later
    # First look for an nClass method.
    obj <- NULL
    if(is.null(obj)) {
      if(is_NCgenerator) {## We are in a class method (by direct call within another class method, no `$` involved)
        # NC_find_method checks the class and its parents
        obj <- NC_find_method(where, code$name, inherits=TRUE)
        if(!is.null(obj)) {
          if(isNF(obj)) {
            cachedOpInfo$case <- "nClass method" # possibly disambiguate method from keyword
            cachedOpInfo$obj_internals <- NFinternals(obj)
            opDef <- cachedOpInfo$obj_internals$compileInfo$opDef # might be NULL
          } else {
            stop(exprClassProcessingErrorMsg(code,
                                              paste0('method ', code$name, 'is being called, but it is not a nFunction.')),
                  call. = FALSE)
          }
        }
      }
    }
    if(is.null(obj)) {
      # Next look for an nFunction (that is not a method)
      # 
      # N.B. nGet follows lexical scoping from either an nFunction or nClass
      # whatever "where" is. (In the nClass case,
      # this is different than finding a method as above.
      # Instead it is following scoping to find nFunctions.)
      obj <- nGet(code$name, where = where)
      if(!is.null(obj)) {
        if(isNF(obj)) {
          # There is no error trapping if obj is not an nFunction, because
          # it could be simply an R function, since nGet (via get0) may traverse up to R_GlobalEnv.
          cachedOpInfo$case <- "nFunction"
          cachedOpInfo$obj_internals <- NFinternals(obj)
          opDef <- cachedOpInfo$obj_internals$compileInfo$opDef # might be NULL
        } else if(inherits(obj, "nClassBuilder")) {
          cachedOpInfo$case <- "nClassBuilder"
          opDef <- getOperatorDef("nClassBuilder") # a dummy to be non-null below
          cachedOpInfo$obj_internals <- obj
        } else {
          obj <- NULL # reset to NULL if not an nFunction or nClassBuilder
        }
      }
    }
    # if(!is.null(obj)) {
    #   if(cachedOpInfo$case == "nFunction") {
    #   # We found an nFunction object that is either a method or not.
    #     cachedOpInfo$obj_internals <- NFinternals(obj)
    #     opDef <- cachedOpInfo$obj_internals$compileInfo$opDef # might be NULL
    #   }
    # }
  }
  if(is.null(opDef)) {
    ## At this point, we have not found an nFunction or nClass method.
    if(is_NCgenerator) {
      opDef <- NCinternals(where)$compileInfo$opDefs[[code$name]]
      if(!is.null(opDef)) {
        cachedOpInfo$case <- "nClass method" # this could be a pure keyword or an nFunction with opDef provided at the nClass level
        # a pure keyword will have obj_internals == NULL, providing a way to 
        # tell these cases apart later if necessary.
      }
    }
  }
  if(is.null(opDef)) {
    if(!is.null(cachedOpInfo$case)) {
      # We found an nFunction (method or not) but it did not have an opDef
      # so here we insert the default opDef
      if(cachedOpInfo$case == "nFunction" || cachedOpInfo$case == "nClass method") {
        opDef <- getOperatorDef("nFunction_default")
      }
    }
  }
  if(is.null(opDef)) {
    # We haven't found any form of nFunction or custom nClass keyword,
    # so look for globally defined operators (e.g. "+" and all the basic DSL keywords)
    opDef <- getOperatorDef(code$name)
    if(!is.null(opDef)) {
      cachedOpInfo$case <- "global"
    }
  }
  if(is.null(opDef)) {
    if(!allowFail)
      stop(exprClassProcessingErrorMsg(
        code,
        paste0('No operator definition found for ', code$name, '.')),
        call. = FALSE)
  }
  cachedOpInfo$opDef <- opDef
  code$aux$cachedOpInfo <- cachedOpInfo
  cachedOpInfo
}
