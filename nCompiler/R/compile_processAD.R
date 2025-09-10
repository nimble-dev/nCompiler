processADEnv <- new.env()
processADEnv$.debug <- FALSE

inProcessADEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = processADEnv)
}

# This handles modifications of code that has AD types, AND
# also nDerivs calls.
# Note that AD types are first-class scalar types along with
# numeric, integer, and logical. Therefore any function can use
# an AD type anywhere. Usually it only makes sense in functions set up
# to be recorded, but that need not always be the case.
compile_processAD <- function(code,
                              symTab,
                              auxEnv,
                              # auxEnv is for sharing across lines in the same code body
                              # workEnv is for sharing within a line.  This
                              # could be done via auxEnv, but having workEnv is cleaner
                              # and more safely avoids having residual information left around.
                              ## It is unclear if workEnv will be needed.
                              workEnv = new.env()) {
  nErrorEnv$stateInfo <- paste0("handling processAD for ",
                                code$name,
                                ".")
  setupExprs <- list()
  if(code$isLiteral) {
    return(list())
  }
  if(code$isName) {
    return(list())
  }
  if(code$isCall) {
    if(code$name == '{') {
      for(i in seq_along(code$args)) {
        compile_processAD(code$args[[i]],
                          symTab,
                          auxEnv,
                          workEnv = new.env()) ## a new line
      }
      return(invisible(NULL))
    }
    if(code$name == 'for') {
      compile_processAD(code$args[[3]], symTab, auxEnv)
      return(invisible(NULL))
    }
    if(code$name %in% ifOrWhile) {
      compile_processAD(code$args[[2]], symTab, auxEnv)
      if(length(code$args)==3)
        compile_processAD(code$args[[3]], symTab, auxEnv)
      return(invisible(NULL))
    }

    opInfo <- check_cachedOpInfo(code, where=auxEnv$where, update=TRUE)
    handlingInfo <- getOperatorField(opInfo$opDef, "processAD")
#    handlingInfo <- getOperatorDef(code$name, "processAD")
    # opInfo <- operatorDefEnv[[code$name]]
    # if(!is.null(opInfo)) {
    #   handlingInfo <- opInfo[["processAD"]]
      if(!is.null(handlingInfo)) {
        beforeHandler <- handlingInfo[['beforeHandler']]
        if(!is.null(beforeHandler)) {
          eval(call(beforeHandler,
                    code,
                    symTab,
                    auxEnv,
                    workEnv,
                    handlingInfo),
               envir = processADEnv)
        }
      }
    # }

    iArgs <- seq_along(code$args)
    for(i in iArgs) {
      if(inherits(code$args[[i]], 'exprClass')) {
        compile_processAD(code$args[[i]], symTab, auxEnv, workEnv)
      }
    }

    ## finally, call any special handlers
    # if(!is.null(opInfo)) {
    #   handlingInfo <- opInfo[["processAD"]]
      if(!is.null(handlingInfo)) {
        handler <- handlingInfo[['handler']]
        if(!is.null(handler)) {
          eval(call(handler,
                    code,
                    symTab,
                    auxEnv,
                    workEnv,
                    handlingInfo),
               envir = processADEnv)
        }
      }
    # }
  }
  invisible(NULL)
}

inProcessADEnv(
  recurse_processAD <-
    function(code, symTab, auxEnv, handlingInfo,
             useArgs = rep(TRUE, length(code$args))) {
      ## won't be here unless code is a call.  It will not be a {
      for(i in seq_along(code$args)) {
        if(useArgs[i]) {
          if(inherits(code$args[[i]], 'exprClass')) {
            compile_processAD(code$args[[i]],
                              symTab,
                              auxEnv)
          }
        }
      }
      NULL
    }
)

inProcessADEnv(
  nDerivs <-
    function(code, symTab, auxEnv, handlingInfo,
             useArgs = rep(TRUE, length(code$args))) {
      # nDerivs(foo(x), wrt, update, reset) needs to become:
      #
      # 1. A new class member variable:
      # deriv_mgr< METHOD_TYPES(&classname::foo),
      #            ADhandling<RETURN, WRT> > foo_deriv_mgr_1;
      # where foo_deriv_mgr_1 may be a generated unique name
      # and the ADhandling types need some kind of system for determination of control.
      #
      # 2. A new initialization list entry for the nClass:
      #  foo_deriv_mgr1(&classname::foo, this),
      #
      # 3. foo_deriv_mgr1.nDerivs(wrt, update, reset, x, ...<other args, which have to be last> );
      #
      # I could create a notation like nDerivs(foo(x, U(y), C(z))) # to indicate Update and Constant arguments.
      # It would be nice to not to confuse potetial run-time vs compile-time arguments.
      # Although... I guess technically they could be run-time but only used when recording.
      # So could even be an R/Rcpp list.
      cpp_classname <- NCinternals(auxEnv$where)$cpp_classname
      call_code <- code$args[[1]]
      method_name <- call_code$args[[1]]$name
      ADmgrName <- ADtapeMgrLabelCreator(envName = "foo")
      ptrToMethod <- paste0("&", cpp_classname, "::",  method_name)
      rolesExpr <- code$args[['roles']]
      if(is.null(rolesExpr)) ADhandlingTypes <- c("RETURN", "WRT") # dummy starter
      else ADhandlingTypes <- eval(nDeparse(rolesExpr))
      ADhandlingTypes <- paste0(ADhandlingTypes, collapse=",")
      ADhandlingExpr <- paste0("ADhandling<", ADhandlingTypes, "> ")
      ADmgrVar <- cppVarFullClass$new(name = ADmgrName,
                                      baseType = "nDerivsMgrClass",
                                      templateArgs = list(paste0("METHOD_TYPES(",ptrToMethod,")"),
                                                          ADhandlingExpr))
      ADconstructorInit <- paste0(ADmgrName,"(", ptrToMethod, ", this)")
      ADconstructorInit <- nParse(substitute(cppLiteral(CODE), list(CODE = ADconstructorInit)))
      if(is.null(auxEnv$derivsContent$ADconstructorInits))
        auxEnv$derivsContent$ADconstructorInits <- list()
      auxEnv$derivsContent$ADconstructorInits[[ADmgrName]] <- ADconstructorInit

      if(is.null(auxEnv$derivsContent$ADtapeMgrSymbols))
        auxEnv$derivsContent$ADtapeMgrSymbols <- list()
      auxEnv$derivsContent$ADtapeMgrSymbols[[ADmgrName]] <- ADmgrVar

      newCode <- substitute(.method(ADMGRNAME, nDerivs),
                            list(ADMGRNAME = as.name(ADmgrName)))
      newCode <- nParse(newCode)
      # We could set up names for newCode$args if necessary.
      insertArg(newCode, 3, code$args[["update"]])
      insertArg(newCode, 4, code$args[["reset"]])
      for(i in seq_along(call_code$args)) {
        if(i == 1) next # first is the name of the method, which isn't needed.
        insertArg(newCode, 4+i-1, call_code$args[[i]])
      }
      replaceArgInCaller(code, newCode)
      NULL
    }
)
