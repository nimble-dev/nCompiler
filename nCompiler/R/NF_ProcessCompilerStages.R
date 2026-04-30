
compilerStage_substituteMangledArgumentNames <-
  function(NFcompiler,
           nameSubList,
           debug = FALSE) {
    if(debug) browser()
    NFcompiler$newRcode <-
      nf_substituteExceptFunctionsAndDollarSigns(NFcompiler$newRcode,
                                                 nameSubList)
    invisible(NULL)
  }

compilerStage_initializeCode <-
  function(NFcompiler,
           debug = FALSE) {
    if(debug) browser()
    NFcompiler$code <- nParse(NFcompiler$newRcode)
    ## Useful for debugging: writeCode(nDeparse(NFcompiler$code))
    invisible(NULL)
  }

nimDebug <- function(f) debug(f)
nimUndebug <- function(f) if(isdebugged(f)) undebug(f)
nimDebugHandlerEnv <- function(env) {
  funNames <- ls(env)
  for(f in funNames) {
    if(is.function(env[[f]]))
      debug(env[[f]])
  }
}
nimUndebugHandlerEnv <- function(env) {
  funNames <- ls(env)
  for(f in funNames) {
    if(is.function(env[[f]]))
      if(isdebugged(env[[f]]))
        undebug(env[[f]])
  }
}

compilerStage_normalizeCalls <-
  function(NFcompiler,
           debug) {
    if(debug) {
      browser()
      normalizeCallsEnv$.debug <- TRUE
      nimDebug(compile_normalizeCalls)
      nimDebugHandlerEnv(normalizeCallsEnv)
      on.exit({
        normalizeCallsEnv$.debug <- FALSE
        nimUndebug(compile_normalizeCalls)
        nimUndebugHandlerEnv(normalizeCallsEnv)
      })
    }
    compile_normalizeCalls(NFcompiler$code,
                           NFcompiler$symbolTable,
                           NFcompiler$auxEnv)
  }

compilerStage_simpleTransformations <-
  function(NFcompiler,
           debug) {
    if(debug) {
      browser()
      simpleTransformationsEnv$.debug <- TRUE
      nimDebug(compile_simpleTransformations)
      nimDebugHandlerEnv(simpleTransformationsEnv)
      on.exit({
        simpleTransformationsEnv$.debug <- FALSE
        nimUndebug(compile_simpleTransformations)
        nimUndebugHandlerEnv(simpleTransformationsEnv)
      })
    }
    compile_simpleTransformations(NFcompiler$code,
                                  NFcompiler$symbolTable,
                                  NFcompiler$auxEnv)
  }

compilerStage_finalTransformations <-
  function(NFcompiler,
           debug) {
    if(debug) {
      browser()
      simpleTransformationsEnv$.debug <- TRUE
      nimDebug(compile_simpleTransformations)
      nimDebugHandlerEnv(simpleTransformationsEnv)
      on.exit({
        simpleTransformationsEnv$.debug <- FALSE
        nimUndebug(compile_simpleTransformations)
        nimUndebugHandlerEnv(simpleTransformationsEnv)
      })
    }
    compile_simpleTransformations(NFcompiler$code,
                                  NFcompiler$symbolTable,
                                  NFcompiler$auxEnv,
                                  opInfoName = "finalTransformations",
                                  handlerEnv = finalTransformationsEnv)
  }

compilerStage_simpleIntermediates <-
  function(NFcompiler,
           debug) {
    if(debug) {
      browser()
    }
    compile_simpleIntermediates(NFcompiler$code)
  }

compilerStage_initializeAuxEnv <- function(NFcompiler,
                                           sourceObj = NULL, ## This will be a derived nClass object if the nFunction is an nClass method 
                                           class_env = new.env(),
                                           project_env = new.env(),
                                           debug = FALSE) {
  if(!exists("built_types", envir = project_env)) {
    project_env$built_types <- list()
  }
  nameSubList <- NFcompiler$nameSubList
  NFcompiler$auxEnv[['uses_nCppVec']] <- FALSE
  NFcompiler$auxEnv[['needed_nFunctions']] <- list()
  NFcompiler$auxEnv[['needed_nClasses']] <- list()
  NFcompiler$auxEnv[["parallelContent"]] <- list()
  NFcompiler$auxEnv[["derivsContent"]] <- list()
  NFcompiler$auxEnv[["class_env"]] <- class_env # used for sharing information among methods in a class
  NFcompiler$auxEnv[["project_env"]] <- project_env # used for sharing information among compilation units in a project
  NFcompiler$auxEnv[['.AllowUnknowns']] <- FALSE ## nimble note: will be FALSE for RHS recursion in setSizes
  # NFcompiler$auxEnv[['.ensureNimbleBlocks']] <- FALSE ## will be TRUE for LHS recursion after RHS sees rmnorm and other vector dist "r" calls.
  ##NFcompiler$auxEnv[['.nCompilerProject']] <- nimbleProject
  passedArgNames <- symbolTable_getArgNames(NFcompiler$symbolTable)
  passedArgNames <- structure(as.list(passedArgNames),
                              names = passedArgNames)
  NFcompiler$auxEnv[['passedArgumentNames']] <- passedArgNames ## only the names are used.
  NFcompiler$auxEnv[['nameSubList']] <- nameSubList
  NFcompiler$auxEnv[['where']] <-
    if(is.null(sourceObj)) NFcompiler$NFinternals$where
    else sourceObj
   NFcompiler$auxEnv[['closure']] <-
    if(is.null(sourceObj)) NFcompiler$NFinternals$where
    else sourceObj$parent_env
  # "where" and "closure" are the same for nimbleFunctions outside of nClasses
  # For an nClass, "where" is the nClass generator (which is an environment)
  # and "closure" is where nClass was called (or the env argument to nClass).
  invisible(NULL)
}

compilerStage_labelAbstractTypes <-
  function(NFcompiler,
           debug = FALSE) {
    NFcompiler$auxEnv$returnSymbol = NFcompiler$returnSymbol
    if(debug) {
      browser()
      labelAbstractTypesEnv$.debug <- TRUE
      nimDebug(compile_labelAbstractTypes)
      nimDebugHandlerEnv(labelAbstractTypesEnv)
      on.exit({
        labelAbstractTypesEnv$.debug <- FALSE
        nimUndebug(compile_labelAbstractTypes)
        nimUndebugHandlerEnv(labelAbstractTypesEnv)
      })
    }
    compile_labelAbstractTypes(NFcompiler$code,
                               NFcompiler$symbolTable,
                               NFcompiler$auxEnv)
    invisible(NULL)
  }


compilerStage_processAD <-
  function(NFcompiler,
           debug = FALSE) {
    if(debug) {
      browser()
      processADEnv$.debug <- TRUE
      nimDebug(compile_processAD)
      nimDebugHandlerEnv(processADEnv)
      on.exit({
        processADEnv$.debug <- FALSE
        nimUndebug(compile_processAD)
        nimUndebugHandlerEnv(processADEnv)
      })
    }
    compile_processAD(NFcompiler$code,
                      NFcompiler$symbolTable,
                      NFcompiler$auxEnv)
    invisible(NULL)
  }


compileInfo_insertAssertions <- function(NFcompiler,
                                         debug = FALSE) {
  if(debug) browser()
  tryResult <- try(exprClasses_insertAssertions(NFcompiler$code))
  if(inherits(tryResult, 'try-error')) {
    stop(
      paste('There is some problem at the insertAdditions processing',
            'step for this code:\n',
            paste(deparse(NFcompiler$origRcode),
                  collapse = '\n'),
            collapse = '\n'),
      call. = FALSE)
  }
  invisible(NULL)
}

compileInfo_eigenize <- function(NFcompiler,
                                 debug = FALSE) {
  if(debug) browser()
  compile_eigenize(NFcompiler$code,
                   NFcompiler$symbolTable,
                   NFcompiler$auxEnv)
  invisible(NULL)
}

compilerStage_addDebug <- function(NFcompiler, debug = FALSE) {
  if(debug) browser()
  workEnv <- new.env()
  workEnv$name <- NFcompiler$name
  compile_addDebug(NFcompiler$code,
                   NFcompiler$symbolTable,
                   NFcompiler$auxEnv,
                   workEnv = workEnv)
  invisible(NULL)
}
