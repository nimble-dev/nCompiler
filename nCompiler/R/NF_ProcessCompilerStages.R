
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
                                           debug = FALSE) {
  nameSubList <- NFcompiler$nameSubList
  NFcompiler$auxEnv[['needed_nFunctions']] <- list()
  NFcompiler$auxEnv[["parallelContent"]] <- list()
  NFcompiler$auxEnv[['.AllowUnknowns']] <- TRUE ## will be FALSE for RHS recursion in setSizes
  NFcompiler$auxEnv[['.ensureNimbleBlocks']] <- FALSE ## will be TRUE for LHS recursion after RHS sees rmnorm and other vector dist "r" calls.
  ##NFcompiler$auxEnv[['.nCompilerProject']] <- nimbleProject
  passedArgNames <- symbolTable_getArgNames(NFcompiler$symbolTable)
  passedArgNames <- structure(as.list(passedArgNames),
                              names = passedArgNames)
  NFcompiler$auxEnv[['passedArgumentNames']] <- passedArgNames ## only the names are used.
  NFcompiler$auxEnv[['nameSubList']] <- nameSubList
  NFcompiler$auxEnv[['where']] <- if(is.null(sourceObj))
                                      NFcompiler$NFinternals$where
  else
    sourceObj
  invisible(NULL)
}


compilerStage_labelAbstractTypes <-
  function(compileInfo,
           debug = FALSE) {
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
    compile_labelAbstractTypes(compileInfo$code,
                               compileInfo$symbolTable,
                               compileInfo$auxEnv)
    invisible(NULL)
  }

compileInfo_insertAssertions <- function(compileInfo,
                                         debug = FALSE) {
  if(debug) browser()
  tryResult <- try(exprClasses_insertAssertions(compileInfo$code))
  if(inherits(tryResult, 'try-error')) {
    stop(
      paste('There is some problem at the insertAdditions processing',
            'step for this code:\n',
            paste(deparse(compileInfo$origRcode),
                  collapse = '\n'),
            collapse = '\n'),
      call. = FALSE)
  }
  invisible(NULL)
}

compileInfo_eigenize <- function(compileInfo,
                                 debug = FALSE) {
  if(debug) browser()
  compile_eigenize(compileInfo$code,
                   compileInfo$symbolTable,
                   compileInfo$auxEnv)
  invisible(NULL)
}

compilerStage_addDebug <- function(compileInfo, debug = FALSE) {
  if(debug) browser()
  workEnv <- new.env()
  workEnv$name <- compileInfo$name
  compile_addDebug(compileInfo$code,
                   compileInfo$symbolTable,
                   compileInfo$auxEnv,
                   workEnv = workEnv)
  invisible(NULL)
}