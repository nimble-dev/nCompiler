nFunctionIDMaker <- labelFunctionCreator('NFID')

NFvirtual_CompilerClass <- R6::R6Class(
  classname = 'NFvirtual_CompilerClass',
  portable = FALSE,
  public = list(
    name = NULL,
    origName = NULL,
    NFinternals = NULL,
    stageCompleted = 'start',
    nameSubList = NULL,
    origRcode = NULL,
    newRcode = NULL,
    symbolTable = NULL,
    returnSymbol = NULL,
    code = NULL,
    auxEnv = new.env(),
    ##... to here
    const = NULL,
    needed_nFunctions = list(), #Each list element will be a list with (name, env), so that nGet(name, env) returns the nFunction
    needed_nClasses = list(), #Each list element will be an NCgenerator (returned by nClass). Populated only from "$new()" usags.
    initialTypeInferenceDone = FALSE,
    initialize = function(f = NULL,
                          ## funName,
                          const = FALSE,
                          useUniqueNameInCpp = FALSE) {
      const <<- const
      if(!is.null(f)) {
        isNFinternals <- inherits(f, 'NF_InternalsClass')
        if(!(isNF(f) || isNFinternals)) {
          stop('Attempt to compile something is neither an nFunction nor an object of class NF_InternalsClass')
        }
        if(isNFinternals) {
          NFinternals <<- f
        } else {
          NFinternals <<- NFinternals(f)
        }
        origName <<- NFinternals$uniqueName
        if (useUniqueNameInCpp) name <<- NFinternals$uniqueName
        else name <<- NFinternals$cpp_code_name
        origRcode <<- NFinternals$code
        newRcode <<- NFinternals$code
      }
    },
    showCpp = function() {
      writeCode(
        compile_generateCpp(code, symbolTable)
      )
    },
    setupSymbolTable = function(parentSymbolTable = NULL) {
      argNames <- NFinternals$argSymTab$getSymbolNames()
      symbolTable <<- NFinternals$argSymTab$clone(deep = TRUE)
      mangledArgumentNames <- mangleArgumentNames( argNames )
      symbolTable$setSymbolNames(mangledArgumentNames)

      nameSubList <<- lapply(mangledArgumentNames,
                             as.name)
      names(nameSubList) <<- argNames

      if(!is.null(parentSymbolTable)) {
        symbolTable$setParentST(parentSymbolTable)
      }

      returnSymbol <<- NFinternals$returnSym$clone(deep = TRUE)
    },
    process = function(...) {
      if(is.null(symbolTable)) {
        setupSymbolTable()
      }
    }
  )
)

NF_CompilerClass <- R6::R6Class(
  'NF_CompilerClass',
  inherit = NFvirtual_CompilerClass,
  portable = FALSE,
  public = list(
    cppDef = NULL,
    ##Rwrapper = NULL,
    createCpp = function(control = list(),
                         sourceObj = NULL) {
      ## Do all steps to create C++ (and R wrapper).
      ## When the function is a class method, the NC_CompilerClass
      ## object manages these steps by calling process() and createCppInternal().
      controlFull <- updateDefaults(
        get_nOption('compilerOptions'),
        control
      )
      process(control = controlFull,
              sourceObj = sourceObj)
      createCppInternal()
    },
    createCppInternal = function() {
      cppDef <<- cpp_nFunctionClass$new(
        name = self$name
      )
      ## It would be nice if self were not stored in cppDef
      ## but for now it is.
      cppDef$buildFunction(self)
      ##cppDef$buildSEXPwrapper()
      ##Rwrapper <<- cppDef$buildRwrapper()
      invisible(NULL)
    },
    process = function(control = list(),
                       sourceObj = NULL,
                       doKeywords = TRUE, ## deprecated?
                       .nCompilerProject = NULL,  ## deprecated?
                       initialTypeInferenceOnly = FALSE) { ## deprecated?
      ## Do all steps of manipulating the abstract syntax tree
      ## to the point where it is ready to be used for C++ generation.
      controlFull <- updateDefaults(
        get_nOption('compilerOptions'),
        control
      )
      processNFstages(self,
                      controlFull,
                      sourceObj,
                      doKeywords,
                      .nCompilerProject,
                      initialTypeInferenceOnly)
    }
  )
)

processNFstages <- function(NFcompiler,
                            control = list(),
                            sourceObj = NULL,
                            doKeywords = TRUE,
                            .nCompilerProject = NULL,
                            initialTypeInferenceOnly = FALSE) {
  ## Do all steps of manipulating the abstract syntax tree
  ## to the point where it is ready to be used for C++ generation.
  controlFull <- updateDefaults(
    get_nOption('compilerOptions'),
    control
  )
  debug <- controlFull$debug
  debugCpp <- FALSE
  cppStacktrace <- controlFull$cppStacktrace
  logging <- controlFull$logging

  startStage <- controlFull$startStage
  endStage <- controlFull$endStage
  use_nCompiler_error_handling <- controlFull$use_nCompiler_error_handling

  if(debug) browser()

  nameMsg <- paste0("(for method or nFunction ", NFcompiler$origName, ")")

  if (logging)
    nDebugEnv$compilerLog <- c(
      nDebugEnv$compilerLog,
      paste("---- Begin compilation log", nameMsg, '----\n'),
      "Original R code", "--------",
      capture.output(NFcompiler$origRcode), "--------\n",
      "Argument Symbol Table", "--------",
      capture.output(NFcompiler$NFinternals$argSymTab), "--------\n",
      "Return Type", "--------",
      capture.output(NFcompiler$NFinternals$returnSym),
      "--------\n"
    )

  ### SET INPUT AND OUTPUT TYPES
  stageName <- 'setInputOutputTypes'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry({
      if(!NFcompiler$initialTypeInferenceDone) {
        if(doKeywords) {
          ## NEW: possibly implement these steps
          ##      using the exprClass code, not the R code
          ## matchKeywords()
          ## processKeywords()
        }
        ## If this function is a class, method, its symbolTable
        ## may have already been created by the NC_CompilerClass object
        ## calling setupSymbolTable()
        if(is.null(NFcompiler$symbolTable)) {
          NFcompiler$setupSymbolTable()
        }
        NFcompiler$initialTypeInferenceDone <- TRUE
      }

      if(initialTypeInferenceOnly)
        return(NULL)
    },
    stageName,
    use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
    if (logging) logAfterStage(stageName)
  }
  ### SET MANGLED ARGUMENT NAMES (e.g. x -> ARG1_X_)
  stageName <- 'substituteMangledArgumentNames'
  if (logging) logBeforeStage(stageName)
  ## simple substitution of the mangled argument names
  ## whereever they are used.
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry(
      compilerStage_substituteMangledArgumentNames(
        NFcompiler,
        NFcompiler$nameSubList,
        debug),
      stageName,
      use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
  }
  ## INITIALIZE CODE
  stageName <- 'initializeCode'
  if (logging) logBeforeStage(stageName)
  ## set up abstract syntax tree (exprClass objects)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry(
      compilerStage_initializeCode(
        NFcompiler,
        debug),
      stageName,
      use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName

    if (logging) {
      logAST(NFcompiler$code, 'AST initialized:',
             showType = FALSE, showImpl = FALSE)
      logAfterStage(stageName)
    }

    ## Initialize initializerList if present (only for constructors)
    if(!is.null(NFcompiler$NFinternals$aux)) {
      if(!is.null(NFcompiler$NFinternals$aux$initializerList)) {
        NFcompiler$NFinternals$aux$initializerList_exprClasses <-
          lapply(NFcompiler$NFinternals$aux$initializerList, nParse)
      }
    }

  }

  stageName <- 'initializeAuxiliaryEnvironment'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry({
      compilerStage_initializeAuxEnv(NFcompiler,
                                     sourceObj,
                                     debug)
    },
    stageName,
    use_nCompiler_error_handling)
    resolveTBDsymbols(NFcompiler$symbolTable,
                      env = NFcompiler$auxEnv[['where']])
    NFcompiler$returnSymbol <- resolveOneTBDsymbol(NFcompiler$returnSymbol,
                                                   env = NFcompiler$auxEnv[['where']])
    if(inherits(NFcompiler$returnSymbol, "symbolNC")) {
      NFcompiler$auxEnv$needed_nClasses <- c(NFcompiler$auxEnv$needed_nClasses, NFcompiler$returnSymbol$NCgenerator)
    }
    NFcompiler$stageCompleted <- stageName
    if (logging) logAfterStage(stageName)
  }

  ## SIMPLE TRANSFORMATIONS (e.g. name changes)
  stageName <- 'simpleTransformations'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    ## Make modifications that do not need size processing
    NFtry(
      compilerStage_simpleTransformations(
        NFcompiler,
        debug),
      stageName,
      use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
    if (logging) {
      logAST(NFcompiler$code, showType = FALSE, showImpl = FALSE)
      logAfterStage(stageName)
    }
  }

  ## build intermediate variables:
  ## Currently this only affects eigen, chol, and run.time
  stageName <- 'simpleIntermediates'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry({
      compilerStage_simpleIntermediates(NFcompiler,
                                        debug)
    },
    stageName,
    use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
    if (logging) {
      logAST(NFcompiler$code, showType = FALSE, showImpl = FALSE)
      logAfterStage(stageName)
    }
  }

  ## annotate sizes and types
  stageName <- 'labelAbstractTypes'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry({
      compilerStage_labelAbstractTypes(NFcompiler,
                                       debug)
      
      # This will only collect nClasses from classGenerator$new()
      # Other nClasses will end up in the symbolTable and be 
      # collected later.
      NFcompiler$needed_nClasses <-
        c(NFcompiler$needed_nClasses,
          NFcompiler$auxEnv[['needed_nClasses']])
      
      NFcompiler$needed_nFunctions <-
        c(NFcompiler$needed_nFunctions,
          NFcompiler$auxEnv[['needed_nFunctions']])
    },
    paste(stageName, nameMsg),
    use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
    if (logging) {
      logAST(NFcompiler$code, showImpl = FALSE)
      logAfterStage(stageName)
    }
  }

  ## insert new lines created by size processing
  stageName <- 'addInsertions'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry({
      compileInfo_insertAssertions(NFcompiler,
                                   debug)
    },
    stageName,
    use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
    if (logging) {
      logAST(NFcompiler$code)
      logAfterStage(stageName)
    }
  }

  ## create symbol table of Eigen implementation types from symbol table of abstract types
  stageName <- 'setImplementation'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry({
      symbolTable_setImplementation(NFcompiler$symbolTable,
                                    "Eigen")
    },
    stageName,
    use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
    if (logging) logAfterStage(stageName)
  }

  ## modify code either for Eigen or Tensorflow back-end
  stageName <- 'doImplementation'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(!NFcompilerMaybeSkip(stageName, controlFull)) {
    eval(NFcompilerMaybeDebug(stageName, controlFull))
    NFtry(
      compileInfo_eigenize(NFcompiler,
                           debug),
      stageName,
      use_nCompiler_error_handling)
    NFcompiler$stageCompleted <- stageName
  }
  ## place Eigen maps in code
  ## This step may be moot in the new design
  exprClasses_liftMaps(NFcompiler$code,
                       NFcompiler$symbolTable,
                       NFcompiler$auxEnv)
  NFcompiler$stageCompleted <- stageName

  if (logging) {
    logAST(NFcompiler$code)
    logAfterStage(stageName)
  }

  ## Expand into fully-fledged stage: finalTransformations
  NFtry(
    compilerStage_finalTransformations(NFcompiler,
                                       debug),
    "finalTransformations",
    use_nCompiler_error_handling)

  stageName <- 'addDebugging'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) return(invisible(NULL))
  if(cppStacktrace) {
    if(debug) writeLines('*** Inserting debugging')
    NFtry(
      compilerStage_addDebug(NFcompiler, debug),
      stageName,
      use_nCompiler_error_handling
    )
  }
  if(debug & debugCpp) {
    writeCode(
      compile_generateCpp(NFcompiler$code,
                          NFcompiler$symbolTable)
    )
  }
  NFcompiler$stageCompleted <- stageName

  if (logging)
    logAfterStage(stageName)
}
