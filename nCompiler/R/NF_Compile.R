## We currently use Rcpp's compilation system as follows.
## 1. Start with a cppDef.
## 2. Create an RcppPacket, which organizes the content from the cppDef more usefully for Rcpp. 
## 3. Call cpp_nCompiler

## There are four potential names associated with an nFunction:
## 1. its NFinternals uniqueName
## 2. its NFinternals cpp_code_name
## 3. a name of it as an R object (which it doesn't know)
## 4. a name of it as a compiled nFunction (which we attempt to deduce reasonably).

#' Compile a nFunction
#'
#' Compiler a nFunction via automatic generation and compilation of C++.
#'
#' @param NF A nFunction to be compiled.
#' @param dir Directory where generated C++ will be written.
#' @param cacheDir Directory to be used for Rcpp cache.
#' @param env Environment for loading compiled results.
#' @param control = List of control parameters.
#' @return A function that will internally call the compiled C++ code.
#' @export
nCompile_nFunction <- function(NF,
                               dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                               cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                               env = parent.frame(),
                               compileInfo = NULL,
                               control = list(),
                              ## name,
                               ...) {
  ## ... is used for internal arguments that are not necessarily documents or 
  ## promised to stay stable.
  dotArgs <- list(...)
  
  if(!isNF(NF))
    stop(paste0("Argument NF must be an nFunction."))
  ## When called from nCompile, stopAfterRcppPacket will be TRUE.
  ## While this could also be done from the control() list, 
  ## we leave that to the user.  E.g. That might set endStage even
  ## earlier.
  stopAfterRcppPacket <- isTRUE(dotArgs$stopAfterRcppPacket)
  stopAfterCppDef <- isTRUE(dotArgs$stopAfterCppDef)
  ## See options.R for defaults
  controlFull <- updateDefaults(
    get_nOption('compilerOptions'),
    control
  )
  loggingOpt <- get_nOption('compilerOptions')[['logging']]
  logging <- controlFull$logging
  
  if (logging != loggingOpt) { ## control and nOptions values differ
    ## to make logging option accessible in handlers, must set in .nOptions
    set_nOption('logging', logging, 'compilerOptions')
    ## reset compilerOptions$logging to previous value
    on.exit(set_nOption('logging', loggingOpt, 'compilerOptions'))
  }

  if(is.null(compileInfo)) compileInfo <- NFinternals(NF)$compileInfo

  NF_Compiler <- NF_CompilerClass$new(f = NF, 
                                      useUniqueNameInCpp = 
                                        controlFull$useUniqueNameInCode,
                                      compileInfo = compileInfo)
  NF_Compiler$createCpp(control = controlFull)
  if(NFcompilerMaybeStopAfter(NF_Compiler$stageCompleted,
                              controlFull)) {
    if(get_nOption('verbose')) 
      message(paste0("Returning after stage ",
                     NF_Compiler$stageCompleted))
    return(NF_Compiler)
  }
  stageName <- 'makeRcppPacket'
  if (logging) logBeforeStage(stageName)
  if(NFcompilerMaybeStop(stageName, controlFull)) 
    return(NF_Compiler)
  
  cppDef <- NF_Compiler$cppDef
  if(stopAfterCppDef) return(cppDef)

  # We might deprecate from here down and make all usages start from nCompile.

  ## We append "_c_" so that the filename does not match the function name.
  ## That prevents Rcpp's "context" system from making a mistake
  ## when we combine multiple RcppPacket contents into a single file.
  ## It seems to check if there are any .cpp files with names that
  ## match names of exported functions, and to think they should
  ## be compiled if so.  If one has compiled a nFunction individually
  ## and then in combination with other files, this could cause a problem.
  #filebase <- make_cpp_filebase(cppDef$name) ## append _c_
  RcppPacket <- cppDefs_2_RcppPacket(cppDef) # filebase now handles default
#                                     filebase = filebase)
  NFinternals(NF)$RcppPacket <- RcppPacket
  NF_Compiler$stageCompleted <- stageName
  
  if (logging) {
    nDebugEnv$compilerLog <- c(
      nDebugEnv$compilerLog,
      'RcppPacket C++ content', '--------',
      paste(RcppPacket$cppContent, collapse = '\n'),
      '--------\n',
      'RcppPacket header content', '--------',
      paste(RcppPacket$hContent, collapse = '\n'),
      '--------\n'
    )
    logAfterStage(stageName)
    nameMsg <- paste0("(for method or nFunction ", NF_Compiler$origName, ")")
    appendToLog(paste("---- End compilation log", nameMsg, " ----\n"))
  }
  
  if(stopAfterRcppPacket) 
    return(NF)
  ## Next two steps should be replaced with single call to cpp_nCompiler.
  ## See nCompile_nClass
  stageName <- 'writeCpp'
  if(NFcompilerMaybeStop(stageName, controlFull)) 
    return(NF)
  if(isTRUE(controlFull$writeCpp)) {
    writeCpp_nCompiler(RcppPacket,
                       dir = dir)
    if(isTRUE(get_nOption('pause_after_writing_files')))
      browser()
  }
  NF_Compiler$stageCompleted <- stageName
  
  stageName = 'compileCpp'    
  if(NFcompilerMaybeStop(stageName, controlFull)) 
    return(NF)
  
  if(isTRUE(controlFull$compileCpp)) {
    ans <- compileCpp_nCompiler(RcppPacket,
                                dir = dir,
                                cacheDir = cacheDir,
                                env = env)
    NF_Compiler$stageCompleted <- stageName
    return(ans)
  }
  NF
}
