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

  is_predefined <- !isFALSE(NFinternals(NF)$predefined)
  gather_needed_units <- isTRUE(controlFull$always_include_units)
  needed_units <- list(needed_nClasses = list(),
                       needed_nFunctions = list())
  allow_write_predefined <- FALSE
  if(is_predefined) {
    predefined_dir <-  NFinternals(NF)$predefined
    # predefined can be character, quoted expression, or function.
    # The latter two allow delayed evaluation, useful if an nClass is defined
    # in an R package and the predefined argument should not get build-system
    # paths baked in but rather delay until evaluation on the when running.
    if(is.call(predefined_dir)) {
      predefined_dir <- eval(predefined_dir, envir = NFinternals(NF)$where)
    }
    if(is.function(predefined_dir)) {
      predefined_dir <- predefined_dir()
    }
    if(!is.character(predefined_dir))
      stop("There is a predefined nFunction whose predefined field is not (and does not evaluate to) character. ",
       "It should give the directory path of the predefined nFunction. ",
       "The name argument to nFunction gives the base for filenames in that directory.")
    regular_filename <-  NFinternals(NF)$cpp_code_name
    if(gather_needed_units) 
      needed_units <- nCompile_process_manual_needed_units(NFinternals(NF))
    allow_write_predefined <- !isTRUE(compileInfo$auto_included)
  }
  if(is_predefined && isFALSE(controlFull$generate_predefined)) {
    RcppPacket <- loadRcppPacket(predefined_dir, regular_filename)
    cppDef <- cppRcppPacket$new(RcppPacket = RcppPacket)
  } else {
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
    if(is_predefined && allow_write_predefined) {
      predefined_gen_dir <- NFinternals(NF)$compileInfo$predefined_output_dir
      if(is.null(predefined_gen_dir))
        predefined_gen_dir <- predefined_dir
      RcppPacket <- cppDefs_2_RcppPacket(NF_Compiler$cppDef)
      saveRcppPacket(RcppPacket, predefined_dir, regular_filename)
    } else {
      if(gather_needed_units) needed_units <- NF_Compiler$gather_needed_units()
    }
    stageName <- 'makeRcppPacket'
    if (logging) logBeforeStage(stageName)
    if(NFcompilerMaybeStop(stageName, controlFull)) 
      return(NF_Compiler)
  
    cppDef <- NF_Compiler$cppDef
  }
  if(stopAfterCppDef) {
    if(gather_needed_units) return(list(needed_units = needed_units, cppDef = cppDef))
    else return(cppDef)
  }
  # We might deprecate from here down and make all usages start from nCompile.

  stop("Entering deprecated portion of nCompile_nFunction. Check what is going on.")

  ## We append "_c_" so that the filename does not match the function name.
  ## That prevents Rcpp's "context" system from making a mistake
  ## when we combine multiple RcppPacket contents into a single file.
  ## It seems to check if there are any .cpp files with names that
  ## match names of exported functions, and to think they should
  ## be compiled if so.  If one has compiled a nFunction individually
  ## and then in combination with other files, this could cause a problem.
  #filebase <- make_cpp_filebase(cppDef$name) ## append _c_

##   RcppPacket <- cppDefs_2_RcppPacket(cppDef) # filebase now handles default
## #                                     filebase = filebase)
##   NFinternals(NF)$RcppPacket <- RcppPacket
##   NF_Compiler$stageCompleted <- stageName

##   if (logging) {
##     nDebugEnv$compilerLog <- c(
##       nDebugEnv$compilerLog,
##       'RcppPacket C++ content', '--------',
##       paste(RcppPacket$cppContent, collapse = '\n'),
##       '--------\n',
##       'RcppPacket header content', '--------',
##       paste(RcppPacket$hContent, collapse = '\n'),
##       '--------\n'
##     )
##     logAfterStage(stageName)
##     nameMsg <- paste0("(for method or nFunction ", NF_Compiler$origName, ")")
##     appendToLog(paste("---- End compilation log", nameMsg, " ----\n"))
##   }

##   if(stopAfterRcppPacket)
##     return(NF)
##   ## Next two steps should be replaced with single call to cpp_nCompiler.
##   ## See nCompile_nClass
##   stageName <- 'writeCpp'
##   if(NFcompilerMaybeStop(stageName, controlFull))
##     return(NF)
##   if(isTRUE(controlFull$writeCpp)) {
##     writeCpp_nCompiler(RcppPacket,
##                        dir = dir)
##     if(isTRUE(get_nOption('pause_after_writing_files')))
##       browser()
##   }
##   NF_Compiler$stageCompleted <- stageName

##   stageName = 'compileCpp'
##   if(NFcompilerMaybeStop(stageName, controlFull))
##     return(NF)

##   if(isTRUE(controlFull$compileCpp)) {
##     ans <- compileCpp_nCompiler(RcppPacket,
##                                 dir = dir,
##                                 cacheDir = cacheDir,
##                                 env = env)
##     NF_Compiler$stageCompleted <- stageName
##     return(ans)
##   }
##   NF
}
