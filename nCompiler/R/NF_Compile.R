## We currently use Rcpp's compilation system as follows.
## 1. Start with a cppDef.
## 2. Create an RcppPacket, which organizes the content from the cppDef more usefully for Rcpp. 
## 3. Call cpp_nCompiler

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
                                  control = list(),
                                  ...) {
    ## See options.R for defaults
    controlFull <- updateDefaults(
        get_nOption('compilerOptions'),
        control
    )

    logging <- controlFull$logging

    ## will be used for C++
    funName <- substitute(NF)
    funName <- Rname2CppName(substr(deparse(funName), 1, 10))
    
    NF_Compiler <- NF_CompilerClass$new(f = NF,
                                        funName=  funName)
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
    filebase <- Rname2CppName(cppDef$name)
    RcppPacket <- cppDefs_2_RcppPacket(cppDef,
                                       filebase = filebase)
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
    
    ## Next two steps should be replaced with single call to cpp_nCompiler.  See nCompile_nClass
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


