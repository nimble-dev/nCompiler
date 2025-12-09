#' Compile a nClass.
#'
#' Generates C++ for the compilable data and methods of a nClass,
#' manages C++ compilation of them and returns a generator for obejcts
#' of the compiled class.
#'
#' @param NC A nClass generator (returned from a call to \link{nClass}).
#'
#' @param dir Directory where generated C++ will be written.
#'
#' @param cacheDir Directory to be used for Rcpp cache.
#'
#' @param env Environment to be used for loading results of compilation.
#'
#' @param control List of control settings for compilation.  See...
#'
#' @return Generator of objects of the compiled version of class
#'     \code{NC}.  These will use C++ objects internally for compiled
#'     data and methods.
#'
#' @export
nCompile_nClass <- function(NC,
                            dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                            cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                            env = parent.frame(),
                            compileInfo = NULL,
                            control = list(),
                            interface = c("full", "generic", "both"),
                            ...) {
  ## ... is used for internal arguments that are not necessarily documents or
  ## promised to stay stable.
  dotArgs <- list(...)

  if(!isNCgenerator(NC))
    stop(paste0("Argument NC must be an nClass generator."))

  ## When called from nCompile, stopAfterRcppPacket will be TRUE.
  ## While this could also be done from the control() list,
  ## we leave that to the user.  E.g. That might set endStage even
  ## earlier.
  stopAfterRcppPacket <- isTRUE(dotArgs$stopAfterRcppPacket)
  stopAfterCppDef <- isTRUE(dotArgs$stopAfterCppDef)

  controlFull <- updateDefaults(
    get_nOption('compilerOptions'),
    control
  )
  is_predefined <- !isFALSE(NCinternals(NC)$predefined)
  gather_needed_units <- isTRUE(controlFull$always_include_units)
  needed_units <- list(needed_nClasses = list(),
                       needed_nFunctions = list())
  allow_write_predefined <- FALSE
  if(is_predefined) {
    predefined_dir <-  NCinternals(NC)$predefined
    # predefined can be character, quoted expression, or function.
    # The latter two allow delayed evaluation, useful if an nClass is defined
    # in an R package and the predefined argument should not get build-system
    # paths baked in but rather delay until evaluation on the when running.
    if(is.call(predefined_dir)) {
      predefined_dir <- eval(predefined_dir, envir = NCinternals(NC)$env)
    }
    if(is.function(predefined_dir)) {
      predefined_dir <- predefined_dir()
    }
    if(!is.character(predefined_dir))
      stop("There is a predefined nClass whose predefined field is not (and does not evaluate to) character. ",
       "It should give the directory path of the predefined nClass. ",
       "The classname argument to nClass gives the base for filenames in that directory.")
    regular_filename <-  NCinternals(NC)$cpp_classname
    if(gather_needed_units)
      needed_units <- nCompile_process_manual_needed_units(NCinternals(NC),
                                                                NC$parent_env, isNC = TRUE)
    allow_write_predefined <- !isTRUE(compileInfo$auto_included)
  }
  if(is_predefined && isFALSE(controlFull$generate_predefined)) {
    RcppPacket <- loadRcppPacket(predefined_dir, regular_filename)
    cppDef <- cppRcppPacket$new(RcppPacket = RcppPacket)
    cppDef$externalCppDefs <- c(cppDef$externalCppDefs,
                                get_R_interface_cppDef()) #might not be needed, but doesn't hurt to add and we don't have the details on whether it is needed from the loaded RcppPacket.
 } else {
    if(is.null(compileInfo)) compileInfo <- NCinternals(NC)$compileInfo
    ## Make a new compiler object
    NC_Compiler <- NC_CompilerClass$new(NC,
                                        compileInfo = compileInfo)
    ## Use the compiler to generate a cppDef
    NC_Compiler$createCpp(control = controlFull,
                          sourceObj = NC,
                          interfaceCalls = !is_predefined) ## We don't retain NC in NC_Compiler in order to simplify many environments pointing to each other.
    ## Get the cppDef
    cppDef <- NC_Compiler$cppDef
    if(is_predefined && allow_write_predefined) {
      predefined_gen_dir <- NCinternals(NC)$compileInfo$predefined_output_dir
      if(is.null(predefined_gen_dir))
        predefined_gen_dir <- predefined_dir      
      RcppPacket <- cppDefs_2_RcppPacket(cppDef)
      saveRcppPacket(RcppPacket, predefined_gen_dir, regular_filename)
      # Now add interface calls if necessary for this live compilation, having
      # kept them out of the written packet code.
      cppDef$buildGenericInterface(interfaceCalls=TRUE, interface=FALSE)
      # To do: check that there aren't any detected needed units that are not in the compileInfo$needed_units
      # because for a predefined, needed units must be provided manually by compileInfo.
    } else {
      if(gather_needed_units) needed_units <- NC_Compiler$gather_needed_units()
    }

    ##
    ## if(isTRUE(get_nOption('serialize')))
    ##   cppDef$addSerialization(include_DLL_funs = !stopAfterRcppPacket)
    ## if(isTRUE(get_nOption('automaticDerivatives')))
    ##   cppDef$addADclassContent()
    ## cppDef$addGenericInterface()
    if(NFcompilerMaybeStop('makeRcppPacket', controlFull))
      return(NC_Compiler)
  }

  if(stopAfterCppDef) {
    if(gather_needed_units) return(list(cppDef = cppDef, needed_units = needed_units))
    else return(cppDef)
  }
  # We might deprecate from here onward.
  # Then nCompile_nClass would only be called via nCompile
  filebase <- controlFull$filename

  if(is.null(filebase))
    filebase <- make_cpp_filebase(cppDef$name)
  RcppPacket <- cppDefs_2_RcppPacket(cppDef,
                                     filebase = filebase)
  NCinternals(NC)$RcppPacket <- RcppPacket

  if(stopAfterRcppPacket)
    return(NC)

  compiledFuns <- cpp_nCompiler(RcppPacket,
                              dir = dir,
                              cacheDir = cacheDir,
                              env = env,
                              write = !NFcompilerMaybeStop('writeCpp', controlFull),
                              compile = !NFcompilerMaybeStop('compileCpp', controlFull),
                              ...)
  if(NFcompilerMaybeStop('compileCpp', controlFull)) {
    return(compiledFuns)
  }

  R6interface <- list(build_compiled_nClass(NC, compiledFuns, env = env))
  names(R6interface) <- cppDef$name # formerly filebase

  interface <- match.arg(interface)

  newDLLenv <- make_DLLenv()
  # newCobjFun <- setup_DLLenv(newCobjFun, newDLLenv)
  finalFun <- setup_nClass_environments(compiledFuns,
                                        newDLLenv,
                                        #nC_names = NC$classname,
                                        R6interfaces = R6interface)

  if(length(finalFun) != 1)
    warning("There may be a problem with number of returned functions in nCompile_nClass.")
  #  newCobjFun <- wrapNCgenerator_for_DLLenv(newCobjFun, newDLLenv)

  if(interface == "generic")
    return(finalFun[[1]])
  if(interface == "full")
    return(R6interface[[1]])
  ## interface is "both"
  return(list(full = R6interface[[1]], generic = finalFun[[1]]))
}
