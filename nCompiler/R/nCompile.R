cppFileLabelFunction <- labelFunctionCreator('nCompiler_units')

# How names are handled through nCompile
#
# When nCompile is called with an nClass generator (as an argument):
#   - names are taken either from named arguments in the ... or from deparsing the ... elements
#   - These become the names of origList and then of the units list
#
# nClass
# Each unit that is an nClass is passed to nCompile_nClass to create C++ code.
#   - This makes an NC_Compiler object, which has a name <<- Rname2CppName(NCgenerator$classname)
#   - The NC_Compiler$createCpp method is called to create a cppDef with name = name of NC_Compiler object
#   - The SEXPgenerator C++ function in the cppDef is named paste0("new_", name)
#   - This then is used to create an RcppPacket.
#   - In nCompile, the cpp_name for that unitResult is the class name for the nClass generator
#
# compileCpp_nCompiler calls sourceCpp_nCompiler, which calls Rcpp::sourceCpp
# compileCpp_nCompiler arranges the results into a named list of the [[Rcpp::export]] functions
# This will include the SEXPgenerator C++ functtion named  paste0("new_", name)
# 
# nFunction
# Each unit that is an nFunction is passed to nCompile_nFunction to create C++ code
# The nFunction already has an NFinternals with a name, uniqueName, and cpp_code_name
#    - That function determines which name to use based on useUniqueNameInCpp,
#       which will have been pulled from get_nOption('compilerOptions')
#    - The NF_CompilerClass uses an origName (the uniqueName) and
#          a name, which defaults to the NFinternals$cpp_code_name, otherwise uniqueName.
#    - NF_CompilerClass$createCpp is called to create the C++ code.
#          The cppDef$name is same as name (and NFinternals$cpp_code_name).
#    - That becomes the name in C++ code.
#    - In nCompile, the cpp_name for that unitResult is the cpp_code_name

#' @export
nCompile <- function(...,
                     dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                     cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                     env = parent.frame(),
                     control = list(),
                     interfaces = list(),
                     returnList = FALSE) { ## return a list even if there is only one unit being compiled.
  dotsDeparses <- unlist(lapply( substitute(list(...))[-1], deparse ))
  origList <- list(...)
  if(is.null(names(origList))) 
    names(origList) <- rep('', length(origList))
  boolNoName <- names(origList)==''
  origIsList <- unlist(lapply(origList, is.list))
  for(i in which(origIsList)) {
    if(is.null(names(origList[[i]])) || any(names(origList[[i]])==""))
      stop("If you provide a list of compilation units, all list elements must be named.")
  }
  dotsDeparses[origIsList] <- ''
  names(origList)[boolNoName] <- dotsDeparses[boolNoName] # This puts default names from deparsing ... entries into list
  units <- do.call('c', origList)

  # Unpack interfaces argument from various formats.
  # Remember interface is only needed for nClass compilation units
  if(!is.list(interfaces)) {
    if(is.character(interfaces)) {
      if(length(interfaces) == 1) {
        interfaces <- rep(interfaces, length(units))
        names(interfaces) <- names(units) # nFunction units will just be ignored
      }
    }
    interfaces <- as.list(interfaces)
  }

  unitTypes <- get_nCompile_types(units)
  if(is.null(names(units))) names(units) <- rep('', length(units))
  if(length(units) == 0) stop('No objects for compilation provided')
  unitResults <- list()
  ## names(units) should be fully populated and unique. TO-DO: check.
  cpp_names <- character(length(units))
  RcppPacket_list <- vector(length = length(units), mode = "list")
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF") {
      unitResults[[i]] <- nCompile_nFunction(units[[i]],
                                             stopAfterRcppPacket = TRUE,
                                             env = env,
                                             control = control)
      cpp_names[i] <- NFinternals(units[[i]])$cpp_code_name
      RcppPacket_list[[i]] <- NFinternals(unitResults[[i]])$RcppPacket
    } else if(unitTypes[i] == "nCgen") {
      unitResults[[i]] <- nCompile_nClass(units[[i]],
                                          stopAfterRcppPacket = TRUE,
                                          env = env,
                                          control = control)
      cpp_names[i] <- units[[i]]$classname
      RcppPacket_list[[i]] <- NCinternals(unitResults[[i]])$RcppPacket
    }
  }

  if(isTRUE(get_nOption('serialize'))) {
    serial_cppDef <- make_serialization_cppDef()
    RcppPacket_list[[ length(RcppPacket_list) + 1]] <- cppDefs_2_RcppPacket(serial_cppDef, "serialization_")
  }
  if(!isTRUE(get_nOption('use_nCompLocal'))) {
    loadedObjectEnv_cppDef <- make_loadedObjectEnv_cppDef()
    RcppPacket_list[[ length(RcppPacket_list) + 1]] <- cppDefs_2_RcppPacket(loadedObjectEnv_cppDef, "loadedObjectEnv_")
  }
  
  ## Write the results jointly, with one .cpp file and multiple .h files.
  ## This fits Rcpp::sourceCpp's requirements.
  cppfile <- paste0(cppFileLabelFunction(),".cpp") ## "nCompiler_multiple_units.cpp"
  writeCpp_nCompiler_combine(RcppPacket_list,
                             cppfile = cppfile)
  if(isTRUE(get_nOption('pause_after_writing_files')))
    browser()
  resultEnv <- new.env()
  ans <- compileCpp_nCompiler(cppfile,
                              dir = dir,
                              cacheDir = cacheDir,
                              env = resultEnv,
                              returnList = returnList)

  newDLLenv <- make_DLLenv()
  ans <- setup_DLLenv(ans, newDLLenv, returnList = returnList)
  
  setup_nClass_interface <- function(interfaceType,
                                     NC,
                                     ans,
                                     env) {
    ans <- wrapNCgenerator_for_DLLenv(ans, newDLLenv)
    if(interfaceType == "generic")
      return(ans)
    fullInterface <- try(build_compiled_nClass(NC,
                                               ans,
                                               env = env))
    if(inherits(fullInterface, "try-error")) {
      warning("There was a problem building a full nClass interface.\n",
              "Attempting to return a generic interface.\n")
      return(ans)
    }
    if(interfaceType == "full")
      return(fullInterface)
    else if(interfaceType == "both")
      return(list(full = fullInterface, generic = ans))
    warning(paste0("Invalid interface type ", interfaceType, " requested.\n",
                   "Returning a full interface.\n"))
    fullInterface
  }
  
  ## Next we re-order results using input names,
  ## in case the ordering in the C++ code or in Rcpp's handling
  ## does not match order of units.
  ## cpp_names should be 1-to-1 with names(ans)
  ## We want to return with names(ans) changed to
  ## names(units), in the order corresponding to cpp_names.
  if(is.list(ans)) {
    newNames <- names(ans)
    for(i in seq_along(units)) {
      if(unitTypes[i] == "nF") {
        iRes <- which(cpp_names[i] == names(ans))
      } else if(unitTypes[i] == "nCgen") {
        iRes <- which( paste0("new_", cpp_names[i]) == names(ans))
      } else {
        iRes <- integer()
      }
      if(length(iRes) != 1) {
        warning("Name matching of results had a problem.  Returning list of compiled results with internal C++ names.")
        return(ans)
      }
      newNames[iRes] <- names(units)[i]

      if(unitTypes[i] == "nCgen") {
        nClass_name <- newNames[iRes]
        interfaceType <- interfaces[[ nClass_name ]]
        if(is.null(interfaceType))
          interfaceType <- "generic"
        ans[[iRes]] <- setup_nClass_interface(interfaceType,
                                              units[[i]],
                                              ans[[iRes]],
                                              env = resultEnv)        
      }
    }
    names(ans) <- newNames
  } else {
    if(unitTypes[[1]] == "nCgen") {
      interfaceType <- "full"
      if(length(interfaces) > 0) {
        interfaceType <- interfaces[[1]]
        if(is.null(interfaceType))
          interfaceType <- "full"
      }
      ans <- setup_nClass_interface(interfaceType,
                                    units[[1]],
                                    ans,
                                    env = resultEnv)
    }
  }
  ans
}

get_nCompile_types <- function(units) {
  ans <- character(length(units))
  for(i in seq_along(units)) {
      if(isNF(units[[i]])) ans[i] <- 'nF'
    else if(isNCgenerator(units[[i]])) ans[i] <- 'nCgen'
    else if(isNC(units[[i]])) 
      stop(paste0("The #", i, " object to be compiled is an nClass object.\n",
                  "Only nClass generators (the class definition, not an object of the class) should be compiled."),
           call.=FALSE)
    else stop(paste0("The #", i, " object to be compiled is neither an nFunction nor an nClass generator (class definition).\n"))
  }
  ans
}
