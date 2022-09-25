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
                     interfaces = "full",
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
  # if(!isTRUE(get_nOption('use_nCompLocal'))) {
  #   loadedObjectEnv_cppDef <- make_loadedObjectEnv_cppDef()
  #   RcppPacket_list[[ length(RcppPacket_list) + 1]] <- cppDefs_2_RcppPacket(loadedObjectEnv_cppDef, "loadedObjectEnv_")
  # }
  
  ## Write the results jointly, with one .cpp file and multiple .h files.
  ## This fits Rcpp::sourceCpp's requirements.
  cppfile <- paste0(cppFileLabelFunction(),".cpp") ## "nCompiler_multiple_units.cpp"
  resultEnv <- new.env()
  compiledFuns <- cpp_nCompiler(RcppPacket_list,
                                cppfile = cppfile,
                                dir = dir,
                                cacheDir = cacheDir,
                                env = resultEnv,
                                packetList = TRUE,
                                returnList = TRUE)

  ## writeCpp_nCompiler_combine(RcppPacket_list,
  ##                            cppfile = cppfile)
  ## if(isTRUE(get_nOption('pause_after_writing_files')))
  ##   browser()
  ## resultEnv <- new.env()
  ## compiledFuns <- compileCpp_nCompiler(cppfile,
  ##                                      dir = dir,
  ##                                      cacheDir = cacheDir,
  ##                                      env = resultEnv,
  ##                                      returnList = TRUE)

  # Build full interfaces for everything, even if generic is requested in the return object.
  unit_is_nClass <- unitTypes=="nCgen"
  num_nClasses <- sum(unit_is_nClass)
  R6interfaces <- vector(mode="list", length = length(units) ) # will remain null for nFunctions
  if(num_nClasses > 0) {
    for(i in seq_along(units)) {
      if(unit_is_nClass[i]) {
        nClass_name <- names(units)[i]
        iRes <- which( paste0("new_", cpp_names[i]) == names(compiledFuns))
        if(length(iRes) != 1) {
          warning(paste0("Building R6 inteface classes: Name matching of results had a problem for ", nClass_name, "."))
        } else {
          R6interfaces[[i]] <- try(build_compiled_nClass(units[[i]],
                                                         compiledFuns[[iRes]],
                                                         env = resultEnv))
          if(inherits(R6interfaces[[i]], "try-error")) {
            warning(paste0("There was a problem building a full nClass interface. for ", nClass_name, "."))
            R6interfaces[[i]] <- NULL
          }
        }
      }
    }
  }
  names(R6interfaces) <- cpp_names
  
  if(any(unitTypes == "nCgen")) {
    newDLLenv <- make_DLLenv()
    compiledFuns <- setup_CnC_environments(compiledFuns,
                                           newDLLenv,
                                           nC_names = cpp_names[unitTypes=="nCgen"],
                                           R6interfaces = R6interfaces,
                                           returnList = TRUE)
  }
  
  ## Next we re-order results using input names,
  ## in case the ordering in the C++ code or in Rcpp's handling
  ## does not match order of units.
  ## cpp_names should be 1-to-1 with names(ans)
  ## We want to return with names(ans) changed to
  ## names(units), in the order corresponding to cpp_names.
  ans <- vector(mode="list", length = length(units))
  ans_names <- character(length = length(units))
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF") {
      iRes <- which(cpp_names[i] == names(compiledFuns)) # iRes is index in compiledFuns of the i-th unit
    } else if(unitTypes[i] == "nCgen") {
      iRes <- which( paste0("new_", cpp_names[i]) == names(compiledFuns))
    } else {
      iRes <- integer()
    }
    if(length(iRes) != 1) {
      warning(paste0("Collecting results: Name matching of results had a problem for ", names(units)[i], ".\n",
                     "  Returning list of compiled results with internal C++ names."))
      return(compiledFuns)
    }
    ans_names[i] <- names(units)[i]
    
    if(unitTypes[i] == "nF") {
      ans[[i]] <- compiledFuns[[iRes]]
    } else if(unitTypes[i] == "nCgen") {
      interfaceType <- interfaces[[ ans_names[i] ]]
      if(is.null(interfaceType))
        interfaceType <- "full"
      if(interfaceType == "full")
        ans[[i]] <- R6interfaces[[cpp_names[i] ]]
      else
        ans[[i]] <- compiledFuns[[iRes]]
    }
  }
  names(ans) <- ans_names
  
  if(is.list(ans)) { # ans should always be a list but this handles if it isn't
    if(!returnList) {
      if(length(ans) == 1) ans[[1]]
      else ans
    } else ans
  } else if(returnList) list(ans)
  else ans
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
