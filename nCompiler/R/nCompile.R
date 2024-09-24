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
  # RcppPacket_list <- vector(length = length(units), mode = "list")
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF") {
      unitResults[[i]] <- nCompile_nFunction(units[[i]],
                                             stopAfterCppDef = TRUE,
                                             env = env,
                                             control = control)
      cpp_names[i] <- NFinternals(units[[i]])$cpp_code_name
#      RcppPacket_list[[i]] <- NFinternals(unitResults[[i]])$RcppPacket
    } else if(unitTypes[i] == "nCgen") {
      unitResults[[i]] <- nCompile_nClass(units[[i]],
                                          stopAfterCppDef = TRUE,
                                          env = env,
                                          control = control)
      cpp_names[i] <- NCinternals(units[[i]])$cpp_classname
 #     RcppPacket_list[[i]] <- NCinternals(unitResults[[i]])$RcppPacket
    }
  }

  allCppDefs <- c(unitResults,
                  do.call("c", lapply(unitResults, function(x) x$getExternalDefs())))
  allCppDefs <- allCppDefs[!duplicated(allCppDefs)] # preserves names. unique(allCppDefs) does not.
  RcppPacket_list <- lapply(allCppDefs, cppDefs_2_RcppPacket)

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
    compiledFuns <- setup_nClass_environments(compiledFuns,
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

createCppDefsInfo <- function(units,
                              unitTypes,
                              control) {
  if(is.null(names(units))) names(units) <- rep('', length(units))
  if(length(units) == 0) stop('No objects for compilation provided')
  unitResults <- list()
  ## names(units) should be fully populated and unique. TO-DO: check.
  cpp_names <- character(length(units))
  # RcppPacket_list <- vector(length = length(units), mode = "list")
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF") {
      unitResults[[i]] <- nCompile_nFunction(units[[i]],
                                             stopAfterCppDef = TRUE,
                                             env = env,
                                             control = control)
      cpp_names[i] <- NFinternals(units[[i]])$cpp_code_name
#      RcppPacket_list[[i]] <- NFinternals(unitResults[[i]])$RcppPacket
    } else if(unitTypes[i] == "nCgen") {
      unitResults[[i]] <- nCompile_nClass(units[[i]],
                                          stopAfterCppDef = TRUE,
                                          env = env,
                                          control = control)
      cpp_names[i] <- NCinternals(units[[i]])$cpp_classname
 #     RcppPacket_list[[i]] <- NCinternals(unitResults[[i]])$RcppPacket
    }
  }
  list(cppDefs = unitResults,
       cpp_names = cpp_names)
}

cppDefsList_2_RcppPacketList <- function(cppDefs) {
  allCppDefs <- c(cppDefs,
                  do.call("c", lapply(cppDefs, function(x) x$getExternalDefs())))
  allCppDefs <- allCppDefs[!duplicated(allCppDefs)] # preserves names. unique(allCppDefs) does not.
  RcppPacket_list <- lapply(allCppDefs, cppDefs_2_RcppPacket)
  RcppPacket_list
}

# refactor to integrate with writePackage
#
#' @export
nCompile2 <- function(...,
                     dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                     cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                     env = parent.frame(),
                     control = list(),
                     unitControls = list(),
                     interfaces = "full",
                     package = FALSE,
                     returnList = FALSE) { ## return a list even if there is only one unit being compiled.
  #(1) Put together inputs from ...
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

  # (1b) Unpack interfaces argument from various formats.
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

  # if package = TRUE, call package steps either with units or original ... (above)
  # after packing up control list (e.g. from interfaces)

  # (2) Create cppDefs
  unitTypes <- get_nCompile_types(units)
  cppDefs_info <- createCppDefsInfo(units, unitTypes, control)
  cppDefs <- cppDefs_info$cppDefs
  cpp_names <- cppDefs_info$cpp_names

  # writePackage inserts roxygen here

  # (3) Create RcppPacket_list

  # called from writePackage or not
  from_writePackage <- control$.writePackage
  if(!is.null(from_writePackage)) {
    # .writePackage content was provided, so we were called from writePackage
    control$prepared_content <- list(
      units = units,
      unitTypes = unitTypes,
      cpp_names = cpp_names,
      interfaces = interfaces,
      cppDefs = cppDefs
    )
    return(
      writePackage2(pkgName = from_writePackage$pkgName,
                    dir = dir,
                    control = control,
                    unitControls = unitControls,
                    modify = from_writePackage$modify,
                    memberData = from_writePackage$memberData,
                    roxygen = from_writePackage$roxygen
                    # see inside writePackage2 for interfaces handling -- to clean up
                    )
    )
  }

  # From here on, we were not called from writePackage
  if(package) {
    # user requests nCompile use packaging mechanism
    control$prepared_content <- list(
      units = units,
      unitTypes = unitTypes,
      cpp_names = cpp_names,
      interfaces = interfaces,
      cppDefs = cppDefs
    )
    return(
      writePackage2(pkgName = "temp_pkg", # generalize this
                    dir = dir,
                    control = control,
                    unitControls = unitControls,
                    modify = "clear",
                    memberData = list(),
                    roxygen = list()
                    )
    )
  } else {
    # We will not use packaging mechanism
    RcppPacket_list <- cppDefsList_2_RcppPacketList(cppDefs)
    return(
      nCompile2_finish_nonpackage(units = units,
                              unitTypes = unitTypes,
                              cpp_names = cpp_names,
                              interfaces = interfaces,
                              RcppPacket_list = RcppPacket_list,
                              dir = dir,
                              cacheDir = cacheDir,
                              env = env,
                              returnList = returnList))
  }
}

writePackage2 <- function(...,
                          pkgName,
                          dir = ".",
                          control = list(), # combines unit defaults and write/compile controls
                          unitControls = list(),
                          modify = get_nOption("modifyPackageFiles"),
                          memberData = list(),
                          roxygen = list(),
                          nClass_full_interface = TRUE) {
  require(Rcpp)
  pkgDir <- file.path(dir, pkgName)
  content <- control$prepared_content
  if(is.null(content)) { # this means we are being called directly, not from nCompile
    modify <- match.arg(modify, c("no", "add", "clear"))
    initializePkg <- FALSE
    if (dir.exists(pkgDir)) {
      if (modify == "no") stop(paste0("Package ", pkgName, " already exists in directory ", dir,
                                      ". Change 'modify' argument to add to or clear it."))
    } else initializePkg <- TRUE
    if(grepl("_", pkgName))
      stop("Package names are not allowed to have underscore characters.")
    # We call nCompile with control$.writePackage containing arguments needed for
    #  nCompile to recurse back to writePackage with control$prepared_content filled in.
    res <- nCompile2(...,
              dir = dir,
              env = parent.frame(), # will not be used
              control = c(control,
                          list(.writePackage=list(
                            pkgName = pkgName,
                            modify = modify,
                            memberData = memberData,
                            roxygen = roxygen
                          ))),
              unitControls = unitControls,
              interfaces = if(nClass_full_interface) "full" else "generic",
              returnList = TRUE # will not be used
              )
    # nCompile will call writePackage again, this time with control$prepared_content
    # note that nCompile may be called by the user with package=TRUE, which will then
    # also call writePackage with control$prepared_content
    return(res)
  } else {
    initializePkg <- TRUE
  }
  units <- content$units
  unitTypes <- content$unitTypes
  cpp_names <- content$cpp_names
  interfaces <- content$interfaces
  message("clean up the two kinds of interfaces argument (nCompile and writePackage)")
  nClass_full_interface <- interfaces[[1]] == "full"
  cppDefs <- content$cppDefs
  # Handle roxygen input
  if (!is.list(roxygen)) {
    if (is.character(roxygen)) roxygen <- list(roxygen)
    else stop("in writePackage: unknown roxygen type")
  }
  if (length(roxygen) == 0) {
    roxygenFlag <- "none"
  } else if (sum(nchar(names(roxygen)) > 0) == length(roxygen)) { # Are all rox entries named?
    roxygenFlag <- "names"
  } else if (length(roxygen) == length(units)) { # Are there as many rox entries as units?
    roxygenFlag <- "indices"
  } else { # If neither, we don't know what to do about it
    stop("If fewer roxygen entries are provided than objects, they must be named",
         " in the input list to indicate the objects to which they correspond.")
  }
  message("set up thorough control and unitControls handling. include $export field")
  # pkgDir <- file.path(dir, pkgName)
  Rdir <- file.path(pkgDir, "R")
  srcDir <- file.path(pkgDir, "src")
  instDir <- file.path(pkgDir, "inst")
  codeDir <- file.path(instDir, "include", "nCompGeneratedCode")
  datDir <- file.path(pkgDir, "data")
#
  WP_check_unit_types(units, unitTypes)
  cppDefs <- WP_add_roxygen_fxns_to_cppDefs(cppDefs, units, unitTypes, roxygen, roxygenFlag)
  full_interface <- WP_build_full_interfaces(units, unitTypes, nClass_full_interface)
#
  if (initializePkg)
    WP_initializePkg(pkgName, dir, pkgDir, instDir, datDir, codeDir, modify)
#
  Rfilepath <- character(length(units))
  #
  RcppPacket_list <- cppDefsList_2_RcppPacketList(cppDefs)
  ## In an earlier version, we also wrote the code to inst/include for purposes of linking to
  ## a generated package from another package. That is currently disabled and we should investigate
  ## Rcpp's Rcpp::interfaces attribute for C++ that provides some automated inst/include
  ## generation of R interface functions.
  WP_writeCpp(RcppPacket_list, srcDir, codeDir)
  WP_writeRinterfaces(units, unitTypes, nClass_full_interface,
                      Rdir, full_interface, roxygen, roxygenFlag)
  WP_writeMemberData(memberData, datDir)
  WP_write_dotOnLoad(cpp_names, unitTypes, Rdir)
  WP_write_DESCRIPTION_NAMESPACE(units, unitTypes, nClass_full_interface, initializePkg, pkgDir, pkgName)
  if (!initializePkg) {
    compiledObjs <- list.files(srcDir, pattern = "o$")
    # message("Deleting ", compiledObjs)
    unlink(compiledObjs)
  }
  compileAttributes(pkgdir = pkgDir)
  invisible(NULL)
}

nCompile2_finish_nonpackage <- function(units,
                                        unitTypes,
                                        cpp_names,
                                        interfaces,
                                        RcppPacket_list,
                                        dir,
                                        cacheDir,
                                        env,
                                        returnList) {
  cppfile <- paste0(cppFileLabelFunction(),".cpp") ## "nCompiler_multiple_units.cpp"
  resultEnv <- new.env()
  compiledFuns <- cpp_nCompiler(RcppPacket_list,
                                cppfile = cppfile,
                                dir = dir,
                                cacheDir = cacheDir,
                                env = resultEnv,
                                packetList = TRUE,
                                returnList = TRUE)
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
    compiledFuns <- setup_nClass_environments(compiledFuns,
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

WP_check_unit_types <- function(units, unitTypes) {
  for(i in seq_along(units)) {
    if((unitTypes[i] != "nF") & (unitTypes[i] != "nCgen"))
      stop(paste("In writePackage: only nFunctions and nClass generators are",
                   "allowed. Cannot compile object of class ", paste(class(units[[i]]), collapse=" ")))
  }
}

WP_add_roxygen_fxns_to_cppDefs <- function(cppDefs,
                                           units,
                                           unitTypes,
                                           roxygen,
                                           roxygenFlag) {
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF") {
      thisRox <- switch(roxygenFlag,
                        none = NULL,
                        indices = if(length(roxygen) < i) roxygen[[i]] else NULL,
                        names = if (objNames[[i]] %in% names(roxygen))
                          roxygen[[ objNames[i] ]]
                        else NULL)
      if (!is.null(thisRox)) {
        cppDefs[[i]]$commentsAbove <- c(thisRox$header, cppDefs[[i]]$commentsAbove)
        # Possible more complicated scheme to insert roxygen content immediately above Rcpp::export tag
        # in the event other comment lines also exist.
        # However I'm not sure we need these extra steps.
        ## exportIndex <- which(cppDefs[[i]]$commentsAbove == "// [[Rcpp::export]]")
        ## cppDefs[[i]]$commentsAbove <-
        ##   c(cppDefs[[i]]$commentsAbove[1:(exportIndex - 1)],
        ##     thisRox$header,
        ##     cppDefs[[i]]$commentsAbove[exportIndex:length(cppDefs[[i]]$commentsAbove)])
      }
    }
  }
  cppDefs
}

WP_build_full_interfaces <- function(units, unitTypes, nClass_full_interface) {
  full_interface <- list()
  for(i in seq_along(units)) {
    if(unitTypes[i]=="nCgen") {
      if (isTRUE(nClass_full_interface)) {
        full_interface[[i]] <- build_compiled_nClass(units[[i]], quoted = TRUE)
      }
    }
  }
  full_interface
}

WP_initializePkg <- function(pkgName,
                             dir,
                             pkgDir,
                             instDir,
                             datDir,
                             codeDir,
                             modify) {
  #   nCompiler_placeholder <<- function() NULL
  ## The following eval(substitute(...), ...) construction is necessary because
  ## Rcpp.package.skeleton (and also pkgKitten::kitten) has a bug when used
  ## directly as needed here from inside a function.
  suppressMessages(
    eval(
      substitute(
        Rcpp.package.skeleton(PN,
                              path = DIR,
                              # list = "nCompiler_placeholder",
                              author = "This package was generated by the nCompiler",
                              force = FORCE),
        list(DIR = dir,
             PN = pkgName,
             FORCE = modify == "clear")),
      envir = .GlobalEnv)
  )
  if(file.exists(file.path(pkgDir, "Read-and-delete-me")))
    file.remove(file.path(pkgDir, "Read-and-delete-me"))
  dir.create(instDir)
  dir.create(datDir)
  dir.create(codeDir, recursive = TRUE)
}

WP_writeCpp <- function(RcppPacket_list, srcDir, codeDir) {
  for (i in seq_along(RcppPacket_list)) {
    ## We write the code once for the package's DLL...
    nCompiler:::writeCpp_nCompiler(RcppPacket_list[[i]],
                                    dir = srcDir)
    ## ... and again for other packages that need to
    ## compile against this package's source code.
    ## Otherwise, C++ source code is not present in an installed package.
    ## Compiling against source code is necessary because of
    ## heavy use of C++ templates.
    nCompiler:::writeCpp_nCompiler(RcppPacket_list[[i]],
                                   dir = codeDir)
  }
}

WP_writeRinterfaces <- function(units, unitTypes, nClass_full_interface,
                                Rdir, full_interface, roxygen, roxygenFlag) {
  for (i in seq_along(units)) {
    if ((unitTypes[i]=='nCgen') && isTRUE(nClass_full_interface)) {
      ## Write the nClass full interface to the package's R directory
      generator_name <- units[[i]]$classname # note this may differ from the name in input list
      Rfile <- paste0(generator_name, '.R')
      Rfilepath[i] <- file.path(Rdir, Rfile)
      con <- file(Rfilepath[i], open = 'w')
      deparsed_full_interface <- deparse(full_interface[[i]])
      deparsed_full_interface[1] <- paste0(
        generator_name, ' <- ', deparsed_full_interface[1]
      )
      exportTag <- "#' @export\n" #if (totalControl[[i]]$export) "#' @export\n" else NULL
      # Retrieve roxygen entry
      thisRox <- switch(roxygenFlag,
                        none = NULL,
                        indices = if(length(roxygen) < i) roxygen[[i]] else NULL,
                        names = roxygen[[ names(units)[i] ]]
                        )
      if (!is.null(thisRox)) {
        # Find the spot where each documented method is defined
        for (m in 1:length(thisRox$methods)) {
          thisDefn <- grep(paste0(names(thisRox$methods)[m], " = function("),
                           deparsed_full_interface, fixed = TRUE)
          targetStr <- deparsed_full_interface[thisDefn]
          deparsed_full_interface[thisDefn] <-
            gsub(pattern = names(thisRox$methods)[m],
                 replacement = paste0(
                   "\n", thisRox$methods[m], "\n", names(thisRox$methods)[m]
                 ),
                 x = deparsed_full_interface[thisDefn],
                 fixed = TRUE)
        }
      }
      deparsed_full_interface <- c(
        '## Generated by nCompiler::writePackage() -> do not edit by hand\n',
        if (is.list(thisRox)) thisRox[["header"]] else thisRox,
        exportTag,
        deparsed_full_interface,
        paste0(generator_name, '$parent_env <- new.env()'),
        paste0(generator_name, '$.newCobjFun <- NULL')
      )
      writeLines(deparsed_full_interface, con)
      close(con)
    }
  }
}

WP_writeMemberData <- function(memberData, datDir) {
  # Write out data
  if (length(memberData) > 0) {
    datEnv <- as.environment(memberData)
    ls_datEnv <- ls(datEnv)
    for (i in seq_along(ls_datEnv)) {
      save(list = ls_datEnv[i], envir = datEnv,
           file = file.path(datDir, paste0(ls_datEnv[i], ".RData")))
    }
  }
}

WP_write_dotOnLoad <- function(cpp_names, unitTypes, Rdir) {
  nClass_names <- cpp_names[unitTypes=="nCgen"]
  #  nClass_names <- unlist(lapply(objs, function(x)
  #    if(isNCgenerator(x)) x$classname else NULL
  #    ))
  if(length(nClass_names)) {
    onLoad_lines <- c(".onLoad <- function(libName, pkgName) {\n",
                      paste0(" nCompiler::setup_nClass_environments_from_package(c(",
                             paste0("\"",nClass_names,"\"", collapse = ", "),  "))\n"),
                      "NULL}\n")
    writeLines(onLoad_lines, con = file.path(Rdir, "zzz.R"))
  }
}

WP_write_DESCRIPTION_NAMESPACE <- function(units, unitTypes, nClass_full_interface,
                                           initializePkg, pkgDir, pkgName) {
  DESCfile <- file.path(pkgDir, "DESCRIPTION")
  NAMEfile <- file.path(pkgDir, "NAMESPACE")
  if (initializePkg) {
    DESCRIPTION <- read.dcf(DESCfile)
    ## TO-DO: Make choice of what to include be smart about what is really needed.
    ## A nFunction might only need:
    ## DESCRIPTION[1, "LinkingTo"] <- paste(DESCRIPTION[1, "LinkingTo"], "RcppEigen", "RcppParallel", "nCompiler", sep = ",")
    ## A nClass might need:
    DESCRIPTION[1, "LinkingTo"] <- paste(DESCRIPTION[1, "LinkingTo"], "RcppEigen", "RcppEigenAD", "RcppParallel", "nCompiler", "Rcereal", sep = ",")
    # DESCRIPTION$Encoding <- "UTF-8"
    ## It is conceivable that nCompLocal will need to be added to this at some point.
    ## If so, it will need to be installed in R's main library, not some local location.
    # DESCRIPTION[1, "Collate"] <- paste(Rfilepath, collapse = ", ")
    write.dcf(DESCRIPTION, DESCfile)
    NAMESPACE <- c(paste0("useDynLib(", pkgName, ", .registration=TRUE)"),
                   "importFrom(Rcpp, evalCpp)",
                   "export(nComp_serialize_)",
                   "export(nComp_deserialize_)",
                   "export(call_method)",
                   "export(get_value)",
                   "export(set_value)"
                   )
  } else {
    NAMESPACE <- readLines(NAMEfile)
  }
  for (i in seq_along(units)) {
    # if (totalControl[[i]]$export && isNCgenerator(objs[[i]]))
    #    if (totalControl[[i]]$export) {
    if(TRUE) {
      if (unitTypes[i]=="nF" || nClass_full_interface) {
        # NAMESPACE <- c(NAMESPACE, paste0("export(", objNames[i], ")"))
        NAMESPACE <- c(NAMESPACE, paste0("export(", units[[i]]$name, ")"))
      }
      if(unitTypes[i] == "nCgen") {
        #  NAMESPACE <- c(NAMESPACE, paste0("export(new_", objNames[i], ")"))
        NAMESPACE <- c(NAMESPACE, paste0("export(new_", units[[i]]$classname, ")"))
      }
    }
  }
  NAMESPACE <- unique(NAMESPACE)
  writeLines(NAMESPACE, con = NAMEfile)
}
