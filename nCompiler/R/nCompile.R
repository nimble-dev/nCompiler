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
#   - Note that cpp_code_name of methods may be intercepted later to match base class
#     cpp_code_name if the method is virtual and inherited.
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
#
# Addendum:
#  exportNames are names exported from sourceCpp, i.e.from Rcpp::export labeling.
#. returnNames are the names returned to the user from nCompile.
#
# compiled generator names when going through writePackage:
#  This is a subtle issue.
#. If a user has written an nClass with an inherit argument for another nClass,
#. we *must* preserve the name of the base class so that it will be found by
#. the derived class by the name used when programming them. Since we don't know
#. where or when inheritance might be done, that means we should preserve all original names.
#
#. However, from the input to nCompile or writePackage, we don't know those names.
#  We only know the intended *returnNames*. In some cases (and when parsed from unnamed inputs),
#  those may match the original generator object name.
#
#  There is also a different need for writePackage called directly vs nCompile with package=TRUE.
#
#. For nCompile with package=TRUE, the user should give returnName matching generator name
#.  (or let that happen by not providing a name). Then the uncompiled generator in the
#   written package code will have the correct name for inheritance, and the same name
#.  can be used for the list element of the compiled generator returned to the user.
#.  (If there is no nClass inheritance, the user can safely change the returnName.)
#
#  However, for writePackage called directly, that scheme will not work. We *must*
#. preserve the original uncompiled generator name (for any inherts) and we must have a distinct
#. object name for the compiled generator.
#  To do that we use compileInfo$classname. A default value of which (if needed) is
#  created by the original NC by appending "_compiled".
#
#  In internal code, the uncompiled generator name will be uncomp_gen_name
#. and the compiled generator name will be comp_gen_name.
#
# Although the compiled_name is strictly only necessary for writePackage called directly,
# we will support and use it also for nCompile with package=TRUE so that that can be used
# for development and testing with the expectation that it mirrors what writePackage does.
#
# There are somewhat corresponding issues of the `classname` arguments (aka tags)
# that are appended on the `class` attribute. For an nClass "nc", we will make
# a base class with `classname="nc"`; the compiled class will get this by `inherits`
# and also `classname="nc_compiled"`. S3 dispatch like `print.nc` will catch both compiled
# and uncompiled. One can also use the isCompiled() method.

#' @export
get_nCompile_types <- function(units) {
  ans <- character(length(units))
  for(i in seq_along(units)) {
    if(isNF(units[[i]])) {
      ans[i] <- if(NFinternals(units[[i]])$compileInfo$callFromR)
                  'nF' else 'nF_noExport'
    } else if(isNCgenerator(units[[i]])) ans[i] <- 'nCgen'
    else if(isNC(units[[i]]))
      stop(paste0("The #", i, " object to be compiled is an nClass object.\n",
                  "Only nClass generators (the class definition, not an object of the class) should be compiled."),
           call.=FALSE)
    else stop(paste0("The #", i, " object to be compiled is neither an nFunction nor an nClass generator (class definition).\n"))
  }
  ans
}

nCompile_createCppDefsInfo <- function(units,
                              unitTypes,
                              control,
                              compileInfos,
                              project_env = new.env()) {
  if(is.null(names(units))) names(units) <- rep('', length(units))
  if(length(units) == 0) stop('No objects for compilation provided')
  unitResults <- vector("list", length(units))
  cpp_names <- character(length(units))
  needed_nClasses <- vector("list", length(units))
  needed_nFunctions <- vector("list", length(units))
  for(i in seq_along(units)) {
    compileInfo <- compileInfos[[i]]
    if(unitTypes[i] == "nF" || unitTypes[i] == "nF_noExport") {
      oneResult <- nCompile_nFunction(units[[i]],
                                      stopAfterCppDef = TRUE,
                                      env = env,
                                      compileInfo = compileInfo,
                                      control = control,
                                      project_env = project_env)
      cpp_names[i] <- NFinternals(units[[i]])$cpp_code_name
    } else if(unitTypes[i] == "nCgen") {
      oneResult <- nCompile_nClass(units[[i]],
                                  stopAfterCppDef = TRUE,
                                  env = env,
                                  compileInfo = compileInfo,
                                  control = control,
                                  project_env = project_env)
      cpp_names[i] <- NCinternals(units[[i]])$cpp_classname
    }
    if(!is.list(oneResult)) stop("nCompile_nFunction or nCompile_nClass did not return a list for ", cpp_names[i])
    unitResults[[i]] <- oneResult$cppDef
    needed_nClasses[[i]] <- oneResult$needed_units$needed_nClasses
    needed_nFunctions[[i]] <- oneResult$needed_units$needed_nFunctions
  }
  list(cppDefs = unitResults,
       cpp_names = cpp_names,
       needed_nClasses = needed_nClasses,
       needed_nFunctions = needed_nFunctions)
}

cppDefsList_2_RcppPacketList <- function(cppDefs) {
  allCppDefs <- c(cppDefs,
                  do.call("c", lapply(cppDefs, function(x) x$getExternalDefs())))
  allCppDefs <- allCppDefs[!duplicated(allCppDefs)] # preserves names. unique(allCppDefs) does not.
  RcppPacket_list <- lapply(allCppDefs, cppDefs_2_RcppPacket)
  RcppPacket_list
}

# prepare information for compilation units:
#.  names, interface type, unit types, inherits.
# previously this was done inside nCompile, but
# now we separate it so we can recurse on units
# that need other units that then need prepared
# information
nCompile_prepare_units <- function(...,
                        #  dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                        #  cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                        #  env = parent.frame(),
                        #  control = list(),
                        #  unitControls = list(),
                          interfaces = list()#,
                        #  package = FALSE,
                        #  returnList = FALSE
                        ) {
  #(1) Put together inputs from ...
  # cat("starting nCompile\n")
  # dotsDeparses is used to get default names for unnamed inputs.
  dotsDeparses <- unlist(lapply( substitute(list(...))[-1], deparse ))
  # origList can have several kinds of elements:
  # nc = nc_generator
  # nf = nFunction
  # not_used = list(nc1 = nc1, nf1 = nf2, etc) (in which case the name of the list isn't used)

  origList <- list(...)
  if(is.null(names(origList)))
    names(origList) <- rep('', length(origList))
  boolNoName <- names(origList)==''
  origIsList <- unlist(lapply(origList, is.list))
  dotsDeparses[origIsList] <- ''
  names(origList)[boolNoName & !origIsList] <- "NO_NAME_PROVIDED"
  for(i in which(origIsList)) {
    if(is.null(names(origList[[i]])))
      names(origList[[i]]) <- rep('', length(origList[[i]]))
  }
  dotsDeparse_boolNoName_before_c <- dotsDeparses[boolNoName & !origIsList] # This puts default names from deparsing ... entries into list
  # for `nCompile(A = foo, foo2))`, names(units) will be c("A", "foo2")
  units <- do.call('c', origList)
  boolNoName <- names(units)=='NO_NAME_PROVIDED' | names(units)==''
  names(units)[names(units)=='NO_NAME_PROVIDED'] <- dotsDeparse_boolNoName_before_c

  inputNamesInfo <- list(names = names(units), boolNameProvided = !boolNoName)

  # (1b) Unpack interfaces argument from various formats.
  # Remember interface is only needed for nClass compilation units
  # If a single value ("full", "generic", or "none") is provided via
  # the interfaces argument, it will over-ride nClass-specific values
  #  from compileInfos. If a named vector of list is provided,
  #  they will over-ride on a one-by-one bases.
  if(!is.list(interfaces)) {
    if(is.character(interfaces)) {
      if(length(interfaces) == 1) {
        interfaces <- rep(interfaces, length(units))
        names(interfaces) <- names(units) # nFunction units will just be ignored
      }
    }
    interfaces <- as.list(interfaces)
  }
  # check for invalid interface names.
  # We could add a check for interfaces that are not for nClasses.
  if(!all(names(interfaces) %in% names(units))) {
    i_bad_names <- which(!(names(interfaces) %in% names(units)))
    stop("Some names in 'interfaces' do not match names of compilation units in '...':",
         paste(names(interfaces)[i_bad_names], collapse=','))
  }
  # make interfaces match the order of units and fill in NULLs when no interface is provided
  # (which will be the case for nFunctions)
  for(un in names(units))
    if(!(un %in% names(interfaces))) interfaces[un] <- ""
  interfaces <- interfaces[names(units)]

  unitTypes <- get_nCompile_types(units)

  # We defer processing of nClass inheritance until compile time to allow nClass
  # to be called with inherit = some_nClass before some_nClass is defined.
  # Note that each step must be done for all units before the next step is done
  # for all units.
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nCgen")
      NCinternals(units[[i]])$connect_inherit()
  }
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nCgen")
      NCinternals(units[[i]])$process_inherit()
  }
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nCgen")
      NC_check_inheritance(units[[i]])
  }
  # set up exportNames, returnNames, and packageNames
  # exportNames: These appear in C++ as [[Rcpp::export(exportName = <exportName>)]].
  #              Thus they give the name of the R function that will call the C++ version of the nFunction or nClass generator function.
  # returnNames: These are the names used in the list returned from nCompile.
  # packageNames: Each element is a vector of two names. The first is the
  #               uncompiled generator name in pacakge code. The second is the
  #               compiled generator name in package code.
  #               If there is inheritance, the uncompiled generator name should be the one
  #               used for inheritance by derived classes.
  #
  # Note that we have retained information from the inputs (... or list) on
  #  (i) whether a name was affirmatively provided (name = unit) and
  #. (ii) what a default name from deparsing the ... would be.
  #.      with the possibility that the default name is "" if the input was from an unnamed list element.
  #
  # The naming decisions are as follows:
  # exportName defaults to compileInfo$exportName, if provided.
  # returnName defaults to name from inputs, if affirmatively provided.
  # packageNames defaults to compileInfo$packageNames, if provided,
  #               normalized as first element "uncompiled", second element "compiled".
  #               (Note: compileInfo$packageNames$uncompiled will have already defaulted to classname
  #                      if classname was provided in the call to nClass.)
  # [nFunction case:] packageNames$compiled next defaults to exportName.
  # returnName next defaults to packageNames$compiled, if provided (or just set in nFunction case)
  # returnName next defaults to default name from inputs (deparsed from ...), if available.
  # exportName next defaults to returnName, if provided.
  # returnName next defaults to exportName, if provided.
  # [nClass case:] exportName and returnName next default to the classname
  # [nFunction case:] exportName defaults to NFinternals -> cpp_code_name;
  #                   returnName defaults to name in NFinternals -> name
  # [next four steps will only matter in nClass case,
  #.   because packageNames$compiled is now set for nFunction case]
  # If neither part of packageNames is provided,
  #.   packageNames$compiled defaults to returnName.
  # If we have only packageNames$compiled (provided, or defaulting to returnName)
  #.   packageNames$uncompiled next defaults to the deparsed ... name, if available, otherwise returnName, if distinct,
  #.   otherwise paste0(packageNames$compiled, "_uncompiled")
  #.     THIS DEFAULT is natural for simple cases but COULD BREAK inherits arguments.
  #.     Hence we may want to error-trap on this later.
  # If only packageNames$uncompiled is provided:
  #    packageNames$compiled next defaults to returnName
  # If packageNames$compiled and packageNames$uncompiled match (even possibly by faulty user input)
  #.   append "_compiled" to packageNames$compiled
  #    This may be then hard to find if unexpected by the user, but they need to give better input.
  #
  # [nClass case:] If exportName matches either packageNames element:
  #.   if interface=="full", exportName gets "_new" appended.
  #    Otherwise the conflicting name gets "_compiled" or "_uncompiled" appended
  #
  # Examples:
  # (1) if no compileInfo$exportName or compileInfo$packageNames is provided,
  #  then exportName and returnName will both be the name from the input list (or deparsed name),
  #  and exportName will get "_new" appended if interface=="full".
  #
  # (2) if compileInfo$packageNames is provided and the input is an unnamed list,
  #
  # For nFunctions:
  # Most of the naming decisions are the same as for nClasses.
  # Key difference: the exportName and packageNames$compiled are redunant.
  #.  Whatever is exported from Rcpp is directly the function in R to call that C++ function.
  #   Hence the separate steps above
  #
  # DEPRECATED COMMENTS TO REMOVE WHEN CLEANING UP
  # exportNames will be from names(units) if named in the call or there is no exportName in the NF or NC compileInfo
  # Otherwise (i.e. no name provided in call and there is an exportName in the object def), use the exportName in the object def (compileInfo)
  #
  # The only case where exportNames and returnNames will be different will be an nClass with full interface.
  # Then exportName is the new-object function and that needs to be different from the
  # returned name for the nClass generator.
  # e.g. for nc1, exportName will be new_nc1 but returnName will be nc1.
  returnNames <- exportNames <- vector("character", length(units))
  packageNames <- vector("list", length(units))
  compileInfos <- structure(vector("list", length(units)),
                            names = names(units))
  for(i in seq_along(units)) {
    if(unitTypes[i] == "nF" || unitTypes[i] == "nF_noExport") {
      compileInfo <- NFinternals(units[[i]])$compileInfo
      case <- "NF"
    } else {
      compileInfo <- NCinternals(units[[i]])$compileInfo
      case <- "NC"
      if(interfaces[[i]] == "")
        interfaces[[i]] <- compileInfo$interface
      if(!(interfaces[[i]] %in% c("full", "generic", "none")))
        stop("Could not determine a valid interface value ('full', 'generic', or 'none') for ", names(units)[i])
    }

    if(!is.null(compileInfo$exportName)) exportNames[i] <- compileInfo$exportName
    if(isTRUE(inputNamesInfo$boolNameProvided[i])) returnNames[i] <- inputNamesInfo$names[i]
    if(!is.null(compileInfo$packageNames)) packageNames[[i]] <- compileInfo$packageNames
    if(is.null(packageNames[[i]])) packageNames[[i]] <- c(uncompiled = "", compiled = "")
    # Note other packageNames normalization is done in NC_InternalsClass$initialize
    # and if a classname was provided, the packageNames$uncompiled defaults to that classname
    # We do some normalization here because a user could have modified the compileInfo$packageNames
    # manually and left one or other element empty.
    if(is.na(packageNames[[i]]["compiled"])) packageNames[[i]]["compiled"] <- ""
    if(is.na(packageNames[[i]]["uncompiled"])) packageNames[[i]]["uncompiled"] <- ""
    packageNames[[i]] <- packageNames[[i]][c("uncompiled", "compiled")]

    if(case == "NF")
      if(packageNames[[i]]["compiled"] == "" && exportNames[i] != "")
        packageNames[[i]]["compiled"] <- exportNames[i]

    # Further "it just works" handling of packageNames can be provided.
    # For now it should have named elements "uncompiled" and "compiled".
    if(returnNames[i] == "")
      if(packageNames[[i]]["compiled"] != "")
        returnNames[i] <- packageNames[[i]]["compiled"]
    if(returnNames[i] == "")
      if(inputNamesInfo$names[i] != "")
        returnNames[i] <- inputNamesInfo$names[i]
    if(exportNames[i] == "") exportNames[i] <- returnNames[i]
    if(returnNames[i] == "") returnNames[i] <- exportNames[i]
    if(returnNames[i] == "" && exportNames[i] == "")
      if(case == "NC")
        returnNames[i] <- exportNames[i] <- NCinternals(units[[i]])$classname
      else if(case == "NF") {
        exportNames[i] <- NFinternals(units[[i]])$cpp_code_name
        returnNames[i] <- NFinternals(units[[i]])$name
      }

    if(case == "NC") {
      # First three of these steps should never be relevant for an nFunction anyway,
      # but it is clearer to but them within this if
      # neither part of packageNames provided
      if(packageNames[[i]]["compiled"] == "" && packageNames[[i]]["uncompiled"] == "")
        packageNames[[i]]["compiled"] <- returnNames[i]
      # packageNames$compiled only (provided or just set to default returnName)
      if(packageNames[[i]]["uncompiled"] == "" && packageNames[[i]]["compiled"] != "") {
        if(inputNamesInfo$names[i] != "") packageNames[[i]]["uncompiled"] <- inputNamesInfo$names[i]
        else  packageNames[[i]]["uncompiled"] <- returnNames[i]
        # if packageNames now match, disambiguate by changing uncompiled name
        if(packageNames[[i]]["uncompiled"] == packageNames[[i]]["compiled"])
          packageNames[[i]]["uncompiled"] <- paste0(packageNames[[i]]["compiled"], "_uncompiled")
        # Any class inheriting from this class will likely not find it if we are in this
        # case of default handling. The user must provide packageNames to fix this.
      }
      # packageNames$uncompiled only provided)
      if(packageNames[[i]]["compiled"] == "" && packageNames[[i]]["uncompiled"] != "") {
        packageNames[[i]]["compiled"] <- returnNames[i]
      }

      if(packageNames[[i]]["compiled"] == packageNames[[i]]["uncompiled"])
        packageNames[[i]]["compiled"] <- paste0(packageNames[[i]]["uncompiled"], "_compiled")

      if(exportNames[i] %in% packageNames[[i]]) {
        if(interfaces[i] == "full") {
          exportNames[i] <- paste0(exportNames[i], "_new")
        } else {
          if(exportNames[i] == packageNames[[i]]["compiled"])
            packageNames[[i]]["compiled"] <- paste0(packageNames[[i]]["compiled"], "_compiled")
          else if(exportNames[i] == packageNames[[i]]["uncompiled"])
            packageNames[[i]]["uncompiled"] <- paste0(packageNames[[i]]["uncompiled"], "_uncompiled")
          ## DANGER! With interface != "full", the exportName gets dominance because that will be the
          ##  generator function for generic C++ objects. This means the uncompiled name must be changed
          ##. if it clashes, and this means any inherits or usage of the name will fail.
          ##  Error-trapping this cleanly can't be done (easily) right here because its a question of whether this is
          ##. used anywhere by other code being compiled.
        }
      }
    }

    if(names(units)[i] == "") names(units)[i] <- returnNames[i]

    # In some cases this is the first addition of an exportName to a compileInfo
    compileInfo$exportName <- exportNames[i]
    compileInfo$interface <- interfaces[[i]]
    compileInfo$packageNames <- packageNames[[i]]
    compileInfos[[i]] <- compileInfo
  }
  list(units = units,
     unitTypes = unitTypes,
     interfaces = interfaces,
     compileInfos = compileInfos,
     exportNames = exportNames,
     returnNames = returnNames,
     packageNames = packageNames)
}

#' @export
nCompile <- function(...,
                     dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                     cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                     env = parent.frame(),
                     control = list(),
                     unitControls = list(),
                     interfaces = list(),
                     package = FALSE,
                     returnList = FALSE) { ## return a list even if there is only one unit being compiled.
  #(1) Put together inputs from ...
  # cat("starting nCompile\n")

  controlFull <- updateDefaults(
    get_nOption('compilerOptions'),
    control
  )
  controlFull$always_include_units <- TRUE # Do this even if auto_include_units is FALSE, so we can error-trap

  unit_info <- nCompile_prepare_units(...,
                                      interfaces = interfaces)
  new_units <- unit_info$units
  new_unitTypes <- unit_info$unitTypes
  new_interfaces <- unit_info$interfaces
  new_compileInfos <- unit_info$compileInfos
  new_exportNames <- unit_info$exportNames # names for Rcpp::export[[exportName = fnName]]
  new_returnNames <- unit_info$returnNames # names for returning to user from nCompile
  new_packageNames <- unit_info$packageNames

  # if package = TRUE, call package steps either with units or original ... (above)
  # after packing up control list (e.g. from interfaces)
  # (2) Create cppDefs
  # cat("making cppDefs\n")
  done_finding_units <- FALSE
  units <- list()
  unitTypes <- character()
  interfaces <- list()
  compileInfos <- list()
  exportNames <- character()
  returnNames <- character()
  packageNames <- list()
  cppDefs <- list()
  cpp_names <- character()
  auto_included <- rep(FALSE, length(new_units))
  # the compileInfos$auto_included field is used in nCompile_nFunction and nCompile_nClass
  # to decide whether it is allowed to generate predefined code. For auto_included units, NO.
  new_compileInfos <- new_compileInfos |> lapply(\(x) {x$auto_included <- FALSE; x})

  cppDefs_project_env <- new.env()

  while(!done_finding_units) {
    cppDefs_info <- nCompile_createCppDefsInfo(new_units, new_unitTypes, controlFull, new_compileInfos, cppDefs_project_env)
    new_cppDefs <- cppDefs_info$cppDefs
    new_cpp_names <- cppDefs_info$cpp_names

    units <- c(units, new_units)
    unitTypes <- c(unitTypes, new_unitTypes)
    interfaces <- c(interfaces, new_interfaces)
    compileInfos <- c(compileInfos, new_compileInfos)
    exportNames <- c(exportNames, new_exportNames)
    returnNames <- c(returnNames, new_returnNames)
    packageNames <- c(packageNames, new_packageNames)
    cppDefs <- c(cppDefs, new_cppDefs)
    cpp_names <- c(cpp_names, new_cpp_names)

    new_needed_nClasses <- do.call("c", cppDefs_info$needed_nClasses) |> unique()
    new_needed_nFunctions <- do.call("c", cppDefs_info$needed_nFunctions) |> unique()
    names(new_needed_nClasses) <- new_needed_nClasses |> lapply(\(x) x$classname)
    names(new_needed_nFunctions) <- new_needed_nFunctions |> lapply(\(x) NFinternals(x)$uniqueName)
    # A bit of design irony: At this point, the needed units are
    # nicely organized into nClasses and nFunctions,
    # but we are going to mix them together as if they were an arbitrary
    # input list because that's what nCompiler_prepare_units and nCompile_createCppDefsInfo uses.
    new_units <- c(new_needed_nClasses, new_needed_nFunctions)
    ## We need to make our own version of setdiff as it won't work on these types.
    ## For now we rely on identical(). If this gets clunky or inefficient,
    ## we can refine, but that would then need looking at types of each comparison
    ## to decide how to do the comparison.
    keep_new_unit <- rep(TRUE, length(new_units))
    for(i in seq_along(new_units)) {
      this_new_unit <- new_units[[i]]
      for(j in seq_along(units)) {
        if(identical(this_new_unit, units[[j]])) {
          keep_new_unit[i] <- FALSE
          break
        }
      }
    }
    new_units <- new_units[keep_new_unit]
    if(length(new_units) == 0) {
        done_finding_units <- TRUE
    } else {
      if(isTRUE(controlFull$nCompile_include_units)) {
        # rely on any included unit having compileInfo$interface set.
        new_unit_info <- nCompile_prepare_units(new_units)
        new_units <- new_unit_info$units
        new_unitTypes <- new_unit_info$unitTypes
        new_interfaces <- new_unit_info$interfaces
        new_compileInfos <- new_unit_info$compileInfos
        new_exportNames <- new_unit_info$exportNames
        new_returnNames <- new_unit_info$returnNames
        new_packageNames <- new_unit_info$packageNames
        auto_included <- c(auto_included, rep(TRUE, length(new_units)))
        new_compileInfos <- new_compileInfos |> lapply(\(x) {x$auto_included <- TRUE; x})
      } else {
        stop("During compilation, additional units (nClasses or nFunctions) were needed but were not provided in the nCompile call. ",
            "To have nCompile automatically include such units, include control(nCompile_include_units=TRUE) or (to change the setting for all calls) do set_nOption(\"nCompile_include_units\", TRUE, \"compilerOptions\").",
            "The missing units are:\n", paste(names(new_units), collapse = "\n"))
      }
    }
  }

  if(isTRUE(controlFull$return_cppDefs)) return(cppDefs)

  # writePackage inserts roxygen here

  # (3) Create RcppPacket_list

  # called from writePackage or not
  from_writePackage <- controlFull$.writePackage
  if(!is.null(from_writePackage) || package) {
    createFromR <- compileInfos |> lapply(\(x) !isFALSE(x$createFromR)) |> unlist()
    controlFull$prepared_content <- list(
      units = units,
      unitTypes = unitTypes,
      cpp_names = cpp_names,
      interfaces = interfaces,
      createFromR = createFromR,
      cppDefs = cppDefs,
      exportNames = exportNames,
      returnNames = returnNames,
      packageNames = packageNames
    )
  }

  if(!is.null(from_writePackage)) {
    # .writePackage content was provided, so we were called from writePackage
    # cat("doing return(writePackage(...))\n")
    return(
      writePackage(pkgName = from_writePackage$pkgName,
                    dir = dir,
                    control = controlFull,
                    unitControls = unitControls,
                    modify = from_writePackage$modify,
                    memberData = from_writePackage$memberData,
                    roxygen = from_writePackage$roxygen
                    # see inside writePackage for interfaces handling -- to clean up
                    )
    )
  }

  # From here on, we were not called from writePackage
  if(package) {
    # cat("calling writePackage for package mode\n")
    # user requests nCompile use packaging mechanism
    temppkgname <- basename(tempfile("TEMPPKG", ""))
    writePackage(pkgName = temppkgname,
                 dir = dir,
                 control = controlFull,
                 unitControls = unitControls,
                 modify = "clear",
                 memberData = list(),
                 roxygen = list()
                 )
    if(isTRUE(get_nOption('pause_after_writing_files')))
      browser()
    lib <- file.path(tempdir(), "templib")
    if(!dir.exists(lib)) dir.create(lib, recursive=TRUE)
    pkgDir <- file.path(dir, temppkgname)
    # cat("about to try devtools::install\n")
    ans <- try({
      withr::with_libpaths(lib, action="prefix", code = {
        devtools::install(pkgDir,
                          quick = TRUE,
                          quiet = !isTRUE(get_nOption("showCompilerOutput")),
                          upgrade = "never") # Make quiet follow showCompilerOutput
        withr::with_libpaths(lib, action="prefix",
                            code = loadNamespace(temppkgname))
      })
      pkgNames <- returnNames
      pc <- controlFull$prepared_content
      for(i in seq_along(pc$units)) {
        if(pc$unitTypes[[i]] == "nCgen") {
          if(pc$interfaces[[i]] == "full")
            pkgNames[i] <- pc$packageNames[[i]][["compiled"]]
          else
            pkgNames[i] <- pc$exportNames[i]
        }
      }
      pkgEnv <- getNamespace(temppkgname)
      ans_ <- lapply(pkgNames, function(x) {
        if(exists(x, envir = pkgEnv, inherits=FALSE)) #might not exist for interface="none"
          get(x, envir = pkgEnv, inherits=FALSE)
        else NULL
      })
      names(ans_) <- returnNames
      # Remove any auto_included entries.
      # See comment below in nCompile_finish_package about this step.
      if(any(auto_included)) {
        ans_ <- ans_[!auto_included]
      }
      ans_
    })
    # cat("done trying devtools::install\n")
    if(inherits(ans, "try-error")) {
      stop("It looks like the package code was generated without stopping,\n",
           "but there was an error installing or loading it.\n",
           "If you want to inspect the package code, ",
           "it is in ", pkgDir, ".")
    }
    if(length(ans)==1)
      if(!returnList) return(ans[[1]])
    return(ans)
  } else {
    # We will not use packaging mechanism
    RcppPacket_list <- cppDefsList_2_RcppPacketList(cppDefs)
    # cat("doing nCompile_finish_nonpackage\n")
    return(
      nCompile_finish_nonpackage(units = units,
                                 cppDefs = cppDefs,
                                 unitTypes = unitTypes,
                                 cpp_names = cpp_names,
                            #     exportNames = exportNames,
                                 returnNames = returnNames,
                            #     interfaces = interfaces,
                                 RcppPacket_list = RcppPacket_list,
                                 dir = dir,
                                 cacheDir = cacheDir,
                                 env = env,
                                 returnList = returnList,
                                 compileInfos = compileInfos,
                                 auto_included = auto_included))
  }
}

#' @export
writePackage <- function(...,
                         pkgName,
                         dir = ".",
                         control = list(), # combines unit defaults and write/compile controls
                         unitControls = list(),
                         interfaces = "full",
                         modify = get_nOption("modifyPackageFiles"),
                         memberData = list(),
                         roxygen = list()) {
  # cat("starting writePackage\n")
  require(Rcpp)
  pkgDir <- file.path(dir, pkgName)
  modify <- match.arg(modify, c("no", "add", "clear"))
  initializePkg <- FALSE
  if(modify == "clear") { # Always initialize
    initializePkg <- TRUE
  } else {
    if (dir.exists(pkgDir)) {
      if (modify == "no") stop(paste0("Package ", pkgName, " already exists in directory ", dir,
                                      ". Change 'modify' argument 'add' (to add to it) or 'clear' ",
                                      " (to erase it before writing). Use erasePackage to erase it as a separate step."))
    } else {
      initializePkg <- TRUE # Initialize is modify != "clear" but no package exists yet
    }
  }
  if(grepl("_", pkgName))
    stop("Package names are not allowed to have underscore characters.")

  content <- control$prepared_content
  if(is.null(content)) { # this means we are being called directly, not from nCompile
    # We call nCompile with control$.writePackage containing arguments needed for
    #  nCompile to recurse back to writePackage with control$prepared_content filled in.
    # cat("calling nCompile\n")
    res <- nCompile(...,
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
              interfaces = interfaces,
              returnList = TRUE # will not be used
              )
    # nCompile will call writePackage again, this time with control$prepared_content
    # note that nCompile may be called by the user with package=TRUE, which will then
    # also call writePackage with control$prepared_content
    return(res)
  }
  units <- content$units
  unitTypes <- content$unitTypes
  cpp_names <- content$cpp_names
  interfaces <- content$interfaces
  createFromR <- content$createFromR
  cppDefs <- content$cppDefs
  exportNames <- content$exportNames
  returnNames <- content$returnNames
  packageNames <- content$packageNames
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
  # pkgDir <- file.path(dir, pkgName)
  Rdir <- file.path(pkgDir, "R")
  srcDir <- file.path(pkgDir, "src")
  instDir <- file.path(pkgDir, "inst")
  codeDir <- file.path(instDir, "include", "nCompGeneratedCode")
  datDir <- file.path(pkgDir, "data")
#
  # cat("starting writePackage writing steps\n")
  WP_check_unit_types(units, unitTypes)
  cppDefs <- WP_add_roxygen_fxns_to_cppDefs(cppDefs, units, unitTypes, roxygen, roxygenFlag)
  CnCgenerator_codes <- WP_build_CnCgenerators(units, unitTypes, interfaces, exportNames,
                                                returnNames, packageNames)
  methodFns <- WP_build_methodFns(units, unitTypes, interfaces)
  #
  if (initializePkg)
    WP_initializePkg(pkgName, dir, pkgDir, instDir, datDir, codeDir, modify)
#
  # Rfilepath <- character(length(units))
  #
  RcppPacket_list <- cppDefsList_2_RcppPacketList(cppDefs)
  ## In an earlier version, we also wrote the code to inst/include for purposes of linking to
  ## a generated package from another package. That is currently disabled and we should investigate
  ## Rcpp's Rcpp::interfaces attribute for C++ that provides some automated inst/include
  ## generation of R interface functions.
  WP_writeCpp(RcppPacket_list, srcDir, codeDir)
  WP_writeRinterfaces(units, unitTypes, interfaces, returnNames, packageNames,
                      Rdir, CnCgenerator_codes, methodFns, roxygen, roxygenFlag)
  WP_writeMemberData(memberData, datDir)
  WP_write_dotOnLoad(units, unitTypes, interfaces, exportNames, returnNames, packageNames,
                      createFromR, Rdir)
  WP_write_DESCRIPTION_NAMESPACE(units, unitTypes, interfaces, createFromR, returnNames,
                                 initializePkg, pkgDir, pkgName)
  ## if (!initializePkg) {
  ##   compiledObjs <- list.files(srcDir, pattern = "o$")
  ##   # message("Deleting ", compiledObjs)
  ##   unlink(compiledObjs)
  ## }
  # cat("calling compileAttributs\n")
  compileAttributes(pkgdir = pkgDir)
  update_compileAttributes_for_argPassing(Rdir,
                                          units, unitTypes, returnNames)
  # cat("finished writePackage\n")
  invisible(NULL)
}

update_compileAttributes_for_argPassing <- function(Rdir,
                                                    units, unitTypes, returnNames) {
  if(!any(unitTypes == "nF")) return();
  RcppExports_file <- file.path(Rdir, "RcppExports.R")
  parsed_RcppExports <- parse(RcppExports_file)
  lines_RcppExports <- readLines(RcppExports_file)
  nLines <- length(lines_RcppExports)
  names_RcppExports <- lapply(parsed_RcppExports,
                              function(x) {
                                if(x[[1]] == "<-") as.character(x[[2]]) else NULL
                              })
  any_updates <- FALSE
  for(iUnit in seq_along(units)) {
    if(unitTypes[iUnit] == "nF") {
      refArgs <- NFinternals(units[[iUnit]])$refArgs
      blockRefArgs <- NFinternals(units[[iUnit]])$blockRefArgs
      if(any(unlist(refArgs)) || any(unlist(blockRefArgs))) {
        iExpr <- which(names_RcppExports == returnNames[iUnit])
        this_name <- returnNames[iUnit]
        if(length(iExpr)!=1) stop("Problem updating arg passing handling for ", this_name)
        local_env <- new.env()
        eval(parsed_RcppExports[[iExpr]], envir = local_env)
        updatedExpr <-  passByReferenceIntoC(local_env[[ this_name ]],
                                             refArgs = refArgs,
                                             blockRefArgs = blockRefArgs)
        updatedText <- deparse(updatedExpr)
        updatedText[1] <- paste0(this_name, " <- ", updatedText[1])
        replace_begin_linenum <- grep(paste0("^",this_name, " <- function\\("), lines_RcppExports)
        replace_end_linenum <- replace_begin_linenum
        if(replace_begin_linenum < nLines) {
          replace_end_linenum <- grep("^}$", lines_RcppExports[(replace_begin_linenum+1):nLines])[1] + replace_begin_linenum
        }
        lines_before <- character()
        if(replace_begin_linenum > 1)
          lines_before <- lines_RcppExports[1:(replace_begin_linenum-1)]
        lines_after <- character()
        if(replace_end_linenum < nLines)
          lines_after <- lines_RcppExports[(replace_end_linenum+1):nLines]
        lines_RcppExports <- c(lines_before, updatedText, lines_after)
        nLines <- length(lines_RcppExports)
        any_updates <- TRUE
      }
    }
  }
  if(any_updates) {
    comment_linenums <- grep("^# Gen", lines_RcppExports)
    updated_lines <- c(lines_RcppExports[comment_linenums],
                       "# Modified by nCompiler to handle ref or blockRef argument passing",
                       lines_RcppExports[-comment_linenums])
    writeLines(updated_lines, con = RcppExports_file)
  }
  any_updates
}

nCompile_finish_nonpackage <- function(units,
                                       cppDefs = cppDefs,
                                       unitTypes,
                                       cpp_names,
                                  #     exportNames,
                                       returnNames,
                                  #     interfaces,
                                       RcppPacket_list,
                                       dir,
                                       cacheDir,
                                       env,
                                       returnList,
                                       compileInfos,
                                       auto_included = rep(FALSE, length(units))
                                       ) {
  cppfile <- paste0(cppFileLabelFunction(),".cpp") ## "nCompiler_multiple_units.cpp"
  resultEnv <- new.env()
  compiledFuns <- cpp_nCompiler(RcppPacket_list,
                                cppfile = cppfile,
                                dir = dir,
                                cacheDir = cacheDir,
                                env = resultEnv,
                                packetList = TRUE,
                                returnList = TRUE)
  if(!(length(units)==length(compileInfos)))
    stop("Problem while post-processing nCompile results.")
  methodFns <- vector(mode="list", length = length(units) ) # ditto
  createFromR_funs <- vector(mode="list", length = length(units) )
  exportNames <- unlist(lapply(compileInfos, function(x) x$exportName))
  expect_nC_interface <- rep(FALSE, length(units))
  expect_createFromR <- rep(FALSE, length(units))
  interfaceTypes <- compileInfos |> lapply(\(x) if(is.null(x$interface)) NA else x$interface ) |> unlist() # can replace some code below using this
  for(i in seq_along(units)) {
    iRes <- which( exportNames[i] == names(compiledFuns))
    if(unitTypes[i] == "nCgen") { #unit_is_nClass[i]) {
      expect_nC_interface[i] <- isTRUE(interfaceTypes[i] %in% c("full", "generic"))
      expect_createFromR[i] <- !isFALSE(compileInfos[[i]]$createFromR) &&
        expect_nC_interface[i] ## Currently one can't create objects without interface support
      if(expect_nC_interface[i]) {
        #createFromR_fun <- NULL
        if((length(iRes) != 1) && expect_createFromR[i]) {
          warning(paste0("Post-processing in nCompile: Name matching of results had a problem for nClass ", exportNames[i], "."))
        } else {
          if(expect_createFromR[i]) createFromR_funs[[i]] <- compiledFuns[[iRes]]
          methodFns[[i]] <- try(build_generic_fns_for_compiled_nClass(units[[i]]))
          if(inherits(methodFns[[i]], "try-error")) {
            warning(paste0("There was a problem building functions for generic nClass interface for ", exportNames[i], "."))
            methodFns[[i]] <- NULL
          }
        }
      }
    } else if(unitTypes[i]=="nF") {
        if(length(iRes) != 1) {
          warning(paste0("Post-processing in nCompile: Name matching of results had a problem for nFunction ", exportNames[i], "."))
        } else {
          refArgs <- cppDefs[[i]]$NF_Compiler$NFinternals$refArgs # alt: NFinternals(units[[i]])$refArgs
          blockRefArgs <- cppDefs[[i]]$NF_Compiler$NFinternals$blockRefArgs # ditto
          compiledFuns[[iRes]] <- passByReferenceIntoC(compiledFuns[[iRes]],
                                                        refArgs = refArgs,
                                                        blockRefArgs = blockRefArgs)
      }
    }
  }
  CnCgenerators <- build_compiled_nClasses(units,
                                          unitTypes,
                                          interfaceTypes,
                                          exportNames,
                                          returnNames,
                                          createFromR_funs,
                                          package=FALSE)
  names(CnCgenerators) <- returnNames

  if(any(unitTypes == "nCgen")) {
    newDLLenv <- make_DLLenv()
    # The next call does NOT rely on alignment of compiledFuns and the other inputs.
    # The other inputs are used to pick out and move subsets of compiledFuns.
    compiledFuns <- setup_nClass_environments(compiledFuns,
                                              newDLLenv,
                                              exportNames = exportNames[expect_nC_interface],
                                              NCgenerators = CnCgenerators[expect_nC_interface],
                                              methodFns = methodFns[expect_nC_interface],
                                              interfaceTypes = interfaceTypes[expect_nC_interface],
                                              returnList = TRUE)
  }

  ## Next we re-order results using input names,
  ## in case the ordering in the C++ code or in Rcpp's handling
  ## does not match order of units.
  ## cpp_names should be 1-to-1 with names(ans), with the exception of nF's that are not exported to R via RcppExport
  ## We want to return with names(ans) changed to
  ## names(units), in the order corresponding to cpp_names, but skipping non-exported nF's.
  ##
  ## At the last step, we also exclude returning auto_included entries, and we must track that through any reordering
  unit_is_nF_noExport <- unitTypes=="nF_noExport"
  ans <- vector(mode="list", length = length(units))
  ans_names <- character(length = length(units))
  ans_auto_included <- logical(length = length(units))
  for(i in seq_along(units)) {
    iRes <- -1 # will not get used. in cases where it is not replaced next, it is not used.
    if(unitTypes[i] == "nF") {
      iRes <- which(exportNames[i] == names(compiledFuns)) # iRes is index in compiledFuns of the i-th unit
    } else if(unitTypes[i] == "nCgen") {
      if(expect_createFromR[i])
        iRes <- which( exportNames[i] == names(compiledFuns))
    }
    if(length(iRes) != 1) {
      warning(paste0("Collecting results: Name matching of results had a problem for ", names(units)[i], ".\n",
                     "  Returning list of compiled results with internal C++ names."))
      return(compiledFuns)
    }
    ## In the case of nCgen and !expect_createFromR[i], ans[[i]] will remain NULL
    ## and below ans[[i]] will remain NULL, which is correct.
    ## The next two lines are a bit silly in that we are simply copying two vectors
    ## element by element. What they demonstrate is that the ans list is being returned
    ## in the same order as input, even if that requires rearrangement from compiledFuns.
    ## When returning an R6interface, that is being picked out by name.
    ans_names[i] <- returnNames[i]
    ans_auto_included[i] <- auto_included[i]

    if(unitTypes[i] == "nF") {
      ans[[i]] <- compiledFuns[[iRes]]
    } else if(unitTypes[i] == "nCgen") {
      interfaceType <- compileInfos[[i]]$interface
      if(is.null(interfaceType))
        interfaceType <- "full"
      if(interfaceType == "full") {
        if(expect_createFromR[i])
          ans[[i]] <- CnCgenerators[[returnNames[i] ]]
      } else if(interfaceType == "generic") {
        if(expect_createFromR[i])
          ans[[i]] <- compiledFuns[[iRes]]
        }
    }
    # ans[[i]] will be left NULL for nF_noExport or nClass with interface "none".
    # This is good as the result will always align with
    # input and be fully named.
  }
  names(ans) <- ans_names

  # Remove results that were auto_included.
  # Arguably this could be done earlier and save work.
  # It is done here for two reasons: being added to the code later,
  #   and potential cleanness in being able to turn it off or modify later.
  if(any(ans_auto_included)) {
    ans <- ans[!ans_auto_included]
  }

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
    if((unitTypes[i] != "nF") && (unitTypes[i] != "nCgen") && (unitTypes[i] != "nF_noExport"))
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

WP_build_CnCgenerators <- function(units, unitTypes, interfaces, exportNames,
                                      returnNames, packageNames) {
  CnCgenerator_codes <- build_compiled_nClasses(units = units,
                                          unitTypes = unitTypes,
                                          interfaces = interfaces,
                                          exportNames = exportNames,
                                          returnNames = returnNames,
                                          newCobjFuns = exportNames,
                                          package=TRUE,
                                          packageNames = packageNames)
  CnCgenerator_codes
}

WP_build_methodFns <- function(units, unitTypes, interfaces) {
  methodFns <- vector("list", length = length(units))
  for(i in seq_along(units)) {
    if(unitTypes[i]=="nCgen") {
      if(isTRUE(interfaces[[i]] %in% c("full", "generic"))) {
        methodFns[[i]] <- build_generic_fns_for_compiled_nClass(units[[i]])
      }
    }
  }
  methodFns
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
  dir.create(dir, showWarnings=FALSE, recursive=TRUE)
  erasePackage(pkgName, dir = dir, quiet = TRUE, error = FALSE)
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
  dir.create(instDir, recursive=TRUE)
  dir.create(datDir, recursive=TRUE)
  dir.create(codeDir, recursive = TRUE)
}

WP_writeCpp <- function(RcppPacket_list, srcDir, codeDir) {
  for (i in seq_along(RcppPacket_list)) {
    ## We write the code once for the package's DLL...
    writeCpp_nCompiler(RcppPacket_list[[i]],
                                    dir = srcDir)
    ## ... and again for other packages that need to
    ## compile against this package's source code.
    ## Otherwise, C++ source code is not present in an installed package.
    ## Compiling against source code is necessary because of
    ## heavy use of C++ templates.
    writeCpp_nCompiler(RcppPacket_list[[i]],
                                   dir = codeDir)
  }
}

WP_writeRinterfaces <- function(units, unitTypes, interfaces, returnNames, packageNames,
                                Rdir, CnCgenerator_codes, methodFns, roxygen, roxygenFlag) {
  Rfilepath <- vector("character", length(units))
  for (i in seq_along(units)) {
    if ((unitTypes[i]=='nCgen') && isTRUE(interfaces[[i]]!="none")) {
      ## Write the nClass full interface to the package's R directory
      generator_name <- packageNames[[i]]["uncompiled"] # must be unchanged in case inherited
      NCI <- NCinternals(units[[i]])
      compiled_generator_name <- packageNames[[i]]["compiled"]
      # if(interfaces[[i]] == "full")
      #   generator_name <- returnNames[i] # note this may differ from the name in input list
      # else
      #   generator_name <- paste0(".", returnNames[i], "_CnCgenerator")
      Rfile <- paste0(returnNames[i], '.R')
      Rfilepath[i] <- file.path(Rdir, Rfile)
      con <- file(Rfilepath[i], open = 'w')
      on.exit(close(con))
      if(is.null(CnCgenerator_codes[[i]])) { # likely a problem, but we'll try to proceed
        warning("In writePackage: R6 interface code for ", returnNames[i], " missing.")
        deparsed_CnCgenerator <- "NULL"
      } else {
        deparsed_CnCgenerator <- CnCgenerator_codes[[i]]$CncGen_code |> deparse()
        deparsed_CnCgenerator[1] <- paste0(
          compiled_generator_name, ' <- ', deparsed_CnCgenerator[1]
        )
        deparsed_Cpub_comp <- CnCgenerator_codes[[i]]$Cpub_comp_code |> deparse()
        deparsed_Cpub_comp[1] <- paste0(
          compiled_generator_name, '_CpubGen <- ', deparsed_Cpub_comp[1]
        )

        inherit_obj <- units[[i]] # NCgenerator from which the CncGen_code will inherit
        NCI_inherit <- NCinternals(inherit_obj)

        # deparsed_main_class <- NCI_inherit$main_class_code |> deparse()
        RpublicMethodNames <- NCI_inherit$RpublicNames[
          NCI_inherit$RpublicNames %in% names(units[[i]]$public_methods)]
        RpublicFieldNames <- NCI_inherit$RpublicNames[
          NCI_inherit$RpublicNames %in% names(units[[i]]$public_fields)]
        #N.B: Cpublic fields are build from NCI_inherit$fieldNames.
        deparsed_main_class <- make_nClass_code(
          internals = NCI_inherit,
          Cpublic = units[[i]]$public_methods[NCI_inherit$methodNames],
          Rpublic = c(units[[i]]$public_methods[RpublicMethodNames],
                      units[[i]]$public_fields[RpublicFieldNames])
        ) |> deparse()
        deparsed_main_class[1] <- paste0(
          generator_name, ' <- ', deparsed_main_class[1]
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
                             deparsed_CnCgenerator, fixed = TRUE)
            targetStr <- deparsed_CnCgenerator[thisDefn]
            deparsed_CnCgenerator[thisDefn] <-
              gsub(pattern = names(thisRox$methods)[m],
                   replacement = paste0(
                     "\n", thisRox$methods[m], "\n", names(thisRox$methods)[m]
                   ),
                   x = deparsed_CnCgenerator[thisDefn],
                   fixed = TRUE)
          }
        }
        all_class_output <- c(
          '## Generated by nCompiler::writePackage() -> do not edit by hand\n',
          deparsed_main_class,
          deparsed_Cpub_comp,
          if (is.list(thisRox)) thisRox[["header"]] else thisRox,
          exportTag,
          deparsed_CnCgenerator
        )
        # .onLoad should call
        # connect_nClass(envs, <CnCgenerator name>, <Cpub_gen_name>, <package env>)
      }
      writeLines(all_class_output, con)
      ##
      methodFns_name <- paste0(".", returnNames[i], "_methodFns")
      deparsed_methodFns <- deparse(methodFns[[i]])
      deparsed_methodFns[1] <- paste0(
        methodFns_name, ' <- ', deparsed_methodFns[1]
      )
      writeLines(deparsed_methodFns, con)
      on.exit()
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

WP_write_dotOnLoad <- function(units, unitTypes, interfaces, exportNames, returnNames, packageNames,
                              createFromR, Rdir) {
  expect_nC_interface <- unitTypes == "nCgen" & interfaces %in% c("full", "generic")
  if(!any(expect_nC_interface)) return()
  exportNames <- exportNames[expect_nC_interface]
  returnNames <- returnNames[expect_nC_interface]
  interfaces <- interfaces[expect_nC_interface]
  createFromR <- createFromR[expect_nC_interface]
  packageNames <- packageNames[expect_nC_interface]
  #  nClass_names <- unlist(lapply(objs, function(x)
  #    if(isNCgenerator(x)) x$classname else NULL
  #    ))
  # Get these differently than expect_nC_interface because
  # we need these even if interface=="none" (but not if createFromR is FALSE), so
  # we check if there is a valid compiled generator name.
  CnCgeneratorNames <- packageNames |> lapply(\(x) x["compiled"])
  keep <- (!is.na(CnCgeneratorNames)) & (!CnCgeneratorNames == "")
  #units |> lapply(function(x)
  #  if(isNCgenerator(x)) NCinternals(x)$compileInfo$classname else NULL) |> unlist()
  # CnCgeneratorNames <- ifelse(interfaces == "full",
  #                            returnNames,
  #                            ifelse(interfaces == "generic",
  #                                   paste0(".",returnNames,"_CnCgenerator"),
  #                                    NA))

  CnCgeneratorNames <- CnCgeneratorNames[keep]
  Cpub_generator_names <- paste0(CnCgeneratorNames, '_CpubGen')
  # Cpub_generator_names <- ifelse(interfaces == "full",
  #                               paste0(returnNames, '_CpubGen'),
  #                               ifelse(interfaces == "generic",
  #                                      paste0(".", returnNames, "_CnCgenerator_CpubGen"),
  #                                      NA))
  methodFnsNames <- paste0(".", returnNames, "_methodFns")
  paste0cq <- function(names) {
    paste0("c(", paste0("\"",names,"\"", collapse = ", "), ")")
  }
  .NCgenerator_names <- packageNames |> lapply(\(x) x["uncompiled"])
  .NCgenerator_names <- .NCgenerator_names[keep]
  # .NCgenerator_names <- ifelse(interfaces == "full",
  #                             paste0(returnNames, "_uncompiled"),
  #                             ifelse(interfaces == "generic",
  #                                    paste0(".", returnNames, "_CnCgenerator_uncompiled"),
  #                                    NA))
  onLoad_lines <- c(".onLoad <- function(libname, pkgname) {\n",
                    paste0("nCompiler::connect_nClass_envs(", CnCgeneratorNames, ",", Cpub_generator_names,
                                      ", env = asNamespace(pkgname), .NCgenerator = ", .NCgenerator_names, ")") |>
                                      paste(collapse = "\n"), #replace parent.frame() with the package env?
                    paste0(" nCompiler::setup_nClass_environments_from_package(\n",
                           "  nClass_exportNames = ", paste0cq(exportNames), ",\n",
                           "  interfaceTypes = ", paste0cq(interfaces), ",\n",
                           "  createFromR = c(", paste0(createFromR, collapse=","), "),\n",
                           "  CnCgenerators = list(", paste0(CnCgeneratorNames, collapse=","), "),\n",
                           "  methodFns = list(", paste0(methodFnsNames, collapse=","), "))\n"),
                    "NULL}\n")
  writeLines(onLoad_lines, con = file.path(Rdir, "zzz.R"))
}

WP_write_DESCRIPTION_NAMESPACE <- function(units, unitTypes, interfaces, createFromR, returnNames,
                                           initializePkg, pkgDir, pkgName) {
  DESCfile <- file.path(pkgDir, "DESCRIPTION")
  NAMEfile <- file.path(pkgDir, "NAMESPACE")
  if (initializePkg) {
    DESCRIPTION <- read.dcf(DESCfile)
    ## TO-DO: Make choice of what to include be smart about what is really needed.
    ## A nFunction might only need:
    ## DESCRIPTION[1, "LinkingTo"] <- paste(DESCRIPTION[1, "LinkingTo"], "RcppEigen", "RcppParallel", "nCompiler", sep = ",")
    ## A nClass might need:
    DESCRIPTION[1, "LinkingTo"] <- paste(DESCRIPTION[1, "LinkingTo"], "nCompiler", "RcppEigen",
                                         #"RcppEigenAD",
                                         "RcppParallel", "Rcereal", sep = ",")
    # On Linux RcppParallel might need to be in both LinkingTo and Imports.
    # Having it in Imports allows the symbols to be found when the on-the-fly package is loaded.
    # DESCRIPTION[1, "Imports"] <- paste(DESCRIPTION[1, "Imports"], "RcppParallel", sep = ",")
    # DESCRIPTION$Encoding <- "UTF-8"
    ## It is conceivable that nCompLocal will need to be added to this at some point.
    ## If so, it will need to be installed in R's main library, not some local location.
    # DESCRIPTION[1, "Collate"] <- paste(Rfilepath, collapse = ", ")
    write.dcf(DESCRIPTION, DESCfile)
    NAMESPACE <- c(paste0("useDynLib(", pkgName, ", .registration=TRUE)"),
                   "importFrom(Rcpp, evalCpp)"# , # required at package loading
#                   "export(nComp_serialize_)",
#                   "export(nComp_deserialize_)",
#                   "export(call_method)",
#                   "export(get_value)",
#                   "export(set_value)"
                   )
  } else {
    NAMESPACE <- readLines(NAMEfile)
  }
  need_export <- interfaces %in% c("generic", "full") & createFromR
  if(sum(need_export)) {
    new_exports <- paste0("export(",returnNames[need_export],")")
    needed <- !(new_exports %in% NAMESPACE)
    NAMESPACE <- c(NAMESPACE, new_exports[needed])
  }
  NAMESPACE <- unique(NAMESPACE) # double checking
  writeLines(NAMESPACE, con = NAMEfile)
}
