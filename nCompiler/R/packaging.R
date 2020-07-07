#' @name writePackage
#' @export
#' @param ... One or more nClass constructor and nFunction objects to be
#'   compiled into the package.
#' @param package.name Character string. The name of the package to be written.
#' @param dir Character string. Path to the parent directory in which the main
#'   package directory will be created. If not provided, default is the current
#'   working directory.
#' @param control A named list of control options or a named list of named lists
#'   of control parameters for each object to be compiled. See Details for more.
#' @param clean Logical, default FALSE. Should contents in the specified package
#'   directory be overwritten?
#' @param roxygen A list of roxygen entries corresponding to the objects being
#'   compiled, indicated either by matching names or (if all objects are documented)
#'   by order. The elements of the list are either character strings or lists 
#'   produced by documentNClass.
#' @param roxygenize Logical, default FALSE. Should roxygen be called to produce
#'   documentation pages before package build?
#' @param nClass_full_interface
#'
#' @details
#' 
writePackage <- function(...,
                         package.name, 
                         dir = ".",
                         control = list(),
                         clean = FALSE,
                         roxygen = list(),
                         roxygenize = FALSE,
                         nClass_full_interface = TRUE) {
  # rcpp_hello_world MUST be included because compileAttributes will not remove
  # its entry even if the file is removed.
  require(Rcpp)
  if(grepl("_", package.name))
    stop("Package names are not allowed to have underscore characters.")
  objs <- list(...)
  testname <- deparse(objs[[1]])
  
  # Handle the case where the user passes a list rather than using ...
  if (length(objs)==1) {
    if (is.list(objs[[1]])) {
      objs <- objs[[1]]
    }
  }
  
  # Handle roxygen input
  if (!is.list(roxygen)) {
    if (is.character(roxygen)) roxygen <- list(roxygen)
    else stop("in writePackage: unknown roxygen type")
  }
  
  if (length(roxygen) == 0) {
    roxygenFlag <- "none"
  } else if (sum(nchar(names(roxygen)) > 0) == length(roxygen)) { # Are all rox entries named?
    roxygenFlag <- "names"
  } else if (length(roxygen) == length(objs)) { # Are there as many rox entries as objs?
    roxygenFlag <- "indices"
  } else { # If neither, we don't know what to do about it
    stop("If fewer roxygen entries are provided than objects, they must be named",
         " in the input list to indicate the objects to which they correspond.")
  }
  
  # Check if control is specified properly.
  # The user has the following options:
  # 1) Provide nothing. Defaults are used for all NF/NC objects.
  # 2) Provide a single unnamed list. This control list will be shared by every object.
  # 3a) Provide 1 or more named lists. These lists are used for the objects whose
  #    names they match, while all others get defaults.
  # 3b) Provided 2 or more named lists, one of which is called "default." 
  #     "default" is used to control all elements.
  
  if (!is.list(control))
    stop("In writePackage, argument 'control' must be a list with controls for all
    or else a list of named lists with controls for each object")
  if (length(control) > 0 && !is.list(control[[1]])) control <- list(control)
  
  # shareControl is a boolean flag indicating if all objects will take the same
  # control variable. If so, it'll be stored in the object globalControl for now
  sharedControl <- FALSE
  globalControl <- defaultControl <- get_nOption("packagingOptions")
  if (length(control) == 0) { # We're in option 1.
    sharedControl <- TRUE
  } else if (sum(nchar(names(control)) > 0) < length(control)) { # If any elements are unnamed...
    if (length(control) == 1) { # We're in option 2
      sharedControl <- TRUE
      globalControl <- updateDefaults(defaultControl, control[[1]])
    } else { # If no names were provided but control was length >1, that's a problem
      stop(paste("More than one control list detected, but not all were named.",
                 "Control lists can only be unnamed if exactly one is provided."))
    }
  }

  # Build a fully fleshed out control list. The ith element of totalControl is a 
  # controls compilation options for the ith element of objs.
  totalControl <- list()
  
  # I retrieve the names of each object. Is there a better way to do this?
  objNames <- unlist(lapply(objs, function(x) {
    if (isNF(x)) return(NFinternals(x)$uniqueName)
    else if (isNCgenerator(x)) return(x$classname)
    else stop(paste("In writePackage: only nFunctions and nClass generators are",
                    "allowed. Cannot compile object of class ", class(x)))}))
  
  # If options 1 or 2 were hit, we can just use the globalControl option set we
  # already built for every element. If neither, we still do this to set up the
  # architecture and think of user specifications as modifying this default
  # structure
  if (sharedControl) {
    for (i in 1:length(objs)) totalControl[[i]] <- globalControl
  } else { # Option 3

    if ("default" %in% names(control)) {
      globalControl <- updateDefaults(defaultControl, controls[["default"]])
    } 
    # If no defaults were specified, globalControl is still all defaults
    # for (i in 1:length(objs)) totalControl[[i]] <- globalControl
    
    if (length(unique(objNames)) < length(objNames)) {
      stop("In writePackage: multiple objects with the same name were provided.")
    }
    
    # Iterate over each specified control list and add it. Throw an error (maybe
    # a warning, but this seems like a big enough problem) if the user provided
    # an unrecognized name.
    for (i in 1:length(control)) {
      if (names(control)[[i]] %in% objNames) {
        totalControl[[which(objNames == names(control)[[i]])]] <-
          updateDefaults(globalControl, control[[i]])
      } else {
        if (!identical(names(control)[[i]], "default")) {
          stop(paste0('In writePackage: Control specified for object named "', 
                      names(control)[[i]],
                      '"\n\t but no object with that name was provided.'))
        }
      }
    }
  } # Now we have a control object, totalControl, which contains all control
    # options for every element in objs.
  # Used to test if this works: return(totalControl)
    
  # if(length(objs) > 1)
  #   stop("writePackage only supports one object as a first step of development")
  
  pkgDir <- file.path(dir, package.name)
  Rdir <- file.path(pkgDir, "R")
  srcDir <- file.path(pkgDir, "src")
  instDir <- file.path(pkgDir, "inst")
  codeDir <- file.path(instDir, "include", "nCompGeneratedCode")

  full_interface <- list(); RcppPackets <- list()
  for (i in 1:length(objs)) {
    if(isNF(objs[[i]])) {
      nCompile_nFunction(objs[[i]],
                            control = list(endStage = "writeCpp"))
      RcppPackets[[i]] <- NFinternals(objs[[i]])$RcppPacket
    } else if(isNCgenerator(objs[[i]])) {
      writtenNC1 <- nCompile_nClass(objs[[i]],
                                    control = list(endStage = "writeCpp"))
      RcppPackets[[i]] <- NCinternals(objs[[i]])$RcppPacket
      if (isTRUE(nClass_full_interface)) {
        full_interface[[i]] <- build_compiled_nClass(objs[[i]], quoted = TRUE)
      }
    } else {
      stop(paste("In writePackage: only nFunctions and nClass generators are",
                 "allowed. Cannot compile object of class ", class(objs[[i]])))
    }
  }
  if(dir.exists(pkgDir)) {
    if(!clean)
      stop(paste0("Package ", package.name, " already exists in directory ", dir ))
    else
      unlink(pkgDir, recursive = TRUE)
  }
  ## The following eval(substitute(...), ...) construction is necessary because
  ## Rcpp.package.skeleton (and also pkgKitten::kitten) has a bug when used
  ## directly as needed here from inside a function.
  
  nCompiler_placeholder <<- function() NULL
  eval(
    substitute(
      Rcpp.package.skeleton(PN,
                        path = DIR,
                        # list = "nCompiler_placeholder",
                        author = "This package was generated by the nCompiler"),
      list(DIR = dir,
           PN = package.name)),
    envir = .GlobalEnv)
  if(file.exists(file.path(pkgDir, "Read-and-delete-me")))
    file.remove(file.path(pkgDir, "Read-and-delete-me"))
  dir.create(instDir)
  dir.create(codeDir, recursive = TRUE)
  Rfilepath <- character(length(objs))
  
  # Loop over each object again
  for (i in 1:length(objs)) {
    # Retrieve entry
    thisRox <- switch(roxygenFlag,
                      none = NULL,
                      indices = roxygen[[i]],
                      names = roxygen[[ objNames[i] ]]
                      )
    
    ## We write the code once for the package's DLL...
    nCompiler:::writeCpp_nCompiler(RcppPackets[[i]],
                                    dir = srcDir)
    ## ... and again for other packages that need to 
    ## compile against this package's source code.
    ## Otherwise, C++ source code is not present in an installed package.
    ## Compiling against source code is necessary because of
    ## heavy use of C++ templates.
    nCompiler:::writeCpp_nCompiler(RcppPackets[[i]],
                                   dir = codeDir)
    if (isNCgenerator(objs[[i]]) && isTRUE(nClass_full_interface)) {
      ## Write the nClass full interface to the package's R directory
      generator_name <- objs[[i]]$classname
      Rfile <- paste0(generator_name, '.R')
      Rfilepath[i] <- file.path(Rdir, Rfile)
      con <- file(Rfilepath[i], open = 'w')
      deparsed_full_interface <- deparse(full_interface[[i]])
      deparsed_full_interface[1] <- paste0(
        generator_name, ' <- ', deparsed_full_interface[1]
      )
      exportTag <- if (totalControl[[i]]$export) "#' @export\n" else NULL
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
  DESCfile <- file.path(pkgDir, "DESCRIPTION")
  DESCRIPTION <- read.dcf(DESCfile)
  ## TO-DO: Make choice of what to include be smart about what is really needed.
  ## A nFunction might only need:
  ## DESCRIPTION[1, "LinkingTo"] <- paste(DESCRIPTION[1, "LinkingTo"], "RcppEigen", "RcppParallel", "nCompiler", sep = ",")
  ## A nClass might need:
  DESCRIPTION[1, "LinkingTo"] <- paste(DESCRIPTION[1, "LinkingTo"], "RcppEigen", "RcppEigenAD", "RcppParallel", "nCompiler", "Rcereal", sep = ",")
    ## It is conceivable that nCompLocal will need to be added to this at some point.
    ## If so, it will need to be installed in R's main library, not some local location.
  # DESCRIPTION[1, "Collate"] <- paste(Rfilepath, collapse = ", ")
  write.dcf(DESCRIPTION, DESCfile)
  
  NAMEfile <- file.path(pkgDir, "NAMESPACE")
  NAMESPACE <- c("useDynLib(", package.name, ", .registration=TRUE)", 
                 "importFrom(Rcpp, evalCpp)")
  for (i in 1:length(objs)) {
    # if (totalControl[[i]]$export && isNCgenerator(objs[[i]])) 
    if (totalControl[[i]]$export) {
      if (isNF(objs[[i]]) || nClass_full_interface) {
        NAMESPACE <- c(NAMESPACE, paste0("export(", objNames[i], ")"))
      }
      if (isNCgenerator(objs[[i]])) NAMESPACE <- c(NAMESPACE, paste0("export(new_", objNames[i], ")"))
    }
  }
  writeLines(NAMESPACE, con = NAMEfile)

  compileAttributes(pkgdir = pkgDir)
  
  # Rename nFunctions
  rcppExports <- readLines(file.path(pkgDir, "R/RcppExports.R"))
  rcppExports <- lapply(rcppExports, function(x) {
    if (grepl(" <- function", x) && grepl("NFID", x)) {
      x <- gsub("_NFID_[0-z]+ ", " ", x)
    } else x
  })
  writeLines(unlist(rcppExports), file.path(pkgDir, "R/RcppExports.R"))
  
  if (roxygenize) roxygen2::roxygenize(package.dir = pkgDir,
                                       roclets = c("rd"))
  
  invisible(NULL)
}

#' @name buildPackage
#' @export
#' @param package.name Character string. The name of the package to be built,
#'   corresponding to the argument of the same name in writePackages.
#' @param dir Character string. Path to the parent directory containing the main
#'   package directory. By default, the current working directory is used.
#' @param lib Character string, optional. Path to the directory where the
#'   package will be installed. See the lib.loc argument in
#'   `install.packages()`.
#' @param load Logical, default TRUE. Should the package be attached once
#'   installed?
buildPackage <- function(package.name, 
                         dir = ".",
                         lib,
                         load = TRUE){
  if(!missing(lib)) {
    if(!dir.exists(lib))
      dir.create(lib)
  }

  if(!requireLocalDLLpackage())
    stop("There was a problem building nCompLocal.")
  pkg_libs_entry <- get_nCompLocal_PGK_LIBS_entry()
  staticLibLoc <- system.file('staticLib', package = 'nCompLocal')
  Sys.setenv("PKG_CXXFLAGS"="-std=c++11 -Wno-invalid-partial-specialization")
  Sys.setenv("PKG_LIBS"=pkg_libs_entry)
  ## possible missingness of lib propagates to install.packages
  install.packages(file.path(dir, package.name),
                   lib = lib,
                   repos = NULL,
                   type = "source",
                   quiet = TRUE)
  ok <- TRUE
  if(load) {
    if(missing(lib))
      lib <- NULL
    ## require can't take lib.loc as missing.
    ## It needs NULL to invoke default behavior.
    ok <- require(package.name, lib.loc = lib, character.only = TRUE)
  }
  ok
}


#' @name documentNClass
#' @title Produce a roxygen2 documentation text block for an nClass
#' @description Builds out a text block in roxygen2 format given the inputs. The
#'   user provides the name, title, and description, as well as a named list of
#'   descriptions of fields and a named list of methods descriptions.
#'   Optionally, the function can process whitespace and perform a check against
#'   the nClass generator object, reporting whether all fields and methods were
#'   described.
#' @param obj The nClass generator being documented. Only required if
#'   checkAgainstObj is TRUE, optional and ignored otherwise.
#' @param name The nClass name. Used in the `@name` field of roxygen2
#'   documentation.
#' @param title a descriptive title for the nClass. Used in the `@title` field
#'   of roxygen2 documentation
#' @param description A text block giving a full and clear description of the
#'   nClass. For use in the `@description` field of roxyygen2 documentation. If
#'   processWhitespacpe is TRUE, any whitespace in the description will be
#'   ignored and the code block will be reformatted for style.
#' @param fields A named list of documentation for C-ready public fields
#'   belonging to the nClass. Fields are values stored internally by the class
#'   and that are publicly accessible with `obj$field`. List names should
#'   exactly match field names, while the elements of the list should be
#'   character strings describing the field.
#' @param CMethodsDescriptions A named list of descriptions for the nClass's
#'   methods. List names should exactly match method names, while the elements
#'   of the list should be character strings describing the method.
#' @param CMethodsParams A named list of named lists. The names in
#'   CMethodsParams should match exactly the names of the methods of the nClass.
#'   Each element of CMethodsParams should itself be a named list, with names
#'   matching parameters of the appropriate function, and elements being text
#'   strings describing those parameters.
#' @otherRoxygen Additional roxygen content to be tacked onto the end of the
#'   roxygen2 header. This could be `@examples`, `@seealso`, or any other valid
#'   roxygen field. You are responsible for formatting these the way you want,
#'   including nwlines, `#'` comment notation, and `@@` prefixes. Character
#'   string, optional.
#' @param processWhitespace Logical, default TRUE. Can whitespace be modified to
#'   maintain style?
#' @param checkAgainstObj Logical, default TRUE. Should the input be compared
#'   against the actual nClass generator to confirm that all elements have been
#'   documented appropriately?
#' @seealso \link[roxygen2]{roxygenize}
#'
#' @example
#' @export
# TODO: Be more thoughtful about when whitespace is and isn't addressed,
#       incl. tabs and the like
documentNClass <- function(obj = NULL, name, title, description = NULL, 
                           fields = list(),
                           CMethodsDescriptions = list(), 
                           CMethodsParams = list(),
                           otherRoxygen = NULL,
                           processWhitespace = TRUE,
                           checkAgainstObj = TRUE) {
  # Check sanity of inputs
  if (sum(nchar(names(fields)) > 0) < length(fields)) {
    stop("in documentNClass: Some elements of list 'fields' are unnamed.")
  }
  if (checkAgainstObj && is.null(obj)) {
    stop(paste("in documentNClass: checkAgainstObj is true but no nClass",
               "object was provided"))
  }
  
  if (processWhitespace) {
    nameProc <- strwrap(gsub("\n", " ", name), width = 80, 
                           prefix = "#'   ", initial = "#' @name ")
    titleProc <- strwrap(gsub("\n", " ", title), width = 80, 
                           prefix = "#'   ", initial = "#' @title ")
    descProc <- 
      if (is.null(description)) { NULL
      } else strwrap(gsub("\n", " ", description), width = 80, 
                   prefix = "#'   ", initial = "#' @description ")
    
    fieldsProc <- character(length(fields))
    if (length(fields) > 0) for (i in 1:length(fields)) {
      fieldsProc[i] <- strwrap(paste(names(fields)[i], fields[[i]]),
                               width = 80, prefix = "#'   ",
                               initial = "#' @field ")
    }
    
  } else {
    nameProc <- gsub("\n", "\n#'", paste0("#' @name ", name))
    titleProc <- gsub("\n", "\n#'", paste0("#' @title ", title))
    descProc <- gsub("\n", "\n#'", paste0("#' @description ", description))
    fieldsProc <- character(length(fields))
    if (length(fields) > 0) for (i in 1:length(fields)) {
      fieldsProc[[i]] <- gsub("\n", "\n#'", paste("#' @field", 
                                                   names(fields)[i], 
                                                   fields[[i]]))
    }
  }
  
  header <- paste0(c(nameProc, titleProc, descProc, fieldsProc, otherRoxygen), 
                   collapse = "\n")

  
  # Handle the methods documentation. 
  # Methods are allowed to have just description, just params, or both
  methodsToDocument <- unique(c(names(CMethodsDescriptions),
                                  names(CMethodsParams)))
  
  methodsList <- list()
  for (i in 1:length(methodsToDocument)) {
    thisMethod <- methodsToDocument[i]
    thisDescStr <- strwrap(gsub("\n", " ", CMethodsDescriptions[[thisMethod]]),
                           width = 80, prefix = "#'   ", 
                           initial = "#' @description ")
    thisParams <- character(length(CMethodsParams[[thisMethod]]))
    for (j in 1:length(thisParams)) {
      thisParams[j] <- strwrap(gsub("\n", " ", CMethodsParams[[i]][j]), 
                               width = 80, prefix = "#'   ", 
                               initial = paste0("#' @param ", 
                                                names(CMethodsParams[[i]])[j], 
                                                " "))
    }
    
    methodsList[[ methodsToDocument[i] ]] <-
      paste(
        c(thisDescStr, thisParams),
        collapse = "\n"
      )
  }
  
  return(list(header = header,
              methods = methodsList))
}







