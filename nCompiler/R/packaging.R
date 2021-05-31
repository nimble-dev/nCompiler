#' @name nWritePackage
#' @title Create packages containing compiled elements
#' @export
#' @param ... One or more nClass constructor and nFunction objects to be
#'   compiled into the package.
#' @param package.name Character string. The name of the package to be written.
#' @param dir Character string. Path to the parent directory in which the main
#'   package directory will be created. If not provided, default is the current
#'   working directory.
#' @param control A named list of control options or a named list of named lists
#'   of control parameters for each object to be compiled. See Details for more.
#' @param modify Logical, default FALSE. Should writing proceed even if the
#'   package exists? If TRUE, elements provided for compilation are added to the
#'   existing package. Existing package elements with the same name as new
#'   elements are overwritten, while elements without name conflicts are
#'   retained. To change the default across multiple calls, use
#'   `set_nOption("modifyPackageContents" = TRUE)`
#' @param memberData A named list of elements to be stored as exported package 
#'   data objects.
#' @param roxygen A list of roxygen entries corresponding to the objects being
#'   compiled, indicated either by matching names or (if all objects are
#'   documented) by order. The elements of the list are either character strings
#'   or lists produced by documentNClass.
#' @param roxygenize Logical, default FALSE. Should roxygen be called to produce
#'   documentation pages before package build?
#' @param nClass_full_interface
#'
#' @details
#'
#' `nWritePackage` is a function for adding compiled nFunctions and nClasses to
#' an R package. `nWritePackage` handles compilation, documentation, and
#' exporting, so that the resulting R package can call the compiled nFunctions
#' and instantiate members of the nClass without further compilation.
#'
#' The names of the elements exported in the package will match the internal
#' names of the elements as they are passed (the \code{name} field in
#' \code{nFunction} and the \code{classname} field in \code{nClass}), NOT
#' the names of the objects in the R environment.
#'
#' If a nonexistent directory or nonexistent directory is indicated by the
#' arguments \code{dir} and \code{package.name}, nWritePackage uses Rcpp's
#' Rcpp.package.skeleton to initialize a directory.
#'
#' If the directory indicated by those two arguments does exist and
#' \code{modify} is \code{TRUE}, then the elements passed to `...` are compiled
#' and added to the package. This process will overwrite existing compiled
#' objects with overlapping names, so can be used to edit and update package
#' elements as well as add new elements.
#' 
#' If an uncompilable nFunction or nClass is passed, \code{nWitePackage} will
#' not error. This will not be caught until the package is built, for example
#' with \code{buildPackage}.
#'
#' @examples
#' # Initialize an example nFunction
#' foo <- nFunction(name = "foo",
#'                  fun = function(x = numericScalar()) {
#'                      ans <- x+1
#'                      return(ans)
#'                      returnType(numericScalar())
#'                    }
#'                  )
#' # Write a package containing the compiled nFunction "foo"
#' nWritePackage(foo,
#'              dir = tempdir(),
#'              package.name = "fooPackage",
#'              control = list(export = TRUE))
#' # Build and install the package
#' buildPackage("fooPackage", dir = tempdir())
#' # We can call "foo" from the new namespace
#' fooPackage::foo(10)
#' @seealso For more nCompiler packaging tools, see \code{\link{buildPackage}}
#'   and \code{\link{erasePackage}}. For nCompiler roxygen utilities see
#'   \code{\link{documentNClass}} and \code{\link{documentNFunction}}. For
#'   package initialization tools on which \code{nWritePackage} depends see
#'   \code{\link[Rcpp]{Rcpp.package.skeleton}} and
#'   \code{\link[utils]{package.skeleton}}.
nWritePackage <- function(...,
                         package.name, 
                         dir = ".",
                         control = list(),
                         modify = get_nOption("modifyPackageContents"),
                         memberData = list(),
                         roxygen = list(),
                         nClass_full_interface = TRUE) {

  if (modify && !dir.exists(file.path(dir, package.name))) {
      warning(paste0("No package named '", package.name, 
                     "' exists in directory '", dir, 
                     "' so one will be created."))
  }
  
  require(Rcpp)
  if(grepl("_", package.name))
    stop("Package names are not allowed to have underscore characters.")
  objs <- list(...)

  # Handle the case where the user passes a list rather than using ...
  if (length(objs)==1) {
    if (is.list(objs[[1]])) {
      objs <- objs[[1]]
    }
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
    stop("In nWritePackage, argument 'control' must be a list with controls for all
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
    else stop(paste("In nWritePackage: only nFunctions and nClass generators are",
                    "allowed. Cannot compile object of class ", class(x)))}))
  if (length(unique(objNames)) < length(objNames)) stop(paste(
    "in nWritePackage: Duplicate internal object names detected."
  ))
  
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
      stop("In nWritePackage: multiple objects with the same name were provided.")
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
          stop(paste0('In nWritePackage: Control specified for object named "', 
                      names(control)[[i]],
                      '"\n\t but no object with that name was provided.'))
        }
      }
    }
  } # Now we have a control object, totalControl, which contains all control
    # options for every element in objs.
  # Used to test if this works: return(totalControl)
    
      # I thought maybe I could put methods doc in C++ as follows but I had issues
      # thisRox <- switch(roxygenFlag,
      #                   none = NULL,
      #                   indices = if(length(roxygen) < i) roxygen[[i]] else NULL,
      #                   names = if (objNames[[i]] %in% names(roxygen)) 
      #                     roxygen[[ objNames[i] ]] 
      #                   else NULL)
      # if (!is.null(thisRox)) {
      #   # givenNames <- names(objs[[i]]$public_methods)
      #   cppCodeNames <- lapply(objs[[i]]$public_methods, 
      #                          function(x) if (isNF(x)) 
      #                            NFinternals(x)$cpp_code_name)
      #   for (m in 1:length(thisRox$methods)) {
      #     thisFnDefIndex <- grep(paste0(cppCodeNames[names(thisRox$methods)[m]], " ("), 
      #                             RcppPackets[[i]]$cppContent, fixed = TRUE)
      #     
      #     RcppPackets[[i]]$cppContent <-
      #       c(RcppPackets[[i]]$cppContent[1:(thisFnDefIndex - 1)],
      #         thisRox$methods[[m]], 
      #         RcppPackets[[i]]$cppContent[(thisFnDefIndex):length(RcppPackets[[i]]$cppContent)])
      #   }
      # }

  
  # if(length(objs) > 1)
  #   stop("nWritePackage only supports one object as a first step of development")

    RcppPacket_list <- compileLoop(objs,
                                   objNames,
                                   roxygen)

  # May want to reconstitute elsewhere:
  # if (!identical(nCompiler:::Rname2CppName(objNames[[i]]), objNames[[i]])) {
  #     warning(paste0("The nFunction name ", objNames[[i]], " isn't valid for ",
  #                    "C++.\n Using the modified name ", 
  #                    Rname2CppName(objNames[[i]]), " instead."))

  pkgDir <- file.path(dir, package.name)
  initializePkg <- FALSE
  if (dir.exists(pkgDir)) {
    if (!modify) stop(paste0("Package ", package.name, " already exists in directory ", dir ))
  } else initializePkg <- TRUE

  ## The following eval(substitute(...), ...) construction is necessary because
  ## Rcpp.package.skeleton (and also pkgKitten::kitten) has a bug when used
  ## directly as needed here from inside a function.
  instDir <- file.path(pkgDir, "inst")
  codeDir <- file.path(instDir, "include", "nCompGeneratedCode")
  datDir <- file.path(pkgDir, "data")
  if (initializePkg) {
    nCompiler_placeholder <<- function() NULL
    suppressMessages(
      eval(
        substitute(
          Rcpp.package.skeleton(PN,
               path = DIR,
               # list = "nCompiler_placeholder",
               author = "This package was generated by the nCompiler"),
          list(DIR = dir,
               PN = package.name)),
        envir = .GlobalEnv)
    )
    if(file.exists(file.path(pkgDir, "Read-and-delete-me")))
      file.remove(file.path(pkgDir, "Read-and-delete-me"))
    dir.create(instDir)
    dir.create(datDir)
    dir.create(codeDir, recursive = TRUE)
  }

  roxygenFlag <- getRoxygenFlag(objs, roxygen)
  Rdir <- file.path(pkgDir, "R")
  srcDir <- file.path(pkgDir, "src")

  # Loop over each object again
  for (i in 1:length(objs)) {
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
    if (isNCgenerator(objs[[i]]) && isTRUE(nClass_full_interface)) {
      ## Write the nClass full interface to the package's R directory
      full_interface <- build_compiled_nClass(objs[[i]], quoted = TRUE)
      deparsed_full_interface <- deparse(full_interface)
      generator_name <- objs[[i]]$classname
      deparsed_full_interface[1] <- paste0(
        generator_name, ' <- ', deparsed_full_interface[1]
      )
      # Retrieve roxygen entry
      thisRox <- switch(roxygenFlag,
                        none = NULL,
                        indices = if(length(roxygen) < i) roxygen[[i]] else NULL,
                        names = if (objNames[[i]] %in% names(roxygen)) 
                                  roxygen[[ objNames[i] ]] 
                                else NULL
                        )
      
      if (!is.null(thisRox)) {
        # Find the spot where each documented method is defined
        for (m in 1:length(thisRox$methods)) {
          thisDefn <- grep(paste0(names(thisRox$methods)[m], " = function("),
                           deparsed_full_interface, fixed = TRUE)
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
        '## Generated by nCompiler::nWritePackage() -> do not edit by hand\n',
        if (is.list(thisRox)) thisRox[["header"]] else thisRox,
        if (totalControl[[i]]$export) "#' @export\n" else NULL,
        deparsed_full_interface,
        paste0(generator_name, '$parent_env <- new.env()'),
        paste0(generator_name, '$.newCobjFun <- NULL')
      )

      Rfile <- paste0(generator_name, '.R')
      Rfilepath <- file.path(Rdir, Rfile)
      con <- file(Rfilepath, open = 'w')
      writeLines(deparsed_full_interface, con)
      close(con)
    }
  }
  
  # Write out data
  if (length(memberData) > 0) {
    datEnv <- as.environment(memberData)
    for (i in 1:length(ls(datEnv))) {
      save(list = ls(datEnv)[i], envir = datEnv, 
           file = file.path(datDir, paste0(ls(datEnv)[i], ".RData")))
    }
  }
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
    
    NAMESPACE <- c(paste0("useDynLib(", package.name, ", .registration=TRUE)"), 
                   "importFrom(Rcpp, evalCpp)")
  } else {
    NAMESPACE <- readLines(NAMEfile)
  }
  
  for (i in 1:length(objs)) {
    # if (totalControl[[i]]$export && isNCgenerator(objs[[i]])) 
    if (totalControl[[i]]$export) {
      if (isNF(objs[[i]]) || nClass_full_interface) {
        NAMESPACE <- c(NAMESPACE, paste0("export(", objNames[i], ")"))
      }
      if (isNCgenerator(objs[[i]])) 
          NAMESPACE <- c(NAMESPACE, paste0("export(new_", objNames[i], ")"))
    }
  }
  NAMESPACE <- unique(NAMESPACE)
  writeLines(NAMESPACE, con = NAMEfile)

  if (!initializePkg) {
    compiledObjs <- list.files(srcDir, pattern = "o$")
    # message("Deleting ", compiledObjs)
    unlink(compiledObjs)
  }
  compileAttributes(pkgdir = pkgDir)
  
  invisible(NULL)
}


#' @name buildPackage
#' @title Build and install packages written by nWritePackage
#' @export
#' @param package.name Character string. The name of the package to be built,
#'   corresponding to the argument of the same name in nWritePackages.
#' @param dir Character string. Path to the parent directory containing the main
#'   package directory. By default, the current working directory is used.
#' @param lib Character string, optional. Path to the directory where the
#'   package will be installed. See the lib.loc argument in
#'   `install.packages()`.
#' @param load Logical, default TRUE. Should the package be attached once
#'   installed?
#'
#' @details
#'
#' \code{buildPackage} is a function for adding compiled nFunctions and nClasses
#' to an R package. It handles the building and installation. Optionally
#' buildPackage calls \code{roxygen2::roxygenize} on the home directory of the
#' package. It may not be necessary to use buildPackage if another developer
#' tool is preferred for your package.
#'
#' \code{buildPackage} errors if package installation fails. If an uncompilable
#' nFunction or nClass was written to the package, this will cause
#' \code{buildPackage} to fail, printing the message that install.packages
#' finished with "non-zero exit status."
#'
#' @examples
#' # Initialize an example nFunction
#' foo <- nFunction(name = "foo",
#'                  fun = function(x = numericScalar()) {
#'                      ans <- x+1
#'                      return(ans)
#'                      returnType(numericScalar())
#'                  })
#' # Write a package containing the compiled nFunction "foo"
#' nWritePackage(foo,
#'              dir = tempdir(),
#'              package.name = "fooPackage",
#'              control = list(export = TRUE))
#' # Build and install the package
#' buildPackage("fooPackage", dir = tempdir())
#' # We can call "foo" from the new namespace
#' fooPackage::foo(10)
#' @seealso For other nCompiler packaging tools, see \code{\link{nWritePackage}}
#'   and \code{\link{erasePackage}}. For nCompiler roxygen utilities see
#'   \code{\link{documentNClass}} and \code{\link{documentNFunction}}. For
#'   roxygen2 tools on which \code{buildPackage} depends see
#'   \code{\link[roxygen2]{roxygenize}}.
nBuildPackage <- function(package.name, 
                         dir = ".",
                         lib,
                         load = TRUE,
                         roxygenize = FALSE){
  if(!missing(lib)) {
    if(!dir.exists(lib))
      dir.create(lib)
  }

  if (roxygenize) roxygen2::roxygenize(
      package.dir = file.path(dir, package.name), 
      roclets = c("rd")
  )

  if(isTRUE(get_nOption("use_nCompLocal")))
    if(!nCompiler:::requireLocalDLLpackage())
      stop("There was a problem building nCompLocal.")
  pkg_libs_entry <- nCompiler:::get_nCompLocal_PKG_LIBS_entry()
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

#' @name erasePackage
#' @title Erase packages generated by `nCompiler`.
#' @description Erases all files associated with a package written out by the
#'   `nWritePackage()` function in nCompiler.
#' @param package.name The name of the package to be erased
#' @param dir The directory containing the package to be erased
#' @param nCompilerOnly Logical, default TRUE. If TRUE, extra checks are
#'   performed to make sure the target directory is a package generated by
#'   nCompiler. If FALSE, checks are still performed to ensure the target looks
#'   like a package.
#' @param unintall Logical, default FALSE. If TRUE, the package is uninstalled
#'   as well as erased.
#' @export
#' @details
#'
#' \code{erasePackage} is a utility function for erasing a written
#' nCompiler-generated package. It is best thought of as the inverse of
#' \code{nWritePackage}, not \code{buildPackage}, as it is intended for use with
#' source code.
#'
#' If called on a package directory, all contents of the package and the
#' directory itself will be deleted with \code{unlink}.
#'
#' By default, \code{erasePackage} checks that the directory specified looks
#' like an R package. It simply looks to see if there is a DESCRIPTION file and
#' a NAMESPACE file in the directory. If not, \code{erasePackage} errors out.
#' This prevents accidental erasure of important directories.
#'
#' If \code{nCompilerOnly} is \code{TRUE} (the default), the DESCRIPTION file
#' will be checked to make sure it looks like one autogenerated by
#' \code{nWritePackage}.
#' 
#' @examples
#' 
#' # Initialize an example nFunction
#' foo <- nFunction(name = "foo",
#'                  fun = function(x = numericScalar()) {
#'                      ans <- x+1
#'                      return(ans)
#'                      returnType(numericScalar())
#'                  })
#' # Write a package containing the compiled nFunction "foo"
#' nWritePackage(foo,
#'              dir = tempdir(),
#'              package.name = "fooPackage",
#'              control = list(export = TRUE))
#' # Decide we don't want fooPackage after all and erase it.
#' erasePackage("fooPackage", dir = tempdir())
#' dir.exists(file.path(tempdir(), "fooPackage")) # FALSE
#' 
#' @seealso For other nCompiler packaging tools, see \code{\link{nWritePackage}}
#'   and \code{\link{buildPackage}}. For more info on deleting a directory see
#'   \code{\link[base]{unlink}}.

nErasePackage <- function(package.name, dir, 
                         nCompilerOnly = TRUE, uninstall = FALSE) {
  pkgDir <- file.path(dir, package.name)
  if (!dir.exists(pkgDir)) 
    stop(paste0("Directory does not exist at the specified path: ", pkgDir))
  if (!file.exists(file.path(pkgDir, "DESCRIPTION")))
    stop("Target is not a package (no DESCRIPTION found).")
  
  if (nCompilerOnly) {
    desc <- read.dcf(file.path(pkgDir, "DESCRIPTION"))
    if (!(desc[,"Author"] == "This package was generated by the nCompiler")) {
      stop("This package was not auto-generated by nCompiler.")
    }
  }
  
  if (uninstall) {
    remove.packages(package.name)
  }
  unlink(pkgDir, recursive = TRUE)
  invisible(NULL)
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
#'   checkAgainstObj is TRUE, ignored otherwise.
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
#' @param otherRoxygen Additional roxygen content to be tacked onto the end of
#'   the roxygen2 header. This could be `@examples`, `@seealso`, or any other
#'   valid roxygen field. You are responsible for formatting these the way you
#'   want, including nwlines, `#'` comment notation, and `@@` prefixes.
#'   Character string, optional.
#' @param processWhitespace Logical, default TRUE. Can whitespace be modified to
#'   maintain style?
#' @param headerComment what string indicates the start of a roxygen comment for
#'   the header? By default nCompiler packaging puts header comments for
#'   nClasses in R, so the default value `#'` should almost always be kept.
#' @param methodsComment what string indicates the start of a roxygen comment
#'   for the methods? By default nCompiler packaging puts methods comments for
#'   nClasses in R, so the default value `#'` should almost always be kept.
#' @param checkAgainstObj Logical, default TRUE. Should the input be compared
#'   against the actual nClass generator to confirm that all elements have been
#'   documented appropriately? (NOTE: not implemented yet, currently ignored)
#' @seealso For documenting nFunctions see \link{documentNFunction}. For
#'   nCompiler packaging tools see \link{nWritePackage}. To learn about roxygen
#'   documentation format see the package roxygen, e.g.
#'   \link[roxygen2]{roxygenize}.
#'
#' @examples
#' # Initialize an example nClass
#' foo <- nClass(
#'   classname = "foo",
#'   Cpublic = list(
#'     x = "numericScalar",
#'     cp1 = nFunction(fun = function(x = numericScalar()) {
#'       x <<- x
#'       ans <- x + 1
#'       return(ans)
#'       returnType(numericScalar())
#'     }))
#' )
#' # Create the roxygen
#' rox <- documentNClass(obj = foo,
#'                       name  = "foo",
#'                       title = "A Test nClass",
#'                       description = "This nClass has a method to add 1
#'                          to a scalar input, and has a field to store
#'                          that input.",
#'                       fields = list(x = "A scalar input."),
#'                       CMethodsDescriptions = list(cp1 = "Adds 1 to x."),
#'                       CMethodsParams =
#'                         list(
#'                          cp1 = list(x = "A scalar to which 1 will be added.")
#'                         ),
#'                       methodsComment = "#'")
#' # Use the roxygen to write and build a package, which will include
#' # documentation for foo
#' nWritePackage(foo,
#'              dir = tempdir(),
#'              package.name = "fooPackageWriteDocnFunction",
#'              roxygen = list(foo = rox))
#' buildPackage("fooPackageWriteDocnFunction",
#'              dir = tempdir(),
#'              roxygenize = TRUE)
#' ?foo
#' @export

# TODO: Be more thoughtful about when whitespace is and isn't addressed,
#       incl. tabs and the like
nDocumentNClass <- function(obj = NULL, name, title, description = NULL, 
                           fields = list(),
                           CMethodsDescriptions = list(), 
                           CMethodsParams = list(),
                           otherRoxygen = NULL,
                           headerComment = "#'",
                           methodsComment = "#'",
                           processWhitespace = TRUE,
                           checkAgainstObj = FALSE) {
  # Check sanity of inputs
  if (sum(nchar(names(fields)) > 0) < length(fields)) {
    stop("in documentNClass: Some elements of list 'fields' are unnamed.")
  }
  if (checkAgainstObj && is.null(obj)) {
    stop(paste("in documentNClass: checkAgainstObj is true but no nClass",
               "object was provided"))
  }
  
  if (processWhitespace) {
    nameProc <- strwrap(gsub("[[:space:]]+", " ", name), width = 80, 
                           prefix = paste0(headerComment, "   "), 
                        initial = paste0(headerComment, " @name "))
    titleProc <- strwrap(gsub("[[:space:]]+", " ", title), width = 80, 
                           prefix = paste0(headerComment, "   "), 
                         initial = paste0(headerComment, " @title "))
    descProc <- 
      if (is.null(description)) { NULL
      } else strwrap(gsub("[[:space:]]+", " ", description), width = 80, 
                   prefix = paste0(headerComment, "   "), 
                   initial = paste0(headerComment, " @description "))
    
    fieldsProc <- character(length(fields))
    if (length(fields) > 0) for (i in 1:length(fields)) {
      fieldsProc[i] <- strwrap(paste(names(fields)[i], fields[[i]]),
                               width = 80, prefix = paste0(headerComment, "   "),
                               initial = paste0(headerComment, " @field "))
    }
    
  } else {
    nameProc <- gsub("\n", paste0("\n", headerComment), 
                     paste0(headerComment, " @name ", name))
    titleProc <- gsub("\n", paste0("\n", headerComment), 
                      paste0(headerComment, " @title ", title))
    descProc <- gsub("\n", paste0("\n", headerComment), 
                     paste0(headerComment, " @description ", description))
    fieldsProc <- character(length(fields))
    if (length(fields) > 0) for (i in 1:length(fields)) {
      fieldsProc[[i]] <- gsub("\n", paste0("\n", headerComment), 
                              paste(headerComment, "@field", names(fields)[i], 
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
    thisNameProc <- strwrap(gsub("[[:space:]]+", " ", thisMethod), width = 80, 
                            prefix = paste0(methodsComment, "   "), 
                            initial = paste0(methodsComment, " @name "))
    
    thisDescStr <- strwrap(gsub("[[:space:]]+", " ", CMethodsDescriptions[[thisMethod]]),
                           width = 80, prefix = paste0(methodsComment, "   "), 
                           initial = paste0(methodsComment, " @description "))
    thisParams <- character(length(CMethodsParams[[thisMethod]]))
    for (j in 1:length(thisParams)) {
      thisParams[j] <- strwrap(gsub("[[:space:]]+", " ", CMethodsParams[[i]][j]), 
                               width = 80, prefix = paste0(methodsComment, "   "), 
                               initial = paste0(methodsComment, " @param ", 
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


#' @name documentNFunction
#' @title Produce a roxygen2 documentation text block for an nFunction
#' @description Builds out a text block in roxygen2 format given the inputs. The
#'   user provides the name, title, and description, as well as a named list of
#'   descriptions of fields and a named list of methods descriptions.
#'   Optionally, the function can process whitespace and perform a check against
#'   the nFunction generator object, reporting whether all fields and methods
#'   were described.
#' @param obj The nFunction being documented. Only required if checkAgainstObj
#'   is TRUE, ignored otherwise.
#' @param name The nFunction name. Used in the `@name` field of roxygen2
#'   documentation.
#' @param title a descriptive title for the nClass. Used in the `@title` field
#'   of roxygen2 documentation
#' @param description A text block giving a full and clear description of the
#'   nClass. For use in the `@description` field of roxyygen2 documentation. If
#'   processWhitespacpe is TRUE, any whitespace in the description will be
#'   ignored and the code block will be reformatted for style.
#' @param params A named list. Names are the names of parameters in the
#'   nFunction. Values are descriptions of each parameter.
#' @param otherRoxygen Additional roxygen content to be tacked onto the end of
#'   the roxygen2 header. This could be `@examples`, `@seealso`, or any other
#'   valid roxygen field. You are responsible for formatting these the way you
#'   want, including nwlines, `#'` comment notation, and `@@` prefixes.
#'   Character string, optional.
#' @param processWhitespace Logical, default TRUE. Can whitespace be modified to
#'   maintain style?
#' @param roxComment what string indicates the start of a roxygen comment? By
#'   default nCompiler packaging puts comments for nFunctions in C++, so the
#'   default value `//'` should almost always be kept.
#' @param checkAgainstObj Logical, default TRUE. Should the input be compared
#'   against the actual nClass generator to confirm that all elements have been
#'   documented appropriately? (NOTE: not implemented yet, currently ignored)
#'
#' @export
#' @seealso For documenting nFunctions see \link{documentNFunction}. For
#'   nCompiler packaging tools see \link{nWritePackage}. To learn about roxygen
#'   documentation format see the package roxygen, e.g.
#'   \link[roxygen2]{roxygenize}.
#'
#' @examples
#' # Initialize an example nFunction
#' foo <- nFunction(name = "foo",
#'                  fun = function(x = numericScalar()) {
#'                      ans <- x+1
#'                      return(ans)
#'                      returnType(numericScalar())
#'                  })
#' # Create the roxygen
#' rox <- documentNFunction(obj = foo,
#'                     name  = "foo",
#'                     title = "A Test nFunction",
#'                     description = "This nFunction just adds 1 to a
#'                               scalar input.",
#'                     params = list(x = "A scalar to which 1 will be added."),
#'                     otherRoxygen = "//' @export")
#' # Use the roxygen to write and build a package, which will include
#' # documentation for foo
#' nWritePackage(foo,
#'              dir = tempdir(),
#'              package.name = "fooPackageWriteDocnFunction",
#'              roxygen = list(foo = rox))
#' buildPackage("fooPackageWriteDocnFunction",
#'              dir = tempdir(),
#'              roxygenize = TRUE)
#' ?foo

# TODO: Be more thoughtful about when whitespace is and isn't addressed,
#       incl. tabs and the like
nDocumentNFunction <- function(obj = NULL, name, title, description = NULL, 
                              params = list(), otherRoxygen = NULL, 
                              processWhitespace = TRUE, roxComment = "//'",
                              checkAgainstObj = FALSE){
  # Check sanity of inputs
  if (sum(nchar(names(params)) > 0) < length(params)) {
    stop("in nDocumentNClass: Some elements of list 'params' are unnamed.")
  }
  if (checkAgainstObj && is.null(obj)) {
    stop(paste("in nDocumentNClass: checkAgainstObj is true but no nFunction",
               "object was provided"))
  }
  
  if (processWhitespace) {
    
    nameProc <- strwrap(gsub("[[:space:]]+", " ", name), width = 80, 
                        prefix = paste0(roxComment, "   "), 
                        initial = paste0(roxComment, " @name "))
    titleProc <- strwrap(gsub("[[:space:]]+", " ", title), width = 80, 
                         prefix = paste0(roxComment, "   "), 
                         initial = paste0(roxComment, " @title "))
    descProc <- 
      if (is.null(description)) { NULL
      } else strwrap(gsub("[[:space:]]+", " ", description), width = 80, 
                     prefix = paste0(roxComment, "   "),  
                     initial = paste0(roxComment, " @description "))
    
    paramsProc <- character(length(params))
    if (length(params) > 0) for (i in 1:length(params)) {
      paramsProc[i] <- strwrap(paste(names(params)[i], params[[i]]),
                               width = 80, prefix = paste0(roxComment, "   "),
                               initial = paste0(roxComment, " @param "))
    }
    
  } else {
    nameProc <- gsub("\n", paste0("\n", roxComment), 
                     paste0(roxComment, " @name ", name))
    titleProc <- gsub("\n", paste0("\n", roxComment), 
                      paste0(roxComment, " @title ", title))
    descProc <- gsub("\n", paste0("\n", roxComment), 
                     paste0(roxComment, " @description ", description))
    fieldsProc <- character(length(params))
    if (length(params) > 0) for (i in 1:length(params)) {
      paramsProc[[i]] <- gsub("\n", paste0("\n", roxComment), 
                              paste(roxComment, "@param", 
                                    names(params)[i], params[[i]]))
    }
  }
  
  header <- paste0(c(nameProc, titleProc, descProc, paramsProc, otherRoxygen), 
                   collapse = "\n")

  
  return(list(header = header))
}

