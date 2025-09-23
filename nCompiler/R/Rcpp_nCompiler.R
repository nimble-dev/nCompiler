## This file contains tools for using Rcpp's compilation system to manage
## compilation of nCompiler-generated C++.

## Notes on how different compilation workflows use tools here.
##
## When calling nCompile with package=FALSE
##  cppDefsList --> RcppPacket_list --> cpp_nCompiler -->
##     writeCpp_nCompiler_combine --> writeCpp_nCompiler (once for .h, once for .cpp)
##
## When calling nCompile with package=TRUE, or calling writePackage
## cppDefsList --> RcppPacket_list --> WP_writeCpp --> writeCpp_nCompiler (for each RcppPacket in the list)

## Rcpp_nCompilerPacket represents information needed from cppDefs in.nCompiler
## to generate Rcpp-ready content.
## At the moment it is just a list, but we establish the abstraction
## because we suspect it will need to do more work in the future.
Rcpp_nCompilerPacket <- function(...) {
    ## In the future we may change internal representation,
    ## so code should use Rcpp_nCompilerPacket as an API for
    ## object creation.
    ## Right now contents are just converted to a list.
    list(...)
}

## Interface between.nCompiler's cppDef representation of code and
##.nCompiler's packet of content for Rcpp
## To be expanded to take a list of cppDefs
cppDefs_2_RcppPacket <- function(cppDef,
                                 filebase) {
  if(inherits(cppDef, 'cppRcppPacket'))
    return(cppDef$RcppPacket)

  name <- cppDef$name
  if(missing(filebase))
    filebase <- make_cpp_filebase(name)

  allCppDefs <- cppDef$getInternalDefs()

  prep <- function(x) x |> unlist(use.names=FALSE) |> unique()

  Hincludes <- allCppDefs |> lapply(\(x) x$getHincludes()) |> prep()
  CPPincludes <- allCppDefs |> lapply(\(x) x$getCPPincludes()) |> prep()
  Hpreamble <- allCppDefs |> lapply(\(x) x$getHpreamble()) |> prep()
  CPPpreamble <- allCppDefs |> lapply(\(x) x$getCPPpreamble()) |> prep()
  CPPusings <- allCppDefs |> lapply(\(x) x$getCPPusings()) |> prep()
  compileInfos <- allCppDefs |> lapply(\(x) x$getCompileInfo())

  ## Fields like copyFiles can be extracted directly from the compileInfos
  ## because they are never modified by the cppDef internally.
  ## (Whereas fields like Hincludes etc. may be modified by cppDef.)
  copyFiles <- compileInfos |> lapply(\(x) x$copyFiles) |> unlist(use.names=FALSE) |> unique()

  selfCPP <- paste0('"', filebase, '.cpp"')
  CPPincludes <- CPPincludes[ CPPincludes != selfCPP ]

  selfH <- paste0('"', filebase, '.h', '"')
  CPPincludes <- c(CPPincludes, selfH)

  debugCpp <- get_nOption('compilerOptions')[['debugCpp']]

  cppCode <-
    unlist(
      lapply(allCppDefs,
             function(x)
               capture.output( {
                 writeLines("")
                 writeCode(x$generate())
               }, split = debugCpp) ## for debugging to send handler output to console
             ),
      use.names = FALSE
    )

  hCode <-
    unlist(
      lapply(allCppDefs,
             function(x)
               capture.output( {
                 writeLines("")
                 writeCode(x$generate(declaration=TRUE))
               })
             ),
      use.names = FALSE
    )

  preamble <- unique(c(Hpreamble, CPPpreamble))

  # preamble is now its own return list element so #inclue's can be combind across packets

  cppContent <- collate_nCompiler_CppCode(preamble = "", #CPPpreamble,
                                          includes = CPPincludes,
                                          usings = CPPusings,
                                          code = cppCode,
                                          ifndefName = paste0('__', name, '_CPP')
                                          )

  hContent <- collate_nCompiler_CppCode(preamble = "", #Hpreamble,
                                        includes = Hincludes,
                                        code = hCode,
                                        ifndefName = paste0('__', name, '_H')
                                        )

  post_cpp_compiler <-
    do.call('c',
            lapply(allCppDefs,
                   function(x) x$get_post_cpp_compiler()))

  RcppPacket <- Rcpp_nCompilerPacket(
    preamble = preamble,
    cppContent = cppContent,
    hContent = hContent,
    filebase = filebase,
    post_cpp_compiler = post_cpp_compiler,
    copyFiles = copyFiles
  )
  # This sketches the idea that we could store packets in NCinternals and NFinternals.
  # In an earlier procesing flow of nCompile, we did this. Now it would be a bit trickier
  # so we will wait to see if it appears really useful.
  # cppDef$storeRcppPacket(RcppPacket) # This may not really be necessary
  RcppPacket
}

## This replaces the role of the cppCodeFileClass.
## It mostly just concatenates pieces.  It does add the #ifndef protection
## against redundant inclusion.
collate_nCompiler_CppCode <- function(preamble = character(),
                                 includes = character(),
                                 usings = character(),
                                 code = character(),
                                 ifndefName = character(),
                                 type = 'cpp') {
    ## open ifndef
    openifndefOut <- if(length(ifndefName) > 0) {
                         c(paste0('#ifndef ',ifndefName),
                           paste0('#define ',ifndefName))
                     } else
                         character()
    ## includes
    includesOut <- c('#ifndef R_NO_REMAP',
                     '#define R_NO_REMAP',
                     '#endif',
                     if(length(includes) > 0)
                              paste0('#include ',
                                     includes)
                     else
                         character()
                     )

    ## usings are included as is
    ## code is included as is

    ## close ifndef
    closeifndefOut <- if(length(ifndefName) > 0) {
                          '#endif'
                      } else {
                          character()
                      }

  list(opener = openifndefOut,
       body =
         c(#preamble
           includesOut,
           usings,
           code,
           closeifndefOut))
  ## c(openifndefOut,
  ##     preamble,
  ##     includesOut,
  ##     usings,
  ##     code,
  ##     closeifndefOut)
}

## We need a way to get ignore.stderr = TRUE and ignore.stdout = TRUE
## into the system() call in the middle of Rcpp::sourceCpp.  This
## suppresses lots of junk warnings from Eigen code we don't control
## and would be a pain to try to modify.
##
## We make a copy of Rcpp::sourceCpp and replace its closure
## with a new environment that has a custom version of system()
## that provides the arguments we need.
## The parent environment of that one is Rcpp's namespace,
## so that non-exported functions will be found.
##
## (Although a namespace is closed, we can still set it as a
## closure or parent environment.)
sourceCppEnv <- new.env()
parent.env(sourceCppEnv) <- environment(Rcpp::sourceCpp)
QuietSourceCpp <- Rcpp::sourceCpp
environment(QuietSourceCpp) <- sourceCppEnv
sourceCppEnv$system <- function(...) {
  sourceCpp_verbose <- isTRUE(nOptions("sourceCpp_verbose"))
  # We would want to use system2, but Rcpp::sourceCpp uses system
  system(..., ignore.stderr = !sourceCpp_verbose, ignore.stdout = !sourceCpp_verbose)
}

## Manage a call to Rcpp's sourceCpp()
sourceCpp_nCompiler <- function(file,
                                cacheDir,
                                ...) {
  if(isTRUE(get_nOption("use_nCompLocal")))
    if(!requireLocalDLLpackage()) {
      stop("Unable to load or create nCompLocal.")
    }
  ## In the future, this function can store "exported"
  ## in the nFunction somewhere if it is needed later.
  ## For now it just calls Rcpp::sourceCpp
  rcpp.warnNoExports <- getOption("rcpp.warnNoExports")
  options(rcpp.warnNoExports=FALSE)
  on.exit(options(rcpp.warnNoExports = rcpp.warnNoExports))
  if(!isTRUE(get_nOption("showCompilerOutput"))) {
    exported <- QuietSourceCpp(file = file,
                               cacheDir = cacheDir,
                               ...)
  } else {
    exported <- Rcpp::sourceCpp(file = file,
                                cacheDir = cacheDir,
                                showOutput = TRUE,
                                ...)
  }
  exported
}

## cpp_nCompiler writes .cpp and .h files and calls compileCpp_nCompiler to compile
## them and produce the interface R functions.
##
## This is the entryway function called by nCompile to provide an RcppPacket
## or list of packets and take all compilation steps from there.
##
## The actual C++ compiler call happens from sourceCpp_nCompiler, which
## calls Rcpp::sourceCpp, possibly wrapped in silence.

#' Call Rcpp's C++ compilation system for nCompiler-generated content
#'
#' @param cpp_nCompilerPacket An object containing content for .h and .cpp
#'     files.
#' @param dir Directory for writing C++ code files.
#' @param cacheDir Directory to use for Rcpp's cache.
#' @param env Environment for loading results of compilation.
#' @param ... Any additional argument provided will be passed to
#'     \code{sourceCpp_nCompiler}.
#' @return If one function is being compiled, \code{cpp_nCompiler} returns
#'     an R function that calls the compiled C++ internally is
#'     returned.  If multiple functions are being compiled,
#'     \code{cpp_nCompiler} returns a list of such functions.  If no
#'     functions are being compiled, \code{cpp_nCompiler} returns
#'     \code{NULL}.
#' @details \code{cpp_nCompiler} is not intended to be called directly.
#'     Instead functions such as \code{nCompile} (TBD),
#'     \code{nCompile_nFunction}, and \code{nCompile_nClass}
#'     should be used.
#' @export
cpp_nCompiler <- function(Rcpp_packet,
                          cppfile = NULL, # must be provided if packetList = TRUE
                      dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                      cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                      env = parent.frame(),
                      packetList = FALSE,
                      write = TRUE,
                      compile = TRUE,
                      ...) {

  if(write) {
    if(packetList) {
      writeCpp_nCompiler_combine(Rcpp_packet,
                                 dir = dir,
                                 cppfile = cppfile)
    } else {
      writeCpp_nCompiler(Rcpp_packet,
                         dir = dir)
    }
  }
  if(!compile) {
    return(Rcpp_packet)
  }
  if(isTRUE(get_nOption('pause_after_writing_files')))
    browser()
  compileCpp_nCompiler(Rcpp_packet = Rcpp_packet,
                       cppfile = cppfile,
                   dir = dir,
                   cacheDir = cacheDir,
                   env = env,
                   packetList = packetList,
                   ...)
}

# writeCpp_nCompiler_combine is a utility to combine .h and .cpp outputs
# from a list of RcppPackets
writeCpp_nCompiler_combine <- function(RcppPacket_list,
                                       cppfile,
                                       dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                                       header.dir) {
  if(missing(cppfile))
    stop("A cppfile name must be provided for the combined contents of the RcppPackets.")
  dir.create(dir, showWarnings = FALSE)
  cppfilepath <- file.path(dir, cppfile)

  combined_preamble <- RcppPacket_list |> lapply(\(x) x$preamble) |> unlist() |> unique()

  con <- file(cppfilepath, open = "w")
  for(RcppPacket in RcppPacket_list) {
    ## write all cpp contents to one file
    RcppPacket$preamble <- combined_preamble
    writeCpp_nCompiler(RcppPacket,
                       dir = dir,
                       con = con,
                       include_h = FALSE)
    ## write each h contents to its own file
    writeCpp_nCompiler(RcppPacket,
                       dir = dir,
                       con = NULL,
                       header.dir = header.dir,
                       include_cpp = FALSE)
  }
  close(con)
  invisible(RcppPacket_list)
}

# writeCpp_nCompiler is a utility to write .h and .cpp files from an Rcpp_packet
#' @export
writeCpp_nCompiler <- function(Rcpp_packet,
                           dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                           con = NULL,
                           include_h = TRUE,
                           include_cpp = TRUE,
                           header.dir
                          ) {
  if(!missing(header.dir)) {
    if(!is.null(con))
      warning("Both a header.dir and a con argument were provided.  header.dir will be ignored.")
  } else
    header.dir <- dir

  copyFiles <- Rcpp_packet$copyFiles
  if(!is.null(copyFiles)) {
    for(cf in copyFiles) {
      file.copy(cf, file.path(dir, basename(cf)), overwrite=TRUE)
    }
  }

  makeStandardFiles <- is.null(con)
  if(is.null(Rcpp_packet$preamble))
    Rcpp_packet$preamble <- ""
  if(include_cpp) {
    if(makeStandardFiles) {
      dir.create(dir, showWarnings = FALSE)
      cppfile <- paste0(Rcpp_packet$filebase,
                        ".cpp")
      cppfilepath <- file.path(dir, cppfile)
      con <- file(cppfilepath,
                  open = "w")
    }

    if(is.character(Rcpp_packet$cppContent))
      Rcpp_packet$cppContent <- list(opener = "",
                                     body = Rcpp_packet$cppContent)
    writeLines(Rcpp_packet$cppContent$opener, con)
    writeLines(Rcpp_packet$preamble, con)
    writeLines("#ifdef USES_NCOMPILER", con)
    writeLines("#include <nCompiler/nCompiler_omnibus.h>", con)
    writeLines("#endif", con)
    writeLines(Rcpp_packet$cppContent$body, con)
    if(makeStandardFiles)
      close(con)
  }
  if(include_h) {
    if(makeStandardFiles) {
      if(header.dir != dir)
        dir.create(header.dir, showWarnings = FALSE)
      hfile <- paste0(Rcpp_packet$filebase,
                      ".h")
      con <- file(file.path(header.dir,
                            hfile),
                  open = "w")
    }
    if(is.character(Rcpp_packet$hContent))
      Rcpp_packet$cppContent <- list(opener = "",
                                     body = Rcpp_packet$hContent)
    writeLines(Rcpp_packet$hContent$opener, con)
    writeLines(Rcpp_packet$preamble, con)
    writeLines("#ifdef USES_NCOMPILER", con)
    writeLines("#include <nCompiler/nCompiler_omnibus.h>", con)
    writeLines("#endif", con)
    writeLines(Rcpp_packet$hContent$body, con)
    if(makeStandardFiles)
      close(con)
  }
  invisible(Rcpp_packet)
}

# compileCpp_nCompiler calls the C++ compiler for C++ code that is
# already generated and then builds the R interface functions for any
# suitable functions in the C++. The Rcpp_packet is used for the filename
# and the post_cpp content, which has information on the argument passing
# semantics for each function.
# Compilation is done via sourceCpp_nCompiler, which uses Rcpp::sourceCpp.
#' @export
compileCpp_nCompiler <- function(Rcpp_packet,
                                 cppfile = NULL,
                                 dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                                 cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                                 env = parent.frame(),
                                 packetList = FALSE,
                                 returnList = FALSE, ## force result list even for a singleton
                                 ...) {
  if(!dir.exists(dir))
    stop(paste0("directory ", dir, " does not exist."))
  if(is.character(Rcpp_packet)) { # backward compatibility, to be deprecated perhaps
    cppfile <- Rcpp_packet
  } else {
    if(is.null(cppfile))
      cppfile <- paste0(Rcpp_packet$filebase,
                        ".cpp")
  }
  cppfilepath <- file.path(dir, cppfile)

  exported <- sourceCpp_nCompiler(file = cppfilepath,
                                  cacheDir = cacheDir,
                                  env = env,
                                  ...)

  ## if(packetList)
  ##   post_cpp_compiler <- do.call('c', lapply(Rcpp_packet, `[[`, 'post_cpp_compiler'))
  ## else
  ##   post_cpp_compiler <- Rcpp_packet$post_cpp_compiler
  ## Next lines copy/imitate Rcpp::cppFunction,
  ## for now without error checking until we see
  ## final uses.
  numFunctions <- length(exported$functions)
  if(numFunctions == 0) return(invisible(NULL))
  if(numFunctions == 1 & !returnList) {
    fname <- exported$functions[[1]]
    ans <- get(fname, env)
    ## this_post <- post_cpp_compiler[[fname]]
    ## if(!is.null(this_post))
    ##   ans <- passByReferenceIntoC(ans,
    ##                               refArgs = this_post[['refArgs']],
    ##                               blockRefArgs = this_post[['blockRefArgs']])
    return(ans)
  }
  ans <- structure(
    lapply(exported$functions,
           function(x) get(x, env)),
    names = exported$functions)
  ## for(fname in names(ans)) {
  ##   this_post <- post_cpp_compiler[[fname]]
  ##   if(!is.null(this_post)) {
  ##     ans[[fname]] <- passByReferenceIntoC(ans[[fname]],
  ##                                          refArgs = this_post[['refArgs']],
  ##                                          blockRefArgs = this_post[['blockRefArgs']])
  ##   }
  ## }
  return(ans)
}

## Functions for saving and loading RcppPackets
## Created with assistance from Copilot using Claude Sonnet 4

#' Save an RcppPacket to disk as a set of files
#'
#' @param RcppPacket The RcppPacket object to save
#' @param dir Directory to save the packet files in
#' @param name Base name for the packet files (defaults to filebase from packet)
#' @return Invisibly returns the directory path where files were saved
#' @export
saveRcppPacket <- function(RcppPacket, dir, name = NULL) {
  if (is.null(name)) {
    name <- RcppPacket$filebase
    if (is.null(name)) {
      stop("RcppPacket must have a filebase element or name must be provided")
    }
  }

  # Normalize the directory path for platform independence
  dir <- normalizePath(dir, mustWork = FALSE)

  # Create directory if it doesn't exist
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # Helper function to write content safely
  writePacketElement <- function(content, filename) {
    filepath <- normalizePath(file.path(dir, filename), mustWork = FALSE)
    if (is.null(content) || length(content) == 0) {
      # Create empty file to indicate element exists but is empty
      file.create(filepath)
    } else if (is.character(content)) {
      writeLines(content, filepath)
    } else if (is.list(content)) {
      # For structured content like cppContent/hContent
      if (all(c("opener", "body") %in% names(content))) {
        # Write as separate sections
        con <- file(filepath, "w")
        writeLines("### OPENER ###", con)
        if (length(content$opener) > 0) {
          writeLines(content$opener, con)
        }
        writeLines("### BODY ###", con)
        if (length(content$body) > 0) {
          writeLines(content$body, con)
        }
        close(con)
      } else {
        # Other list content - serialize to text
        dput(content, file = filepath)
      }
    } else {
      # For other types, use dput
      dput(content, file = filepath)
    }
  }

  # Write each element to its own file
  writePacketElement(RcppPacket$preamble, paste0(name, "_preamble.txt"))
  writePacketElement(RcppPacket$cppContent, paste0(name, "_cppContent.txt"))
  writePacketElement(RcppPacket$hContent, paste0(name, "_hContent.txt"))
  writePacketElement(RcppPacket$filebase, paste0(name, "_filebase.txt"))
  writePacketElement(RcppPacket$post_cpp_compiler, paste0(name, "_post_cpp_compiler.txt"))
  writePacketElement(RcppPacket$copyFiles, paste0(name, "_copyFiles.txt"))

  # Write a manifest file listing what was saved
  manifest <- list(
    saved_at = Sys.time(),
    packet_name = name,
    elements = names(RcppPacket),
    files = list(
      preamble = paste0(name, "_preamble.txt"),
      cppContent = paste0(name, "_cppContent.txt"),
      hContent = paste0(name, "_hContent.txt"),
      filebase = paste0(name, "_filebase.txt"),
      post_cpp_compiler = paste0(name, "_post_cpp_compiler.txt"),
      copyFiles = paste0(name, "_copyFiles.txt")
    )
  )

  manifest_path <- normalizePath(file.path(dir, paste0(name, "_manifest.txt")), mustWork = FALSE)
  dput(manifest, file = manifest_path)

  invisible(normalizePath(dir))
}

#' Load an RcppPacket from disk files
#'
#' @param dir Directory containing the packet files
#' @param name Base name of the packet files to load
#' @return An RcppPacket object (list)
#' @export
loadRcppPacket <- function(dir, name) {
  # Normalize and check if directory exists
  dir <- normalizePath(dir, mustWork = TRUE)

  # If name is missing, try to find a manifest file and use its base name
  if (missing(name)) {
    manifest_files <- list.files(dir, pattern = "_manifest\\.txt$", full.names = FALSE)
    if (length(manifest_files) == 0) {
      stop("No manifest files (from saved RcppPackets) found in directory: ", dir)
    }
    if (length(manifest_files) > 1) {
      stop("More than one manifest files (from saved RcppPackets) found in directory: ", dir, ". Provide a name argument to choose one.")
    }
    # Use the first manifest file found
    name <- sub("_manifest\\.txt$", "", manifest_files[1])
  }

  # Load manifest first to check what files should exist
  manifest_file <- normalizePath(file.path(dir, paste0(name, "_manifest.txt")), mustWork = FALSE)
  if (file.exists(manifest_file)) {
    manifest <- dget(manifest_file)
    cat("Loading RcppPacket saved at:", as.character(manifest$saved_at), "\n")
  } else {
    warning("No manifest file found. Attempting to load standard files.")
    manifest <- list(files = list(
      preamble = paste0(name, "_preamble.txt"),
      cppContent = paste0(name, "_cppContent.txt"),
      hContent = paste0(name, "_hContent.txt"),
      filebase = paste0(name, "_filebase.txt"),
      post_cpp_compiler = paste0(name, "_post_cpp_compiler.txt"),
      copyFiles = paste0(name, "_copyFiles.txt")
    ))
  }

  # Helper function to read content safely
  readPacketElement <- function(filename) {
    filepath <- normalizePath(file.path(dir, filename), mustWork = FALSE)
    if (!file.exists(filepath)) {
      return(NULL)
    }

    # Check if file is empty
    if (file.size(filepath) == 0) {
      return(NULL)
    }

    # Try to detect the content type
    first_line <- readLines(filepath, n = 1, warn = FALSE)

    if (length(first_line) == 0) {
      return(character(0))
    }

    # Check if it's a structured file (cppContent/hContent)
    if (first_line == "### OPENER ###") {
      lines <- readLines(filepath, warn = FALSE)
      opener_start <- which(lines == "### OPENER ###")
      body_start <- which(lines == "### BODY ###")

      if (length(opener_start) == 1 && length(body_start) == 1) {
        opener_lines <- if (body_start > opener_start + 1) {
          lines[(opener_start + 1):(body_start - 1)]
        } else {
          character(0)
        }

        body_lines <- if (length(lines) > body_start) {
          lines[(body_start + 1):length(lines)]
        } else {
          character(0)
        }
        return(list(opener = opener_lines, body = body_lines))
      }
    }

    # Check if it looks like dput output
    if (grepl("^(list|c|character|NULL|[0-9])", first_line)) {
      tryCatch({
        return(dget(filepath))
      }, error = function(e) {
        # If dget fails, treat as plain text
        return(readLines(filepath, warn = FALSE))
      })
    }

    # Default: read as character lines
    return(readLines(filepath, warn = FALSE))
  }

  # Load each element
  RcppPacket <- Rcpp_nCompilerPacket(
    preamble = readPacketElement(manifest$files$preamble),
    cppContent = readPacketElement(manifest$files$cppContent),
    hContent = readPacketElement(manifest$files$hContent),
    filebase = readPacketElement(manifest$files$filebase),
    post_cpp_compiler = readPacketElement(manifest$files$post_cpp_compiler),
    copyFiles = readPacketElement(manifest$files$copyFiles)
  )

  return(RcppPacket)
}

#' List available saved RcppPackets in a directory
#'
#' @param dir Directory to search for saved packets
#' @return Character vector of packet names found
#' @export
listRcppPackets <- function(dir) {
  # Normalize path, but allow non-existent directories
  dir <- normalizePath(dir, mustWork = FALSE)

  if (!dir.exists(dir)) {
    return(character(0))
  }

  manifest_files <- list.files(dir, pattern = "_manifest\\.txt$", full.names = FALSE)
  packet_names <- sub("_manifest\\.txt$", "", manifest_files)

  return(packet_names)
}
