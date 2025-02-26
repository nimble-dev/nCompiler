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
  name <- cppDef$name
  if(missing(filebase))
    filebase <- make_cpp_filebase(name)

  allCppDefs <- cppDef$getInternalDefs()

  Hincludes <- allCppDefs |> lapply(\(x) x$getHincludes()) |> unlist(use.names=FALSE)
  CPPincludes <- allCppDefs |> lapply(\(x) x$getCPPincludes()) |> unlist(use.names=FALSE)
  Hpreamble <- allCppDefs |> lapply(\(x) x$getHpreamble()) |> unlist(use.names=FALSE)
  CPPpreamble <- allCppDefs |> lapply(\(x) x$getCPPpreamble()) |> unlist(use.names=FALSE)
  CPPusings <- allCppDefs |> lapply(\(x) x$getCPPusings()) |> unlist(use.names=FALSE)

  ## Hincludes <- cppDef$getHincludes()
  ## CPPincludes <- cppDef$getCPPincludes()
  ## Hpreamble <- cppDef$getHpreamble()
  ## CPPpreamble <- cppDef$getCPPpreamble()
  ## CPPusings <- cppDef$getCPPusings()

  Hincludes <- unique(Hincludes)
  CPPincludes <- unique(CPPincludes)
  Hpreamble <- unique(Hpreamble)
  CPPpreamble <- unique(CPPpreamble)
  CPPusings <- unique(CPPusings)

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
             )
    )

  hCode <-
    unlist(
      lapply(allCppDefs,
             function(x)
               capture.output( {
                 writeLines("")
                 writeCode(x$generate(declaration=TRUE))
               })
             )
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
    post_cpp_compiler = post_cpp_compiler
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
                         dir)
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
                       con = con,
                       include_h = FALSE)
    ## write each h contents to its own file
    writeCpp_nCompiler(RcppPacket,
                       dir = dir,
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
