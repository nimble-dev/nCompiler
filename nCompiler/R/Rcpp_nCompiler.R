## This file contains tools for using Rcpp's compilation system to manage
## compilation of nCompiler-generated C++.

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
    Hincludes <- cppDef$getHincludes()
    CPPincludes <- cppDef$getCPPincludes()
    Hpreamble <- cppDef$getHpreamble()
    CPPpreamble <- cppDef$getCPPpreamble()
    
    CPPusings <- cppDef$getCPPusings()
    
    Hincludes <- unique(Hincludes)
    CPPincludes <- unique(CPPincludes)
    Hpreamble <- unique(Hpreamble)
    CPPpreamble <- unique(CPPpreamble)
    CPPusings <- unique(CPPusings)
    

    selfCPP <- paste0('"', filebase, '.cpp"')
    CPPincludes <- CPPincludes[ CPPincludes != selfCPP ]

    selfH <- paste0('"', filebase, '.h', '"')
    CPPincludes <- c(CPPincludes, selfH)

    allCppDefs <- cppDef$getDefs()

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

    cppContent <- collate_nCompiler_CppCode(preamble = CPPpreamble,
                                       includes = CPPincludes,
                                       usings = CPPusings,
                                       code = cppCode,
                                       ifndefName = paste0('__', name, '_CPP')
                                       )

    hContent <- collate_nCompiler_CppCode(preamble = Hpreamble,
                                     includes = Hincludes,
                                     code = hCode,
                                     ifndefName = paste0('__', name, '_H')
                                     )
    
    Rcpp_nCompilerPacket(
        cppContent = cppContent,
        hContent = hContent,
        filebase = filebase
    )    
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

    c(openifndefOut,
      preamble,
      includesOut,
      usings,
      code,
      closeifndefOut)
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

## Write file .cpp and .h files and call sourceCpp_nCompiler to compile
## them.
## C++ code (not intended to be called directly).

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
                      dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                      cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                      env = parent.frame(),
                      write = TRUE,
                      compile = TRUE,
                      ...) {

  if(write) {
    writeCpp_nCompiler(Rcpp_packet,
                   dir)
  }
  if(!compile) {
    return(Rcpp_packet)
  }
  if(isTRUE(get_nOption('pause_after_writing_files')))
    browser()
  compileCpp_nCompiler(Rcpp_packet,
                   dir,
                   cacheDir,
                   env,
                   ...)
}

writeCpp_nCompiler_combine <- function(RcppPacket_list,
                                       cppfile,
                                       dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                                       header.dir) {
  if(missing(cppfile))
    stop("A cppfile name must be provided for the combined contents of the RcppPackets.")
  dir.create(dir, showWarnings = FALSE)
  cppfilepath <- file.path(dir, cppfile)
  con <- file(cppfilepath, open = "w")
  for(RcppPacket in RcppPacket_list) {
    ## write all cpp contents to one file
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
  if(include_cpp) { 
    if(makeStandardFiles) {
      dir.create(dir, showWarnings = FALSE)
      cppfile <- paste0(Rcpp_packet$filebase,
                        ".cpp")
      cppfilepath <- file.path(dir, cppfile)
      con <- file(cppfilepath,
                  open = "w")
    }
    writeLines(Rcpp_packet$cppContent, con)
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
    writeLines(Rcpp_packet$hContent, con)
    if(makeStandardFiles)
      close(con)
  }
  invisible(Rcpp_packet)
}

#' @export
#' Returns a pair of vectors, one containing the compiled function names and the other containing
#' the corresponding environments.
compileCpp_nCompiler <- function(Rcpp_packet,
                                 dir = file.path(tempdir(), 'nCompiler_generatedCode'),
                                 cacheDir = file.path(tempdir(), 'nCompiler_RcppCache'),
                                 env = parent.frame(),
                                 returnList = FALSE, ## force result list even for a singleton
                                 ...) {
  if(!dir.exists(dir)) 
    stop(paste0("directory ", dir, " does not exist."))
  if(is.character(Rcpp_packet)) {
    cppfile <- Rcpp_packet
  } else {
    cppfile <- paste0(Rcpp_packet$filebase,
                      ".cpp")
  }
  cppfilepath <- file.path(dir, cppfile)
  
  exported <- sourceCpp_nCompiler(file = cppfilepath,
                                  cacheDir = cacheDir,
                                  env = env,
                                  ...)
  
  ## Next lines copy/imitate Rcpp::cppFunction,
  ## for now without error checking until we see
  ## final uses.
  numFunctions <- length(exported$functions)
  if(numFunctions == 0) return(invisible(NULL))
  if(numFunctions == 1 & !returnList) return(get(exported$functions[[1]], env))
  return(structure(
    lapply(exported$functions, 
           function(x) get(x, env)),
    names = exported$functions)
    )
}
