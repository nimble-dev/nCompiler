nCompilerUserNamespace <- as.environment(list(sessionSpecificDll = NULL))
# new.env() here fails with: Error in as.environment(pos) : using 'as.environment(NULL)' is defunct when testing package loading during INSTALL

updateDefaults <- function(defaults, control) {
  defaults[names(control)] <- control
  defaults
}

.nOptions <- as.environment(
  list(
    enableSaving = TRUE,
    check_nFunction = TRUE,  ## check syntax of nFunction fun
    showCompilerOutput = FALSE,
    use_nCompLocal = FALSE,
    debugSizeProcessing = FALSE,
    serialize = FALSE,           # if TRUE, include serialization code in generated C++
    enableDerivs = FALSE,
    allow_method_overloading = FALSE,
    allow_inherited_field_duplicates = FALSE,
    compilerOptions = list(
      use_nCompiler_error_handling = TRUE,
      rebuild = FALSE,
      rebuildCppDef = FALSE,
      rebuildCpp = FALSE,
      filebase = NULL,
      debug = FALSE,
      debugCpp = FALSE,
      cppStacktrace = FALSE,
      throwEigenErrors = TRUE,
      logging = FALSE,
      startStage = 'start',
      endStage = 'end',
      startDebugStage = 'end',
      endDebugStage = 'end',
      writeCpp = TRUE,
      compileCpp = TRUE,
      useUniqueNameInCode = FALSE,
      generate_predefined = FALSE
    ),
    packagingOptions = list(
      export = TRUE
    ),
    modifyPackageFiles = "no",
    # localDLL_isLoaded = FALSE, # if TRUE, the localDLL has been loaded.  This will be set TRUE after first compilation.
    # localDLLdir = "nCompLocalLibrary", # directory to be used from the working directory if localDLL needs to be locally built.
    # showLocalDLLoutput = FALSE,
    error_recover = FALSE, # if TRUE, attempt to recover into a browser() after errors caught by nComp's error trapping, similarly to options(error = recover)
    pause_after_writing_files = FALSE, #if TRUE, enter browser() after generated code files have been written but before the C++ compiler has been called.
    verbose = FALSE,
    sourceCpp_verbose = FALSE,
    nimble = FALSE, ## ensure all backward compatibility
    dropSingleSizes = FALSE ## backward compatibility
  )
)

#' Set nCompiler Option
#'
#' Allow the user to set the value of a global _option_
#' that affects the way in which nCompiler operates
#'
#' @param x a character string holding an option name
#' @param value the new value of the option
#' @param listName an optional character string with the name of a list in which to find \code{x}
#' @export
#' @return \code{value}
set_nOption <- function(x, value, listName = NULL) {
  if (is.null(listName))
    .nOptions[[x]] <- value
  else {
    thisList <- .nOptions[[listName]]
    if (!is.list(thisList))
      .nOptions[[listName]] <- list() # overwrite any existing object
    .nOptions[[listName]][[x]] <- value
  }
  value
}

#' Get nCompiler Option
#'
#' Allow the user to get the value of a global _option_
#' that affects the way in which nCompiler operates
#'
#' @param x a character string holding an option name
#' @export
#' @return The value of the option.
get_nOption <- function(x) {
  .nOptions[[x]] # returns NULL is x is not in .nCompilerOptions
}

## UsenOptions for access to multiple options
## Use get_nOption for faster access to a single option
#' @export
nOptions <- function(...) {
  invisibleReturn <- FALSE
  args <- list(...)
  if (!length(args)) {
    # Get all.nCompiler options.
    return(as.list(.nOptions))
  }
  if (length(args) == 1 && is.null(names(args)) && is.list(args[[1]])) {
    # Unpack a single list of many args.
    args <- args[[1]]
  }
  if (is.null(names(args))) {
    # Get some.nCompiler options.
    args <- unlist(args)
  } else {
    # Set some.nCompiler options.
    for(i in seq_along(args)) {
      set_nOption(names(args)[[i]], args[[i]])
    }
    args <- names(args)
    invisibleReturn <- TRUE
  }
  out <- structure(lapply(args, get_nOption),
                   names = args)
  ##out <- as.list(nOptions)[args]
  if(length(out) == 1) out <- out[[1]]
  if(invisibleReturn) return(invisible(out)) else return(out)
}
