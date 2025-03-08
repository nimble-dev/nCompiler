nClassLabelMaker <- labelFunctionCreator('nClass')

nClassClass <- R6::R6Class(
  classname = "nClass",
  portable = FALSE
)

CnClassClass <- R6::R6Class(
  classname = "CnClass",
  inherit = nClassClass,
  portable = FALSE
)

#' Create a nClass definition
#'
#' A nClass is like an R6 class, but it allows some data and
#' methods (written as nFunctions) to be automatically compiled
#' via C++, while others are in pure R.  nClasses are implemented
#' using R6 classes.
#'
#' @param classname name for the class.
#' @param Rpublic list of public data and methods (functions) for use from R
#'   only.
#' @param Cpublic list of public data (with type declarations) and methods
#'   (nFunctions) that can be turned into C++ via \link{nCompile_nClass}. As in
#'   R6 classes (see \link{R6Class}), data and methods go in the same list.
#' @param enableDerivs list or character vector of methods in Cpublic for which
#'   derivatives should be enabled.
#' @param enableSaving logical indicating whether C++ should include support for
#'   saving objects. Defaults to the value of nOptions("enableSerialize"), which
#'   defaults to \code{TRUE}.
#' @param env An environment that should be used as the enclosing environment
#'   for objects created from the nClass
#' @return An R6 class generator enhanced with information needed for
#'   compilation.
#' @details Use of \code{enableDerivs} will substantially slow down C++
#'   compilation. See \code{help(nDerivs)} about how to obtain derivatives from
#'   enabled methods.
#'
#' Setting \code{enableSaving=TRUE} will somewhat slow down C++ compilation. See
#' \code{help(nSave)} and \code{help(nSerialize)} about how to save objects.
#' (The technical jargon for converting data structures such as class objects
#' into data suitable for saving and later loading is "serialization".)
#'
#' The internal information used for compilation can be accessed with
#'   \code{NCinternals(nClassGenerator)} and modified with
#'   \code{NCinternals(nClassGenerator)<-}, where \code{nClassGenerator} was
#'   returned from a call to \code{nClass}.
#' @examples
#' \donttest{
#'     nc1 <- nClass(
#'     Rpublic = list(
#'         Rv = NULL,
#'         Rfoo = function(x) x+1
#'     ),
#'     Cpublic = list(
#'         Cv = 'numericScalar',
#'         Cfoo = nFunction(
#'             fun = function(x) {
#'                 return(x+1)
#'             },
#'             argTypes = list(x = 'numericScalar'),
#'             returnType = 'numericScalar')
#'         )
#'   )
#' }
#' @export
nClass <- function(classname,
                   Rpublic = list(),
                   Cpublic = list(),
                   enableDerivs = character(),
                   enableSaving = get_nOption("enableSaving"),
                   inherit = NULL,
                  # control = list(),
                   compileInfo = list(),
                   predefined = FALSE,
                   env = parent.frame()) {
  # supported elements of compileInfo:
  # exportName: name of the R function to call the
  #    C/C++ function for a new object. Defaults to paste0("new_", classname)
  # interface ("full"): "full", "generic" or "none". First two: build interface. Fine-grained control possible in future.
  # interfaceMembers(NULL): character vector of which members (variables and methods) to include. Default to all
  # depends (list()): Values for Cpp::depends.
  # inherit: list of C++ class inheritances (all public inheritance).
  #   If one is named "base", it will replace the inheritance from
  #   an inherit base nClass. Multiple inheritance among nClasses
  #   is not supported (same for R6 classes), so there is no ambiguity.
  #   If the nClass argument inherit is provided but compileInfo$inherit$base
  #   is not provided, inheritance will be determined from the base nClass.
  #   All inheritance provided by compileInfo$inherit should include any
  #   accessor specifier, typically "public", e.g. "public some_class".
  #   Similarly, template arguments (include CRTP) should be in the text explicitly.
  compileInfo <- updateDefaults(
    list(exportName = NULL, interface = "full",
         interfaceMembers = NULL,
         depends = list(),
         inherit = list()),
    compileInfo
  )
  if(missing(classname))
    classname <- nClassLabelMaker()
  internals = NC_InternalsClass$new(classname = classname,
                                    Cpublic = Cpublic,
                                    isOnlyC = length(Rpublic) == 0,
                                    enableDerivs = enableDerivs,
                                    enableSaving = enableSaving,
                                    inherit = inherit,
                                 #   control = control,
                                    compileInfo = compileInfo,
                                    predefined = predefined,
                                    env = env)
  ## We put the internals in 2 places:
  ## 1. in an environment layer around every instance
  new_env <- new.env(parent = env)
  # The R6Class inherit argument has weird handling:
  # "captured as an unevaluated expression which is evaluated in parent_env each time an object is instantiated."
  # so if provided in the nClass call, we stick it in new_env.
  # (That is not the only reason for new_env.)
  if(!is.null(inherit)) new_env$.inherit_obj <- inherit
  new_env$.NCinternals <- internals
  # Uncompiled behavior for Cpublic fields needs to be handled.
  # Right now a type string like 'numericScalar' just becomes a
  # default value.
  eval(substitute(
    result <- R6::R6Class(
      classname = classname,
      public = c(Rpublic, Cpublic),
      portable = FALSE,
      inherit = INHERIT,
      parent_env = new_env
    ),
    list(INHERIT =
           if(!is.null(inherit)) quote(.inherit_obj)
         else quote(nCompiler:::nClassClass))
    ))
  ## 2. in the generator
  result$.nCompiler <- internals
  ## NB: We want to avoid having every object
  ## include the generator, to keep saving light.
  result
}

# Provenance of names for an nClass:
#
# classname is provided by the user or generated automatically.
#   This is used as the R6 classname.
#
# NC_InternalsClass does not keep track of the name.
#
# In nCompile, the classname is used as the cpp_name

# See nCompile comments for more.
