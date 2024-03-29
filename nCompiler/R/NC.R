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
#' @param classname A name for the class.
#' @param Rpublic A list of public data and methods (functions) for
#'     use from R only.
#' @param Cpublic A list of public data (with type declarations) and
#'     methods (nFunctions) that can be turned into C++ via
#'     \link{nCompile_nClass}.  As in R6 classes (see
#'     \link{R6Class}), data and methods go in the same list.
#' @param enableDerivs A list or character vector of methods in Cpublic 
#'     for which derivatives should be enabled.
#' @param env An environment that should be used as the
#'     enclosing environment for objects created from the nClass
#' @export
#' @return An R6 class generator enhanced with information needed for
#'     compilation.
#' @details The internal information used for compilation can be
#'     accessed with \code{NCinternals(nClassGenerator)} and
#'     modified with \code{NCinternals(nClassGenerator)<-}, where
#'     \code{nClassGenerator} was returned from a call to
#'     \code{nClass}.
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
nClass <- function(classname,
                   Rpublic = list(),
                   Cpublic = list(),
                   enableDerivs = character(),
                   predefined = FALSE,
                   env = parent.frame()) {
  if(missing(classname))
    classname <- nClassLabelMaker()
  internals = NC_InternalsClass$new(Cpublic = Cpublic,
                                    isOnlyC = length(Rpublic) == 0,
                                    enableDerivs = enableDerivs,
                                    predefined = predefined)
  ## We put the internals in 2 places:
  ## 1. in an environment layer around every instance
  new_env <- new.env(parent = env)
  new_env$.NCinternals <- internals
  # Uncompiled behavior for Cpublic fields needs to be handled.
  # Right now a type string like 'numericScalar' just becomes a
  # default value.
  result <- R6::R6Class(
    classname = classname,
    public = c(Rpublic, Cpublic),
    portable = FALSE,
    inherit = nCompiler:::nClassClass,
    parent_env = new_env
  )
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
