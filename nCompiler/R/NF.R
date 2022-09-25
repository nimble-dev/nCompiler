nFunctionLabelMaker <- labelFunctionCreator('nFun')

## nFunction represents a pure function, not a class, although it
## may sometimes be implemented as a class.

## In particular, when derivatives are enabled, a nFunction wil be
## implemented as a class, with the CppAD tape as member data.

## Argument types and/or passing semantics can be provided in the argument
## fields and function body or indicated in separate arguments.

## This almost allows a pure R function to be simply modified ("decorated")
## to be a nFunction, with the exception that explicit return() statements
## are required.

## nClass represents a class with R fields/methods and C fields/methods,
## all explicitly defined.

## A method in a nClass is a nFunction.

## The old.nCompiler approach of smartly determining a class definition from
## evaluated setup code is a special case that will continue to be supported.
## It will create a nClass.


## nFunctionClass inherits from function.
## Hence an object can be used as a function but also has slots (accessed with @)
## for internal content.
nFunctionClass <- setClass(
  Class = "nFunction",
  contains = "function",
  slots = list(
    internals = "ANY"       ## An NF_InternalsClass.
    , originalCode = "ANY"
    
  )
)

#' Create a nFunction.
#'
#' Create a nFunction, which can be compiled via C++ using \link{nCompile_nFunction} or \code{nCompile} (TBD)
#'
#' @param fun R function to be turned into a nFunction
#' @param name An internal name for the nFunction.  If \code{NA}, an internal name will be generated.
#' @param argTypes List of argument types declarations.  An alternative is to provide argument types as part of default values in \code{fun}.  See details below.
#' @param refArgs Character vector of names of arguments that should be passed by reference instead of by value.  An alternative is to indicate pass-by-reference as part of a type declaration in \code{refArgs} or in default value(s) of \code{fun}.  See details below.
#' @param returnType A type declaration for the type returned by \code{fun}.  An alternative is to provide this information with a \code{returnType()} statement in the body of \code{fun}.
#' @param enableDerivs Allows derviatives to be obtained automatically.  Currently disabled.
#' @param check If \code{TRUE}, \code{fun} will be checked for errors (including anything that cannot be compiled.  (This is currently disabled.)
#' @param returnCallable If \code{TRUE}, return a \code{nFunction} object that can be used as a funtion (because it is a function).  If \code{FALSE} (only for advanced debugging), return the internal information of the \code{nFunction}.
#' @param where Environment to be used as the closure of the returned \code{nFunction}.
#' @return An object of class \code{nFunction}, which inherits from class \code{function}.
#' @details A \code{nFunction} is a special kind of R function that can be compiled by automatic generation of C++.  See (TBD) for information about writing \code{nFunctions}.  See (TBD) for information about type declarations.
#' 
#' @seealso \cite{\link{NFinternals}} for access to the internal information of a \code{nFunction} (for advanced use only).
#'
#' @examples
#' \donttest{
#' rawfoo <- function(a = 5, b) {
#'     b <- b + a
#'     return(a) ## explicit use of return() is necessary
#' }
#' foo <- nFunction(
#'     fun = rawfoo,
#'     argTypes = list(a = "numericScalar()",
#'                     b = "ref(integerVector())"),
#'     # First alternative is to provide arguments in \code{rawfoo} as
#'     #    function(a = numericScalar(5), b = ref(integerVector()))
#'     # Second alternative is to use b = integerVector() and provide
#'     #    refArgs = "b".
#'     returnType = "numericScalar()"
#'     # Alternative would be to include "returnType(numericVector())"
#'     # in \code{rawfoo}
#' )
#' }
#' @export
nFunction <- function(fun,
                      name = NA,
                      argTypes = list(),
                      refArgs = character(),
                      blockRefArgs = character(),
                      returnType = NULL,
                      enableDerivs = list(),
                      check = get_nOption('check_nFunction'),
                      returnCallable = TRUE,
                      where = parent.frame(),
                      ...
) {
  ## Provide a default label is one is needed.
  if(is.na(name))
    name <- nFunctionLabelMaker()
  
  ## Create internals that will be used for compilation.
  internals <- NF_InternalsClass$new(fun,
                                     name = name,
                                     argTypes = argTypes,
                                     refArgs = refArgs,
                                     blockRefArgs = blockRefArgs,
                                     returnType = returnType,
                                     enableDerivs = enableDerivs,
                                     check = check,
                                     where = where)
  ## Return a callable function.
  ## This will be modified:
  ## 1. to provide pass-by-reference behavior
  ## as requested for any arguments.
  ## 2. with any returnType() statement removed.
  if(returnCallable) {
    modifiedFun <- internals$getFunction()
    nFunctionClass(modifiedFun, internals = internals)
  }
  else
    internals
}

# Provenance of names
#
# nFunction creates a name either from the user or from a label maker.
# This name goes in the NF_InternalsClass object, where it is also copied to uniqueName
#
# The NF_InternalsClass object makes a cpp_code_name by pasting the name to a unique ID separated by "_"
# 
