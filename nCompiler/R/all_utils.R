## Sequential label generation system:
## labelFunctionMetaCreator returns a function that returns a function.
## labelFunctionMetaCreator is only called once, immediately below, to create labelFunctionCreator
## The outer layer allows allLabelFunctionCreators to be in the closure of every function returned
## by labelFunctionCreator.  Each of those functions is registered as an element of allLableFunctionCreators.
##
## This scheme allows the function resetLabelFunctionCreators below to work simply,
## resetting the count to 1 for all of the label generators.
##
## The motivation for resetLabelFunctionCreators is for testing: If we want to check
## that two pathways to code generation (one existing, one experimental) create identical
## code, it is helpful to have identical generated labels.  Resetting all label generators
## supports this goal.
labelFunctionMetaCreator <- function() {
  allLabelFunctionCreators <- list()

  creatorFun <- function(lead, start = 1) {
    nextIndex <- start
    force(lead)
    labelGenerator <- function(reset = FALSE, count = 1, envName = "") {
      if(reset) {
        nextIndex <<- 1
        return(invisible(NULL))
      }
      lead <- paste(lead, envName , sep = '_')
      ans <- paste0(lead, nextIndex - 1 + (1:count))
      nextIndex <<- nextIndex + count
      ans
    }
    allLabelFunctionCreators[[ length(allLabelFunctionCreators) + 1 ]] <<- labelGenerator
    labelGenerator
  }
  creatorFun
}

labelFunctionCreator <- labelFunctionMetaCreator()

resetLabelFunctionCreators <- function() {
  allLabelFunctionCreators <- environment(labelFunctionCreator)$allLabelFunctionCreators
  for(i in allLabelFunctionCreators) {
    i(reset = TRUE)
  }
}

ADtapeMgrLabelCreator <- labelFunctionCreator("ADtapeMgr")
nodeFxnLabelCreator <- labelFunctionCreator("nodeFxn")
modelLabelCreator <- labelFunctionCreator("model")

# no longer documented in Rd
# Generates a valid C++ name from an R Name
#
# replaces [ ( $ and a few other symbols with underscores, and removes ] ) and spaces in a string
#
# @param rName A String
# @return returns a string representing the modified rName
# @author Jagadish Babu
# @keywords Name
# @export
# @examples
#  genName('theta[1]')
Rname2CppName <- function(rName, colonsOK = TRUE) {
  ## This will serve to replace and combine our former Rname2CppName and nameMashupFromExpr
  ## which were largely redundant
  if (!is.character(rName))
    rName <- deparse(rName)

  if( colonsOK) {
    # Substitute single colons but preserve double colons.
    rName <- gsub('::', '_DOUBLE_COLON_', rName)
    rName <- gsub(':', 'to', rName)  # replace colons with 'to'
    rName <- gsub('_DOUBLE_COLON_', '::', rName)
  } else if(any(grepl(':', rName))) {
    stop(paste0('trying to do name mashup on expression with colon (\':\') from ',
                paste(rName[grepl(':', rName)], collapse=', ')))
  }
  rName <- gsub(' ', '', rName)
  rName <- gsub('\\.', '_dot_', rName)
  rName <- gsub("\"", "_quote_", rName)
  rName <- gsub(',', '_comma_', rName)
  rName <- gsub("`", "_backtick_" , rName)
  rName <- gsub('\\[', '_oB', rName)
  rName <- gsub('\\]', '_cB', rName)
  rName <- gsub('\\(', '_oP', rName)
  rName <- gsub('\\)', '_cP', rName)
  rName <- gsub("\\$", "_" , rName)
  rName <- gsub(">=", "_gte_", rName)
  rName <- gsub("<=", "_lte_", rName)
  rName <- gsub("<=", "_eq_", rName)
  rName <- gsub("!=", "_neq_", rName)
  rName <- gsub(">", "_gt_", rName)
  rName <- gsub("<", "_lt_", rName)
  rName <- gsub("!", "_not_", rName)
  rName <- gsub("\\|\\|", "_or2_", rName)
  rName <- gsub("&&", "_and2_", rName)
  rName <- gsub("\\|", "_or_", rName)
  rName <- gsub("&", "_and_", rName)
  rName <- gsub("%%", "_mod_", rName)
  rName <- gsub("%\\*%", "_matmult_", rName)
  rName <- gsub("=", "_eq_" , rName)
  rName <- gsub("\\(", "_" , rName)
  rName <- gsub("\\+", "_plus_" , rName)
  rName <- gsub("-", "_minus_" , rName)
  rName <- gsub("\\*", "_times_" , rName)
  rName <- gsub("/", "_over_" , rName)
  rName <- gsub('\\^', '_tothe_', rName)
  rName <- gsub('^_+', '', rName) # remove leading underscores.  can arise from (a+b), for example
  rName <- gsub('^([[:digit:]])', 'd\\1', rName)    # if begins with a digit, add 'd' in front
  rName
}

## This takes a character vector as the first argument and length-1
## character vector as the second argument.  It returns a list with
## the first vector as names and the second argument as the value of
## each element.  E.g. makeCallList(c('A','B'), 'foo') is equivalent
## to list(A = 'foo', B = 'foo')
makeCallList <- function(opList, call) {
  ans <- rep(list(call), length(opList))
  names(ans) <- opList
  ans
}

## This allows definition of a template and substitution into it.  It is a
## shortcut over the painful eval(substitute(substitute(...))) idiom
## example:
## template <- quote({RNAME <- foo(CNAME)})
## l1 <- codeSubstitute(template, list(RNAME = 'Ra', CNAME = as.name('ca')))
codeSubstitute <- function(code, subList) {
  eval(substitute(substitute(code, subList), list(code = code)))
}

makeSpaces <- function(num) paste(rep(' ', num), collapse = '')

pasteSemicolon <- function(x, indent = '') {
  if(is.numeric(indent)) indent <- makeSpaces(indent)
  if(is.character(x)) return(paste0(indent, x, ';'))
  if(is.list(x)) return(lapply(x, function(ix) paste0(indent, ix, ';')))
  if(is.null(x)) return(character())
  stop(paste0('Error, pasteSemicolon called for object of class ',
              class(x),
              '. Must be character or list.'),
       call. = FALSE)
}

#' Write unlisted code generated from nCompiler cpp definitions.
#'
#' This is not intended to be called directly but is useful for
#' debugging nCompiler's C++ output.
#'
#' @param x A (potentially nested) list of C++ code in character
#'     strings.
#' @param ... Argument provided in \code{...} will be passed to
#'     \code{writeLines}
#' @export
writeCode <- function(x, ...) writeLines(unlist(x), ...)

#' return sizes of an object whether it is a vector, matrix or array
#'
#' R's regular \code{dim} function returns NULL for a vector.  It is useful to have this function that treats a vector similarly to a matrix or array.  Works in R and nCompiler.  In nCompiler \code{dim} is identical to \code{nDim}, not to R's \code{dim}
#'
#' @param obj  objects for which the sizes are requested
#'
#' @return a vector of sizes in each dimension
#'
#' @author nCompiler development team
#'
#' @aliases dim
#'
#' @examples
#' x <- rnorm(4)
#' dim(x)
#' nDim(x)
#' y <- matrix(x, nrow = 2)
#' dim(y)
#' nDim(y)
#'
#' @export
nDim <- function(obj) {
  if(is.null(dim(obj))) return(length(obj))
  return(dim(obj))
}

is.blank <- function(arg) {
  if(is.null(arg)) return(FALSE)
  return(identical(arg, quote(x[])[[3]]))
}


# Modified from nimble, including comments
# simply adds width.cutoff = 500 as the default to deal with creation of long variable names from expressions
# The control list is the default plus "digits17", which is the only one done in nimble.
# We need to deparse lists (e.g. in build_compiled_nClass) and have the names in the deparsed result.
# I think "niceNames" does that, possibly  "showAttributes" too.
deparse <- function(...) {
  control <- c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")
  if("width.cutoff" %in% names(list(...))) {
    base::deparse(..., control = control)
  } else {
    base::deparse(..., width.cutoff = 500L, control = control)
  }
}

## This version of deparse avoids splitting into multiple lines, which generally would lead to
## problems. We keep the original nimble:::deparse above as deparse is widely used and there
## are cases where not modifying the nlines behavior may be best.
safeDeparse <- function(..., warn = FALSE) {
  out <- deparse(...)
  if(isTRUE(get_nOption('useSafeDeparse'))) {
    dotArgs <- list(...)
    if("nlines" %in% names(dotArgs))
      nlines <- dotArgs$nlines else nlines <- 1L
    if(nlines != -1L && length(out) > nlines) {
      if(warn)
        message("  [Note] safeDeparse: truncating deparse output to ", nlines, " line", if(nlines>1) "s" else "")
      out <- out[1:nlines]
    }
  }
  return(out)
}
