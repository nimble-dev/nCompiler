isRbracket <- function(code) {
  if(!is.call(code)) return(FALSE)
  return(code[[1]] == '{')
}

## put a single exprObject within a '{'
embedInRbracket <- function(code) {
  template <- quote({A})
  template[[2]] <- code
  template
}

embedListInRbracket <- function(code) {
  if(!is.list(code)) stop('Error: embedListInRbracket called with code that is not a list')
  as.call(c(list(as.name('{')), code))
}

splitCompileTimeArgs <- function(call, template,
                                 compileArgs = character()) {
  # call should be R code, e.g. from quote() or parse()
  if(!missing(template))
    call <- match.call(template, call)
  processedArgs <- list()
  for(compileArg in compileArgs) {
    processedArg <- NULL
    if(!is.null(call[[compileArg]])) {
      processedArg <- call[[compileArg]]
    } else if(!nCompiler:::is.blank(formals(template)[[compileArg]])) {
      processedArg <- formals(template)[[compileArg]]
    }
    if(!is.null(processedArg)) {
      processedArgs[compileArg] <- list(processedArg)
      call[compileArg] <- list(NULL)
    }
  }
  list(code = call,
       compileArgs = processedArgs)
}

## build exprClasses from an R parse tree.
## caller and callerArgID are for recursion, not to be used on first entry
#' Create.nCompiler parse tree from R code.
#'
#' Create nCompiler parse tree, an annotated bidirectional syntax tree
#' used by the.nCompiler compiler, from R code.
#'
#' @param code R code object such as returned by \code{quote()} or
#'     \code{call()}.
#' @param caller exprClass object in which \code{code} is an argument.
#'     Used only recursively.
#' @param callerArgID Integer index of which argument \code{code} is
#'     in its \code{caller}.  Used only recursively.
#' @param recursing \code{TRUE} only if called by recursion.
#' @return An \code{exprClass} object.
#' @export
nParse <- function(code,
                   caller = NULL,
                   callerArgID = numeric(),
                   recursing = FALSE,
                   opDefEnv = operatorDefEnv) { ## input code is R parse tree
  if(!recursing) {
    if(is.character(code))
      code <- parse(text = code,
                    keep.source = FALSE)[[1]]
  }
  ## NULL
  if(is.null(code)) return(NULL)
  ## name:
  if(is.name(code))
    return(exprClass$new(Rexpr = code,
                         isName = TRUE,
                         isCall = FALSE,
                         isAssign = FALSE,
                         isLiteral = FALSE,
                         name = as.character(code),
                         caller = caller,
                         callerArgID = callerArgID)
           )
  ## literal
  if(is.numeric(code) |
       is.logical(code) |
       is.character(code) |
       is.null(code)) {
    return(exprClass$new(Rexpr = code,
                         isName = FALSE,
                         isCall = FALSE,
                         isAssign = FALSE,
                         isLiteral = TRUE,
                         name = code,
                         caller = caller,
                         callerArgID = callerArgID)
           )
  }
  ## call
  if(is.call(code)) {
    if(is.call(code[[1]])) {
      ## This is a chained call like a(b)(c) or a[[b]](c).
      ## We wrap these as chainedCall(a(b), c) or chainedCall(a[[b]], c)
      code <- as.call(c(list(as.name('chainedCall')), as.list(code))) 
    }
    name <- as.character(code[[1]])
    processedCompileArgs <- list()
    if(!is.null(opDefEnv)) {
      opDef <- opDefEnv[[name]]
      if(!is.null(opDef)) {
        template <- opDef$matchDef
        if(!is.null(template)) {
          processedCall <- splitCompileTimeArgs(call=code,
                                                template=template,
                                                compileArgs=opDef$compileArgs)
          code <- processedCall$code
          processedCompileArgs <- processedCall$compileArgs
        }
      }
    }
    isAssign <- name %in% c('<-','=','<<-')
    args <- vector('list', length = length(code)-1)
    ## build the object
    ans <- exprClass$new(Rexpr = code,
                         isName = FALSE,
                         isCall = TRUE,
                         isAssign = isAssign,
                         isLiteral = FALSE,
                         name = name,
                         args = args,
                         caller = caller,
                         callerArgID = callerArgID)
    if(length(processedCompileArgs))
      ans$aux$compileArgs <- processedCompileArgs
    ## Ensure that bodies of for, if, while, switch, and run.time are
    ## in { expressions.
    ## Doing this here reduces special-case checking in later processing.
    if(name == 'for') {
      if(!isRbracket(code[[4]]))
        code[[4]] <- embedInRbracket(code[[4]])
    }
    if(name %in% ifOrWhile) {
      ## 'then' or 'while' clause
      if(!isRbracket(code[[3]]))
        code[[3]] <- embedInRbracket(code[[3]])
      ## 'else' clause
      if(length(code)==4) {
        if(!isRbracket(code[[4]]))
          code[[4]] <- embedInRbracket(code[[4]])
      }
    }
    if(name == 'nimSwitch') {
      if(length(code) > 3)
        for(iSwitch in 4:length(code))
          if(!isRbracket(code[[iSwitch]]))
            code[[iSwitch]] <- embedInRbracket(code[[iSwitch]])
    }
    if(name == 'run.time') {
      if(!isRbracket(code[[2]]))
        code[[2]] <- embedInRbracket(code[[2]])
    }
    if(name == 'map') {
      ## special treatment. just stick the remaining arguments
      ## in as a list
      ans$args <- as.list(code[-1])
      return(ans)
    }
    
    ## populate args with recursive calls
    if(length(code) > 1) {
      if(!is.null(names(code)))
        ## If there are named arguments, use those names for names(args)
        ## entries like "" on the RHS leave no name on LHS --> good.
        names(ans$args) <- names(code)[-1]
      ## Note for NULL this removes the list entry.
      ## Not very general, but handles return(invisible(NULL))
      for(i in 2:length(code))
        ans$args[i-1] <- list(nParse(code[[i]],
                                     caller = ans,
                                     callerArgID = i-1,
                                     recursing = TRUE,
                                     opDefEnv=opDefEnv))
    }
    return(ans)
  }
  stop(paste0('Something is wrong in nParse handling: ', deparse(code)),
       call. = FALSE)
}


## Operator lists for nDeparse
binaryMidDoubleOperators <- c('/', '^')
binaryMidPromoteNoLogicalOperators <- c('*','%%')
binaryMidOperators <- c(binaryMidDoubleOperators,
                        binaryMidPromoteNoLogicalOperators)
binaryMidLogicalOperatorsLogical <- c('&','|')
binaryMidLogicalOperatorsComparison <- c('==','!=','<=','>=','<','>')
binaryMidLogicalOperators <- c(binaryMidLogicalOperatorsLogical,
                               binaryMidLogicalOperatorsComparison)
binaryOrUnaryOperators <- c('+','-')
assignmentOperators <- c('<-','<<-','=')

midOperatorsForDeparse <- as.list(
  paste0(' ',
         c(binaryMidOperators,
           binaryMidLogicalOperators,
           binaryOrUnaryOperators,
           assignmentOperators),
         ' ')
)

names(midOperatorsForDeparse) <- c(binaryMidOperators,
                                   binaryMidLogicalOperators,
                                   binaryOrUnaryOperators,
                                   assignmentOperators)

rm(binaryMidDoubleOperators,
   binaryMidPromoteNoLogicalOperators,
   binaryMidOperators,
   binaryMidLogicalOperatorsLogical,
   binaryMidLogicalOperatorsComparison,
   binaryMidLogicalOperators,
   binaryOrUnaryOperators)
## assignmentOperators should be kept

midOperatorsForDeparse <- c(midOperatorsForDeparse,
                            list('$' = '$', '%*%' = ' %*% ', ':' = ':', '%o%' = '%o%'))

brackOperatorsForDeparse <- list('[' = c('[',']'),
                                 '[[' = c('[[',']]'))


#' Convert a nCompiler parse tree to an R parse tree or text.
#'
#' Convert a nCompiler parse tree, specifically an \code{exprClass}
#' object, to an R parse tree or text.
#'
#' @param code An \code{exprClass} object, representing a.nCompiler parse
#'     tree.
#' @param indent Amount of indentation in text output if \code{toR} is
#'     \code{FALSE}
#' @param toR If \code{TRUE}, the returned object will be an R parse
#'     tree (similar to what is returned from \code{quote()} or
#'     \code{call()}.  If \code{FALSE}, the returned object will be
#'     text.
#' @return Either an R parse tree or text, depending on the value of \code{toR}.
#' @export
nDeparse <- function(code,
                     indent = '',
                     toR = FALSE) {
  deparsed_text <- nDeparseToText(code, indent)
  if(toR)
    parse(text = deparsed_text,
          keep.source = FALSE)[[1]]
  else
    deparsed_text
}

### Deparse from exprClass back to R code:
## not guaranteed to be identical, but valid.
nDeparseToText <- function(code, indent = '') {
  if(!inherits(code, 'exprClass'))
    return(class(code))
  ## literals
  if(code$isLiteral) {
    if(is.numeric(code$name) |
         is.logical(code$name))
      return(code$name)
    if(is.character(code$name))
      return(paste0('\"', code$name, '\"'))
    if(is.null(code$name))
      return('NULL')
  }
  ## name
  if(code$isName)
    return(code$name)
  ## { : iterate through contents, deparsing and adding indentation
  if(code$name == '{') {
    ans <- vector('list', length(code$args) + 2)
    ans[[1]] <- paste0(indent, '{')
    for(i in seq_along(code$args)) {
      newInd <- paste0(indent, '    ')
      ans[[i+1]] <- addIndentToList(
        nDeparse(code$args[[i]], newInd),
        newInd)
    }
    ans[[length(code$args) + 2]] <- paste0(indent, '}')
    return(unlist(ans))
  }
  ## for: 
  if(code$name == 'for') {
    part1 <- paste0('for(',
                    paste(nDeparse(code$args[[1]]),
                          collapse = ""),
                    ' in ',
                    paste(nDeparse(code$args[[2]]),
                          collapse = ""),
                    ')')
    part2 <- paste(nDeparse(code$args[[3]])) ## body of loop
    ## If the body is not in a {, this is not consistent with deparse()
    return(paste(part1, part2))
  }
  ## if:
  if(code$name %in% ifOrWhile) {
    part1 <- paste0('if(', nDeparse(code$args[[1]]),')')
    ## body of 'then' or 'while' clause:
    part2 <- nDeparse(code$args[[2]])
    if(is.list(part2)) { ## If it is in a {, put { on same line as if()
      part2[[1]] <- paste(part1, part2[[1]])
      part2 <- addIndentToList(part2, indent)
    } else {
      ## If it is not in a {, put on same line as if()
      part2 <- paste(part1, part2)
    }
    ## If there is no 'else' clause, return
    if(length(code$args) == 2) return(part2) ## no else clause
    ## body of 'else' clause:
    part3 <- nDeparse(code$args[[3]])
    if(is.list(part3)) { ## If it is in {, merge lines to get '} else {'
      part3[[1]] <- paste(part2[[length(part2)]], 'else', part3[[1]])
      part3[-1] <- addIndentToList(part3[-1], indent)
      part3 <- c(part2[-length(part2)], part3)
      return(part3)
    } else {
      ## If it is not in {, merge lines to get '} else else-body'
      part2[[length(part2)]] <- paste(part2[[length(part2)]],
                                      'else',
                                      part3)
      return(part2)
    }
  }
  ## if(code$name == ":") {
  ##     return (paste0( nDeparse(code$args[[1]]),
  ##                    ':',
  ##                    nDeparse(code$args[[2]])))
  ## }
  ## For operators like '+' that go after the first argument:
  if(code$name %in% names(midOperatorsForDeparse)) {
    if(length(code$args) == 2) {
      if(is.null(code$caller)) useParens <- FALSE
      else {
        thisRank <- operatorRank[[code$name]]
        callingRank <- if(!is.null(code$caller))
                         operatorRank[[code$caller$name]]
        else
          NULL
        useParens <- FALSE ## default to FALSE
        if(!is.null(callingRank)) {
          if(!is.null(thisRank)) {
            if(callingRank < thisRank)
              useParens <- TRUE
          }
        }
      }
      if(useParens)
        return( paste0('(',
                       nDeparse(code$args[[1]]),
                       midOperatorsForDeparse[[code$name]],
                       paste0(
                         nDeparse_makeArgumentText(
                           lapply(code$args[-1], nDeparse)
                         ),
                         collapse = ', '),
                       ')' )
               )
      else
        return( paste0(nDeparse(code$args[[1]]),
                       midOperatorsForDeparse[[code$name]],
                       paste0(nDeparse_makeArgumentText(
                         lapply(code$args[-1], nDeparse)
                       ),
                       collapse = ', ') )
               )
      ## unary case will go to end, so -A will become -(A)
    }
  }
  ## for operators like '[' that go after the first argument and then have a closing part like ']'
  if(code$name %in% names(brackOperatorsForDeparse)) {
    return( paste0(nDeparse(code$args[[1]]),
                   brackOperatorsForDeparse[[code$name]][1],
                   paste0(
                     nDeparse_makeArgumentText(
                       lapply(code$args[-1], nDeparse)
                     ),
                     collapse = ', '),
                   brackOperatorsForDeparse[[code$name]][2] )
           )
  }
  if(code$name == '(') {
    return(paste0('(',
                  paste0(
                    nDeparse_makeArgumentText(
                      lapply(code$args, nDeparse)
                    ),
                    collapse = ', '),
                  ')')
           )
  }
  if(code$name == 'chainedCall') {
    return(paste0(nDeparse(code$args[[1]]),
                  '(',
                  paste0(
                    nDeparse_makeArgumentText(
                      lapply(code$args[-1], nDeparse)
                    ),
                    collapse = ', '),
                  ')' )
           )
  }
  ## for a general function call. Modified to preseve any argument names
  deparsedArguments <- lapply(code$args, nDeparse)
  argumentText <- nDeparse_makeArgumentText(deparsedArguments)
  return(paste0(code$name,
                '(',
                paste0(argumentText, collapse = ', '),
                ')' )
         )
}

nDeparse_makeArgumentText <- function(deparsedArguments) {
  if(!is.null(names(deparsedArguments)))
    paste0(ifelse(names(deparsedArguments) != "",
                  paste0(names(deparsedArguments), " = "),
                  ""),
           deparsedArguments
           )
  else
    unlist(deparsedArguments)
}
