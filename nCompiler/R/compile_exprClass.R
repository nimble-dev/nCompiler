## exprClass is a class for parse trees (expressions) that enhances
## the information in the regular R parse tree.  It makes it so we can
## navigate the parse tree more easily, keeping track of what we need, and
## not being awkward with the R parse tree all the time.  In particular, it has fields
## flagging whether the expression is a name, a call, and/or an assignment;
## it has information on the types, number of dimensions, and size expressions, when appropriate;
## it knows what exprClass object it is an argument to, and what number argument it is;
## and it has a field eigMatrix used for tracking matrix vs. array status of Eigen expressions
##
## In places it is still convenient to use R parse trees, or to generate
## code in an R parse tree and then call nParse to convert it
exprClass <- R6::R6Class(
    'exprClass',
    portable = FALSE,
    public = list(
        Rexpr = NULL, ## optional: original R expr. 
        isName = NULL, ## is it a name
        isCall = NULL, ## is it a call
        isAssign =  NULL, ## is it an assignment (all assignments are also calls)
        isLiteral = FALSE, ## is it a literal (like 2 or "hello")
        name =  NULL, ## name of call or variable, or contents of literal
        ##nDim =  NULL,
        ##sizeExprs =  list(),
        ##typeName =  NULL, ## type label
        type = NULL, ## symbol object
        args =  list(),	## list of exprClass objects for the arguments
        implementation = list(),
        ##eigMatrix =  logical(),
        ##toEigenize =  'unknown',
        caller = NULL, ## exprClass object to which this is an argument(or NULL)
        callerArgID =  NULL, ## index in the calling object's args list for this object.
        insertions =  list(), 
        cppADCode = FALSE, ## is expr in code generated for cppad?
        aux = list(), ## auxiliary list of additional info to be used as needed 
        initialize = function(...) {
            dotsList <- list(...)
            for(v in names(dotsList))
                self[[v]] <- dotsList[[v]]
            self
        },
        ## This displays the parse tree using indentation on multiple rows of output
        ## It also checks that the caller and callerArgID fields are all correct
        ## For deparsing, call nDeparse
        print = function(...) {
            exprClass_print(self, ...)
        })
)

exprClass_print <- function(AST,
                            indent = '',
                            showType = FALSE,
                            showImpl = FALSE,
                            argName = "") {
    ## output separator for annontations
    sepDisp <- character()
    if(showType | showImpl)
        sepDisp <- " | "

    ## output string for type
    typeDisp <- character()
    if(showType)
        if(!is.null(AST$type))
            typeDisp <- paste0(if(is.character(AST$type))
                                   AST$type
                               else
                                   AST$type$shortPrint())

    ## output string for implementation
    implDisp <- character()
    if(showImpl)
        if(!is.null(AST$implementation$toEigen))
            implDisp <- paste0("E(",
                               AST$implementation$toEigen, ## yes, no or maybe
                               ")"
                               )
    assertDisp <- character()
    writeLines(paste0(indent,
                      if(argName=="")
                          argName
                      else
                          paste0(argName,"="),
                      AST$name,
                      sepDisp,
                      typeDisp,
                      implDisp,
                      assertDisp)
               )
    ## Iterate through arguments and show them with more indenting
    args <- AST$args
    for(i in seq_along(args)) {
        if(inherits(args[[i]], 'exprClass')) {
            argNames <- names(args)
            if(is.null(argNames))
                argNames <- rep("", length(args))
            exprClass_print(args[[i]],
                            indent = paste0(indent, '  '),
                            showType,
                            showImpl,
                            argName = argNames[i]
                            )
            ## check caller and callerArgID validity
            if(!identical(args[[i]]$caller, AST) |
               args[[i]]$callerArgID != i)
                writeLines(
                    paste0(indent,
                           '****',
                           'Warning: caller and/or callerID ',
                           'are not set correctly.')
                )
        } else {
            writeLines(
                paste0(
                    indent,
                    '  ',
                    if(is.null(args[[i]]))
                        'NULL'
                    else
                        args[[i]])
            )
        }
    }
    ## close brackets
    if(AST$name=='{')
        writeLines(paste0(indent,'}'))
}


copyExprClass <- function(original) {
    result <- original$clone(deep = FALSE)
    ## shallow=FALSE does not deep-copy on list elements, so it is
    ## useless for args list.  Another reason for shallow = TRUE
    ## is we do not want to deep copy 'caller' here.  Instead it is
    ## re-assigned below.
    for(i in seq_along(result$args)) {
        if(inherits(result$args[[i]], 'exprClass')) {
            result$args[[i]] <- copyExprClass(result$args[[i]])
            result$args[[i]]$caller <- result
        }
    }
    result
}

## Add indendation to every character() element in a list, doing so recursively for any nested list()
addIndentToList <- function(x, indent) {
    if(is.list(x)) return(lapply(x, addIndentToList, indent))
    paste0(indent, x)
}

## error trapping utilities to be used from the various processing steps
exprClassProcessingErrorMsg <- function(code, msg) {
  deparsed_code <- try(nDeparse(code), silent = TRUE)
  ans <- paste0(
    msg, '\n This occured for',
    if (!inherits(deparsed_code, 'try-error'))
      paste0(': ', deparsed_code, collapse = '\n')
    else
      paste0(" the code beginning at '", code$name, "'."),
    '\n'
  )
  if (!is.null(code$caller)) {
    deparsed_caller <- try(nDeparse(code$caller), silent = TRUE)
    if (!inherits(deparsed_caller, 'try-error'))
      ans <- paste(
        ans, 'This was part of the call: ',
        paste(unlist(deparsed_caller), collapse = '\n')
      )
    else
      ans <- paste(
        ans, 'This was part of the call beginning at ',
        code$caller$name, "."
      )
  }
  ans
}


## some utilities

## return a list of size expressions with any "1"s dropped
## E.g. (dim(A)[1], 1, 2) returns (dim(A)[1], 2)
## This would be useful for determining e.g. if (dim(B)[1], 2) and (dim(A)[1], 1, 2) can be added as like types
## For now, the only case we use it for is determining is something can be treated like a scalar, e.g. with sizes of 1 in every dimension
dropSingleSizes <- function(sizeList) {
    drop <- logical(length(sizeList))
    for(i in seq_along(sizeList)) {
        drop[i] <- if(is.numeric(sizeList[[i]])) sizeList[[i]] == 1 else FALSE 
    }
    sizeList[drop] <- NULL
    list(sizeExprs = sizeList, drop = drop)
}

## Often we have an expr foo(a, b) and we want to make it foo(a, g(b))
## We do that by insertExprClassLayer(code, 2, 'g'), where code is the 'foo' exprClass.
## It could be convenient to make it easier to insert additional arguments to g.
## One can always do so with setArg after using insertExpressionClasLayer
insertExprClassLayer <- function(code, argID, funName, isName = FALSE, isCall = TRUE, isAssign = FALSE, ... ) {
    newExpr <- exprClass$new(name = funName, isName = isName, isCall = isCall, isAssign = isAssign,
                             args = list(code$args[[argID]]), ...)
    setCaller(code$args[[argID]], newExpr, 1)
    setArg(code, argID, newExpr)
    newExpr
}

insertIndexingBracket <- function(code, argID, index) {
    insertExprClassLayer(code, argID, '[')
    setArg(code$args[[argID]], 2, index)
}

## make a new bracket { expression in exprClass objects
newBracketExpr <- function(args = list()) {
    ans <- exprClass$new(isCall = TRUE, isName = FALSE, isAssign = FALSE,
                  name = '{', args = args)
    for(i in seq_along(args)) {
        setCaller(ans$args[[i]], ans, i)
    }
    ans
}

removeExprClassLayer <- function(code, argID = 1) {
    setArg(code$caller, code$callerArgID, if(length(code$args) >= argID) code$args[[argID]] else NULL)
}

setCaller <- function(value, expr, ID) {
    value$caller <- expr
    value$callerArgID <- ID
    invisible(value)
}

setArg <- function(expr, ID, value, add = FALSE) {
  arg_names <- names(expr$args)
  expr$args[[ID]] <- value
  if(inherits(value, 'exprClass')) {
    if (is.character(ID)) {
      arg_name <- ID
      ID <- which(arg_names == arg_name)
      if (length(ID) == 0) {
        if (isTRUE(add))
          ID <- length(expr$args) ## add to end
        else {
          ## remove the arg from the AST since added at beginning
          expr$args[[arg_name]] <- NULL
          stop(
            exprClassProcessingErrorMsg(
              expr,
              paste0(
                "Attempted to set '", arg_name, "' as an argument of '",
                expr$name, "' by name, but '", arg_name, "' was not found ",
                "in the list of arguments. To add a new named argument to ",
                "the end of the argument list, use 'add = TRUE'."
              )
            ), call. = FALSE
          )
        }
      } else if (length(ID) > 1)
        stop(
          exprClassProcessingErrorMsg(
            expr,
            paste0(
              "Attempted to set '", arg_name, "' as an argument of '",
              expr$name, "' by name, but it appears multiple times in the ",
              "list of arguments."
            )
          ), call. = FALSE
        )
    }
    setCaller(value, expr, ID)
  }
  invisible(value)
}

checkArgDims <- function(expr, ID, nDimRange) {
  if (expr$args[[ID]]$type$nDim < nDimRange[1] ||
      expr$args[[ID]]$type$nDim > nDimRange[2])
    stop(
      exprClassProcessingErrorMsg(
        expr, 'An argument does not have the appropriate dimensions.'
      ),
      call. = FALSE
    )
}

isEigScalar <- function(code) {
  code$isLiteral || (code$isName && identical(code$type$nDim, 0))
}

newAssignmentExpression <- function() {
    exprClass$new(isName = FALSE, isCall = TRUE, isAssign = TRUE, name = '<-')
}

literalDoubleExpr <- function(value) {
  type <- symbolBasic$new(name = 'NONAME',
                          type = 'double',
                          nDim = 0)
  exprClass$new(isName = FALSE,
                isCall = FALSE,
                isLiteral = TRUE,
                name = value,
                type = type)
}

literalIntegerExpr <- function(value) {
  type <- symbolBasic$new(name = 'NONAME',
                          type = 'integer',
                          nDim = 0)
  exprClass$new(isName = FALSE,
                isCall = FALSE,
                isLiteral = TRUE,
                name = value,
                type = type)
}

literalLogicalExpr <- function(value = TRUE) {
  type <- symbolBasic$new(name = 'NONAME',
                          type = 'logical',
                          nDim = 0)
  exprClass$new(isName = FALSE,
                isCall = FALSE,
                isLiteral = TRUE,
                name = value,
                type = type)
}

## This modifies the code$caller in place
## and generates the temp expr
buildSimpleIntermCall <- function(code) {
    if(code$caller$isAssign) return(NULL)
    newName <- IntermLabelMaker()

    ## change my argument from the caller to the new temp
    setArg(code$caller, code$callerArgID, nParse(as.name(newName)))
    
    newExpr <- newAssignmentExpression()
    setArg(newExpr, 1, nParse(as.name(newName))) 
    setArg(newExpr, 2, code) ## The setArg function should set code$caller (to newExpr) and code$callerArgID (to 3)

    return(newExpr)
}

isCodeScalar <- function(code) {
    for(i in seq_along(code$sizeExprs))
        if(!identical(code$sizeExprs[[i]], 1)) return(FALSE)
    TRUE
}

anyNonScalar <- function(code) {
    if(!inherits(code, 'exprClass')) return(FALSE)
    if(code$name == 'map') return(TRUE)
    if(is.character(code$type))
        if(code$type[1] == 'nCompilerList') return(FALSE)
    if(code$isName) {
        return(!isCodeScalar(code))
    }
    if(code$isCall) {
        if(code$name == 'nfVar') ## don't recurse just for nested member access
            return(!isCodeScalar(code))
        skipFirst <- FALSE
        if(code$name == '[') skipFirst <- TRUE
        if(code$name == 'size') skipFirst <- TRUE
        for(i in seq_along(code$args)) {
            if(!(skipFirst & i == 1)) {
                if(anyNonScalar(code$args[[i]])) return(TRUE)
            }
        }
    }
    FALSE
}
