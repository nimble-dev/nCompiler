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
      if(!identical(args[[i]]$caller, AST) ||
           !isTRUE(args[[i]]$callerArgID == i))
        writeLines(
          paste0(indent,
                 '****',
                 'Warning: caller and/or callerArgID ',
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

wrapInExprClass <- function(code, funName, argName=NULL) {
  caller <- code$caller
  callerArgID <- code$callerArgID
  newExpr <- exprClass$new(name=funName, isName=FALSE, isCall=TRUE, isAssign=FALSE)
  argID <- if(is.null(argName)) 1 else argName
  setArg(newExpr, argID, code, add=TRUE)
  setArg(caller, callerArgID, newExpr)
  newExpr
}

## Sometimes we have an expr foo(a, b) and we want to make it g(foo(a, b))
## We do that by wrapExprClassOperator(code, 'g'), where code is the 'foo' expClass.
wrapExprClassOperator <- function(code, funName, isName = FALSE, isCall = TRUE, 
                                  isAssign = FALSE, ...) {
  newExpr <- exprClass$new(name = funName, isName = isName, isCall = isCall, 
                           isAssign = isAssign, args = list(code), ...)  
  if(!is.null(code$caller)) {
    setArg(code$caller, code$callerArgID, newExpr)
  }
  setCaller(code, newExpr, 1)
  newExpr
}

insertIndexingBracket <- function(code, argID, index) {
  insertExprClassLayer(code, argID, 'index[')
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
  if (is.character(argID)) {
    arg_name <- argID
    argID <- which(names(code$args) == arg_name)
    if(length(argID)!=1)
      stop(exprClassProcessingErrorMsg(
        code,
        paste0(
          "Could not find (unique) argument named ", arg_name, " to remove from syntax tree.")),
        call. = FALSE)
  }
  setArg(code$caller, code$callerArgID, if(length(code$args) >= argID) code$args[[argID]] else NULL)
}

setCaller <- function(value, expr, ID) {
  value$caller <- expr
  value$callerArgID <- ID
  invisible(value)
}

insertArg <- function(expr, ID, value, name = NULL) {
  origNumArgs <- length(expr$args)
  if(ID > origNumArgs + 1)
    stop(exprClassProcessingErrorMsg(
      expr,
      paste0(
        "Attempted to insert an argument with ID = ", ID, " but that is too large. ",
        "There are only ", origNumArgs, " arguments.")),
      call. = FALSE)

  argsToShift <- origNumArgs - ID + 1
  if(argsToShift) {
    for(i in (origNumArgs):(ID)) { ## a singleton or downward sequence
      setArg(expr, i+1, expr$args[[i]])
      names(expr$args)[i+1] <- names(expr$args)[i]
    }
  }
  setArg(expr, ID, value)
  if(is.null(name))
    if(!is.null(names(expr$args)))
      name <- ""
  if(!is.null(name))
    names(expr$args)[ID] <- name
  invisible(value)
}

replaceArgInCaller <- function(expr, value) {
  if(!inherits(expr$caller, 'exprClass')) {
    stop("Problem replacing a call in replaceArgInCaller.")
  }
  setArg(expr$caller, expr$callerArgID, value)
}

setArg <- function(expr, ID, value, add = FALSE) {
  arg_names <- names(expr$args)
  expr$args[ID] <- list(value)
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

removeArg <- function(expr, ID, allow_missing = FALSE) {
  origNumArgs <- length(expr$args)
  if(is.character(ID)) {
    numID <- which(ID == names(expr$args))
    if(length(numID)!=1) {
      if(!allow_missing) {
        stop(paste0("Problem in removeArg for ID=",ID,". That arg name was not found."))
      } else return(invisible(NULL))
    }
    ID <- numID
  }
  if(ID > origNumArgs) {
    if(!allow_missing) {
      stop(exprClassProcessingErrorMsg(
        expr,
        paste0(
          "Attempted to remove an argument with ID = ", ID, " but that is too large. ",
          "There are only ", origNumArgs, " arguments.")),
        call. = FALSE)
    } else return(invisible(NULL))
  }
  value <- expr$args[[ID]]
  argsToShift <- origNumArgs - ID
  if(argsToShift) {
    for(i in (ID + 1):origNumArgs) {
      setArg(expr, i - 1, expr$args[[i]])
      names(expr$args)[i-1] <- names(expr$args)[i]
    }
  }
  ## Either the arg to remove was the last arg in the AST or the last arg is
  ## now duplicated, appearing in the last two positions.
  expr$args[[origNumArgs]] <- NULL
  invisible(value)
}

reorderArgs <- function(expr, perm) {
  # Change the order of an exprClass object's arguments.  Useful for placing
  # arguments into canonical order for dispatch via associated C calls.
  # 
  # Parameters:
  #  expr - exprClass object whose arguments should be reordered
  #  perm - vector of integers indicating where each argument should be moved to
  
  # extract arguments
  args <- expr$args
  argNames <- names(args)
  nArgs <- length(args)
  
  # temporarily remove arguments from expr
  for(i in 1:nArgs) {
    removeArg(expr = expr, ID = 1)
  }

  # insert arguments in new order
  #   Note: use insertArg vs. setArg b/c insertArg lets us rename the arguments
  #         in addition to changing the argument values
  for(i in 1:nArgs) {
    # figure out which argument should be in the i^th position after reordering
    tgt <- which(perm == i)
    # add that argument
    insertArg(expr = expr, ID = i, value = args[[tgt]], name = argNames[tgt])
  }
  
  invisible(expr)
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

isEigScalar <- function(code, allowIndexing = FALSE) {
  code$isLiteral || ((if(allowIndexing) TRUE else code$isName) && identical(code$type$nDim, 0))
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

exprClass_match_call <- function(def, expr) {
  # This is a version of match.call where def is an R function definition (like for match.call)
  # or a pairlist, and expr is an exprClass, returned by nParse.
  # However, the returned object contains argument (re-)ordering information only.
  #
  # The strategy will be to create something like
  # match.call(function(a , b , c ){}, call("foo", b = 1, 2, 3))
  # resulting in foo(a = 2, b = 1, c = 3),
  # from which we can see that the expr arguments need to be re-orded by c(2, 1, 3).
  # The object foo(a = 2, b = 1, c = 3) is returned, so that exprClass_put_args_in_order 
  #   can do the further work needed using the ordering.
  # We can also see what expected arguments are missing and store that information
  #  for potential later handling.
  # The sequential values 1, 2, 3 in the artificial call will reveal argument permutations needed.
  # An error will reveal bad argument matching.
  # The call part will actually take the form do.call("call", list of "foo" and args)

  # def can be a function (used for its formals only) or a pairlist
  # If it is a pairlist, make it the formals of a function
  if(is.pairlist(def)) {
    foo <- function() {}
    formals(foo) <- def
    def <- foo
  }
  # Make list of artificial arg values starting from 1,  with provided names
  exprArgs <- structure( as.list(seq_along(expr$args)), names = names(expr$args) )
  result <- try(match.call(def, do.call("call", c(list("foo"), exprArgs ))))
  if(inherits(result, "try-error"))
    stop("error in matching arguments for ", expr$name)
  result
}

exprClass_put_args_in_order <- function(def, expr, 
                                compileArgs = NULL, insertDefaults = TRUE) {
  match_res <- exprClass_match_call(def, expr)
  # there is a function reorderArgs above which appears to have been written
  # for eigenization of nDiag.
  # However its behavior is slightly different than needed here, so we don't use it.
  args <- expr$args
  expr$args <- NULL
  for(i in seq_along(match_res)) {
    if(i==1) next # "foo"
    insertArg(expr = expr, ID=i-1, value = args[[match_res[[i]] ]],
              name = names(match_res)[i] )
  }
  formals_def <- formals(def)
  missing_names <- setdiff(names(formals_def), names(match_res)[-1] )
  expr$aux[["provided_as_missing"]] <- missing_names
  expr$aux[["missing"]] <- missing_names
  # match.call DOES NOT insert defaults for missing arguments,
  # but we want to do so.
  if(insertDefaults) {
    new_missing_names <- character()
    for(mname in missing_names) {
      if(!is.blank(formals_def[[mname]])) {
        i <- which(mname == names(formals_def))
        if(i > length(expr$args))
          i <- length(expr$args)+1
        insertArg(expr, i,
                  value = nParse(formals_def[[mname]], recursing=TRUE), # recursing=TRUE prevents parsing if the default is a string
                  name = mname)
      } else {
        new_missing_names <- c(new_missing_names, mname)
      }
    }
    expr$aux[["missing"]] <- new_missing_names
  }
  # separate compile-time arguments.
  # This is done AFTER inserting defaults, so that compile-time args can have defaults.
  # The nParse-ing of compileTime args was superfluous, so we throw it out in this step.
  if(length(compileArgs)>0) {
    aux_compileArgs <- list()
    iRes <- 1
    for(CA_name in compileArgs) {
      if(CA_name %in% names(expr$args)) {
        aux_compileArgs[[iRes]] <- expr$args[[CA_name]]$Rexpr
        names(aux_compileArgs)[iRes] <- CA_name
        iRes <- iRes + 1
        removeArg(expr, CA_name)
      }
    }
    expr$aux[["compileArgs"]] <- aux_compileArgs
  }
  expr
}

## expr <- nParse(quote(foo(a = 1, NULL)))
## def <- \(a, b, c = 1) {}
## exprClass_match_call(def, expr)
## test <- exprClass_put_args_in_order(def, expr)
## test
## test$aux
