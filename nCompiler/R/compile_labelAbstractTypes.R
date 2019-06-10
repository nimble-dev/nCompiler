
labelAbstractTypesEnv <- new.env()
labelAbstractTypesEnv$.debug <- FALSE

compile_labelAbstractTypes <- function(code,
                                       symTab,
                                       auxEnv) { 
    nErrorEnv$stateInfo <- paste0("handling labelAbstractTypes for ",
                                       code$name,
                                       ".")

    logging <- get_nOption('compilerOptions')[['logging']]
    if (logging) appendToLog(paste('###', nErrorEnv$stateInfo, '###'))
    
    if(code$isLiteral) {
      if(is.numeric(code$name)) {
        if(is.integer(code$name)) {
          code$type <- symbolBasic$new(name = 'NONAME',
                                       type = 'integer',
                                       nDim = 0)
        } else {
          code$type <- symbolBasic$new(name = 'NONAME',
                                       type = 'double',
                                       nDim = 0)
        } 
      } else if(is.logical(code$name)) {
        code$type <- symbolBasic$new(name = 'NONAME',
                                     type = 'logical',
                                     nDim = 0)
      } else if(is.character(code$name)) {
        warnings("Type labeling of a literal string is not handled yet in labelAbstractTypes.")
      }
    }
    
    if(code$isName) {
        ## If it doesn't exist and must exist, stop
        if(code$name != "") { ## e.g. In A[i,], second index gives name==""
            if(symTab$symbolExists(code$name, TRUE)) {
                thisSymbolObject <- symTab$getSymbol(code$name, TRUE)
                ##code$typeName <- class(thisSymbolObject)[1]
                code$type <- thisSymbolObject
            } else {
                if(!auxEnv$.AllowUnknowns)
                    if(identical(code$name, 'pi')) {
                        ## unique because it may be encountered anew on a RHS
                        ## and be valid as a new variable name, or on the LHS
                        ## and be a constant if it's not yet a variable
                        newSymbol <- symbolBasic$new(name = 'pi',
                                                     type = 'double',
                                                     nDim = 0)
                        symTab$addSymbol(newSymbol)
                        code$type <- newSymbol
                        code$nDim <- 0
                        ##code$typeName <- 'double'
                    } else {
                        stop(paste0("variable '",
                                       code$name,
                                       "' has not been created yet."),
                                call.=FALSE) 
                    }
            }

            ## TO DO:
            if(FALSE) {
                ## Add RCfunctions to neededRCfuns.
                if(exists(code$name) && is.rcf(get(code$name))) {
                    nfmObj <- environment(get(code$name))$nfMethodRCobject
                    uniqueName <- nfmObj$uniqueName
                    if (is.null(auxEnv$needed_nFunctions[[uniqueName]])) {
                        auxEnv$needed_nFunctions[[uniqueName]] <- nfmObj
                    }
                }
            }
            ## Note that generation of a symbol for LHS of an assignment is done in the sizeAssign function, which is the handler for assignments
            return(NULL)
        }
    }

    if(code$isCall) {
        if(code$name == '{') {
            ## recurse over lines
            for(i in seq_along(code$args)) {
                newInsertions <-
                    compile_labelAbstractTypes(code$args[[i]], symTab, auxEnv)
                code$args[[i]]$insertions <-
                    if(is.null(newInsertions)) list() else newInsertions
            }
            return(invisible(NULL))
        }

        opInfo <- operatorDefEnv[[code$name]]
        ## TO-DO: Check for methods or nFunctions.
        if(is.null(opInfo) && exists(code$name, envir = auxEnv$closure)) {
          ## An nFunction should already have been transformed to
          ## have code$name nFunction in stage simpleTransformatnions.
          ## But if not (if a custom handler was provided that avoided that change),
          ## it will still be caught here.
          obj <- get(code$name, envir = auxEnv$closure)
          if(isNF(obj)) {
            opInfo <- operatorDefEnv[['nFunction']]
          }
        }
        
        if(!is.null(opInfo)) {
            handlingInfo <- opInfo[["labelAbstractTypes"]]
            if(!is.null(handlingInfo)) {
                handler <- handlingInfo[['handler']]
                if(!is.null(handler)) {
                    if (logging)
                      appendToLog(paste('Calling handler', handler, 'for', code$name))
                    ans <- eval(call(handler, code, symTab, auxEnv, handlingInfo),
                                envir = labelAbstractTypesEnv)
                    nErrorEnv$stateInfo <- character()
                    if (logging) {
                      appendToLog(paste('Finished handling', handler, 'for', code$name))
                      logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
                    }
                    return(ans)
                }
            }
        }
    }
    nErrorEnv$stateInfo <- character()
    invisible(NULL)
}

inLabelAbstractTypesEnv <- function(expr) {
    expr <- substitute(expr)
    eval(expr, envir = labelAbstractTypesEnv)
}

inLabelAbstractTypesEnv(
    setReturnType <- function(handlingInfo, argType) {
        returnTypeCode <- handlingInfo[['returnTypeCode']]
        if(is.null(returnTypeCode)) return('double')
        switch(returnTypeCode,
               'double', ##1
               'integer', ##2
               'logical', ##3
               argType, ##4
               if(argType == 'logical') 'integer' else argType ##5
               )
    }
)

inLabelAbstractTypesEnv(
  Generic_nFunction <-
    function(code, symTab, auxEnv, handlingInfo) {
      inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv,
                                            handlingInfo)
        obj <- get(code$aux$nFunctionInfo$nFunctionName,
                   envir = code$aux$nFunctionInfo$where)
        ## code$aux$nFunctionInfo$where should be same as auxEnv$closure
        
        ## TO-DO: Add error-trapping of argument types 
        returnSym <- NFinternals(obj)$returnSym
        if(is.null(returnSym))
          stop(
            exprClassProcessingErrorMsg(
              code, paste('In Generic_nFunction: the nFunction ', code$name, 
                          ' does not have a valid returnType.')
            ), call. = FALSE
          )
        code$type <- returnSym$clone() ## Not sure if a clone is needed, but it seems safer to make one.
        if(length(inserts) == 0) NULL else inserts
    }
)

inLabelAbstractTypesEnv(
    Assign <- 
        function(code, symTab, auxEnv, handlingInfo) {
            auxEnv$.AllowUnknowns <- FALSE
            inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv,
                                                  handlingInfo,
                                                  useArgs = c(FALSE, TRUE))
            auxEnv$.AllowUnknowns <- TRUE
            if(length(code$args) > 2) {
                inserts <- c(inserts,
                             compile_labelAbstractTypes(code, symTab, auxEnv))
            }
            else{
                inserts <- c(inserts,
                             recurse_labelAbstractTypes(code, symTab, auxEnv,
                                                        handlingInfo, useArgs = c(TRUE, FALSE)))
                auxEnv[['.ensureNimbleBlocks']] <- FALSE ## may have been true from RHS of rmnorm etc.
                inserts <- c(inserts,
                             AssignAfterRecursing(code, symTab, auxEnv,
                                                  handlingInfo))
            }
            if(length(inserts) == 0) NULL else inserts
        }
)

inLabelAbstractTypesEnv(
    AssignAfterRecursing <- 
        function(code, symTab, auxEnv, handlingInfo) {
            LHS <- code$args[[1]]
            RHS <- code$args[[2]]
            RHStype <- RHS$type
            if(LHS$isName) {
                if(!symTab$symbolExists(LHS$name, TRUE)) {
                    newSym <- RHStype$clone()
                    newSym$isArg <- FALSE
                    newSym$name <- LHS$name
                    symTab$addSymbol(newSym)
                    LHS$type <- newSym
                    code$type <- newSym
                }
            }
            NULL
        }
)

inLabelAbstractTypesEnv(
  For <- function(code, symTab, auxEnv, handlingInfo) {
    if(length(code$args) != 3)
      stop(paste('Error in labelAbstractTypes handler For:',
                 'expected 3 arguments to a for-loop'), call. = FALSE)
    ## first handle type of the indexing variable
    if(!inherits(code$args[[2]], 'exprClass'))
      stop(
        exprClassProcessingErrorMsg(
          code, paste('In sizeFor: expected the index',
                      'range to be an expression (exprClass).')
        ), call. = FALSE
      )

    inserts <- compile_labelAbstractTypes(code$args[[2]], symTab, auxEnv)

    code$args[[1]]$type <-
      symbolBasic$new(name = code$args[[1]]$name,
                      nDim = 0, type = code$args[[2]]$type$type)

    ## code$args[[1]]$sizeExprs <- list()
    ## code$args[[1]]$toEigenize <- 'no'

    ## If index is unknown, create it in typeEnv and in the symTab (old nimble comment)
    if (!symTab$symbolExists(code$args[[1]]$name, inherits = TRUE))
      if (TRUE) ##!auxEnv$.AllowUnknowns)
        symTab$addSymbol(code$args[[1]]$type)
    ## auxEnv[[code$args[[1]]$name]]$sizeExprs <- list()

    ## Now the 3rd arg, the body of the loop, can be processed
    inserts <- c(inserts, compile_labelAbstractTypes(code$args[[3]], symTab, auxEnv))
    ## I think there shouldn't be any inserts returned since the body should be a bracket expression.
    return(if (length(inserts) == 0) invisible(NULL) else inserts)
  }
)

inLabelAbstractTypesEnv(
  Colon <- function(code, symTab, auxEnv, handlingInfo, recurse = TRUE) {
    inserts <-
      if (recurse)
        recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
      else list()

    if (length(code$args) != 2)
      stop(
        exprClassProcessingErrorMsg(
          code, paste(
                  'In sizeColonOperator: Problem determining',
                  'size for : without two arguments.'
                )
        ), call. = FALSE
      )

    for (i in 1:2) {
        if (inherits(code$args[[i]], 'exprClass')) {
            if (!code$args[[i]]$isName) {
              if (!(code$args[[i]]$name == '[' &&
                    (code$args[[i]]$args[[1]]$name == 'dim' &&
                     code$args[[i]]$args[[1]]$args[[1]]$name == 'nfVar'))) {
                inserts <- c(
                  inserts ##, sizeInsertIntermediate(code, i, symTab, typeEnv)
                )
              }
            }
        }
    }

    code$type <- symbolBasic$new(nDim = 1, type = 'double')

    ## could generate an assertion that second arg is >= first arg
    if(is.numeric(code$args[[1]]) & is.numeric(code$args[[2]])) {
      ## do we need to annotate code$type$size here?
      ## code$sizeExprs <- list(code$args[[2]] - code$args[[1]] + 1)
    } else { ## at least one part is an expression
        ## This is an awkward case:
        ## sizeExprs are R parse trees, not exprClasses
        ## But in this case, we want the expression from an exprClass.
        ## so we need to nimDeparse and then parse them
        ## code$sizeExprs <- list(substitute( A - B + 1, list(A = parse(text = nimDeparse(code$args[[2]]), keep.source = FALSE)[[1]], B = parse(text = nimDeparse(code$args[[1]]), keep.source = FALSE)[[1]] ) ) )
    }
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
    recurse_labelAbstractTypes <- 
        function(code, symTab, auxEnv, handlingInfo,
                 useArgs = rep(TRUE, length(code$args))) {
            ## won't be here unless code is a call.  It will not be a {
            inserts <- list()
            for(i in seq_along(code$args)) {
                if(useArgs[i]) {
                    if(inherits(code$args[[i]], 'exprClass')) {
                        inserts <- c(inserts,
                                     compile_labelAbstractTypes(code$args[[i]],
                                                                symTab,
                                                                auxEnv))
                    }
                }
            }
            if(length(inserts)==0) NULL else inserts
        }
)

inLabelAbstractTypesEnv(
    BinaryUnaryCwise <- 
        function(code, symTab, auxEnv, handlingInfo) {
            if(length(code$args) == 1)
                return(UnaryCwise(code, symTab, auxEnv, handlingInfo))
            if(length(code$args) == 2)
                return(BinaryCwise(code, symTab, auxEnv, handlingInfo))
            stop(exprClassProcessingErrorMsg(
                code,
                paste0('In sizeBinaryUnarycWise: Length of arguments is not 1 or 2.')),
                call. = FALSE)
        }
)

## Handler for unary component-wise operators
inLabelAbstractTypesEnv(
    UnaryCwise <-
        function(code, symTab, auxEnv, handlingInfo) {
            if(length(code$args) != 1)
                stop(exprClassProcessingErrorMsg(
                    code,
                    'sizeUnaryCwise called with argument length != 1.'
                ),
                call. = FALSE)

            inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)

            ## pull out the argument
            arg <- code$args[[1]]

            argType <- arg$type
            resultScalarType <- arithmeticOutputType(
              argType$type, returnTypeCode = handlingInfo$returnTypeCode
            )
            resultType <- symbolBasic$new(nDim = argType$nDim,
                                          type = resultScalarType)
            code$type <- resultType
            invisible(NULL)

        }
)

## Handler for binary component-wise operators
inLabelAbstractTypesEnv(
    BinaryCwise <- 
        function(code, symTab, auxEnv, handlingInfo) {
            if(length(code$args) != 2)
                stop(exprClassProcessingErrorMsg(
                    code,
                    'sizeBinaryCwise called with argument length != 2.'
                ),
                call. = FALSE)

            inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
            ## sizes of arguments must have already been set

            ## pull out the two arguments
            a1 <- code$args[[1]]
            a2 <- code$args[[2]]

            a1Type <- a1$type
            a2Type <- a2$type

            nDim <- max(a1Type$nDim, a2Type$nDim)
            resultScalarType <- arithmeticOutputType(
              a1Type$type, a2Type$type, handlingInfo$returnTypeCode
            )
            resultType <- symbolBasic$new(nDim = nDim,
                                          type = resultScalarType)
            code$type <- resultType
            ##code$typeName <- class(resultType)[1]
            invisible(NULL)
            ## 
        }
)

inLabelAbstractTypesEnv(
    BinaryCwiseLogical <- 
        function(code, symTab, auxEnv, handlingInfo) {
            ans <- BinaryCwise(code, symTab, auxEnv, handlingInfo)
            code$type$type <- 'logical'
            ans
        }
)

inLabelAbstractTypesEnv(
    UnaryReduction <-
        function(code, symTab, auxEnv, handlingInfo) {
            if(length(code$args) != 1)
                stop(exprClassProcessingErrorMsg(
                    code,
                    'unaryReduction called with argument length != 1.'
                ),
                call. = FALSE)
            
            inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
            
            ## Kludgy catch of var case here.
            ## Can't do var(matrix) because in R that is interpreted as cov(data.frame)
            if(!(code$args[[1]]$isLiteral)) {
                if(code$args[[1]]$type$nDim >= 2) {
                    if(code$name == 'var') {
                        stop(exprClassProcessingErrorMsg(
                            code,
                            'nCompiler compiler does not support var with a matrix (or higher dimensional) argument.'),
                            call. = FALSE) 
                    }
                }
            }
            argType <- code$args[[1]]$type
            code$type <- symbolBasic$new(nDim = 0,
                                         type = setReturnType(handlingInfo, argType$type))
            inserts
        }
)

inLabelAbstractTypesEnv(
  Distribution <- function(code, symTab, auxEnv, handlingInfo) {
    code$type <- symbolBasic$new(nDim = 1, type = setReturnType(handlingInfo))
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  IndexingBracket <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)

    ## drop must be named if provided, so this should work
    drop_arg <- code$args$drop
    code$args$drop <- NULL ## remove from AST

    ## the indexed object should be the first arg among those other than drop
    obj <- code$args[[1]]
    index_args <- code$args[-1]
    nDim <- obj$type$nDim

    code$args <- NULL ## reset args

    brackets_empty <- length(index_args) == 1 &&
      index_args[[1]]$isName &&
      index_args[[1]]$name == ""

    if (brackets_empty) {
      ## no indexing is happening, so just replace [ with the obj in the AST
      ## and return
      setArg(code$caller, code$callerArgID, obj)
      return(invisible(NULL))
    }

    ## at this point, the indexing args are not empty
    if (nDim == 0) {
      ## indexed object is a scalar so there should be at most 1 indexing arg
      if (length(index_args) != 1)
        stop(
          exprClassProcessingErrorMsg(
            code,
            paste0(
              "In IndexingBracket: '", obj$name,
              "' is a scalar; expected at most 1 indexing arg but received ",
              length(index_args), "."
            )
          ), call. = FALSE
        )
    } else if (length(index_args) != nDim) {
      ## indexed object is not scalar, the indexing args are not empty,
      ## and so their number should be equal to the obj dimension
      stop(
        exprClassProcessingErrorMsg(
          code,
          paste0(
            "In IndexingBracket: number of indexing arguments does not match the dimension of '",
            obj$name, "'; expected ", nDim, " but received ", length(index_args), "."
          )
        ), call. = FALSE
      )
    }

    setArg(code, 1, obj) ## put indexed object back as first arg

    nDrop <- 0
    for (i in seq_along(index_args)) {
      ## ensure that indexing args appear before drop in AST
      setArg(code, i + 1, index_args[[i]])

      ## do a bunch of indexing arg error checking
      if (index_args[[i]]$isCall)
        ## for now, can't handle indexing args of nDim > 0 other than those
        ## created via ':'
        ## TODO: allow for (more) general expressions
        if (index_args[[i]]$name != ':' && index_args[[i]]$type$nDim != 0)
          stop(
            exprClassProcessingErrorMsg(
              code,
              "In IndexingBracket: non-scalar indexing expressions other than ':' not currently supported."
            ), call. = FALSE
          )

      if (index_args[[i]]$name != '') {
        ## not a call resulting in non-scalar other than ':'
        if (is.null(index_args[[i]]$type) || ## missing index nDim info
            is.null(index_args[[i]]$type$nDim)) ## would this ever happen?
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0("In IndexingBracket: '", index_args[[i]]$name,
                     "' has no dimension.")
            ), call. = FALSE
          )
        ## TODO: allow for scalar logicals?
        if (index_args[[i]]$type$type == 'logical') ## index logical
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0("In IndexingBracket: '", index_args[[i]]$name,
                     "' is a logical which is not allowed when indexing.")
            ), call. = FALSE
          )
        if (index_args[[i]]$type$nDim > 1) ## bad index nDim
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0(
                "In IndexingBracket: the dimension of '", index_args[[i]]$name,
                " is ", index_args[[i]]$type$nDim, " but must be 0 or 1."
              )
            ), call. = FALSE
          )
        if (nDim == 0 && index_args[[i]]$type$nDim != 0) ## indexing a scalar with non-scalar
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0(
                "In IndexingBracket: '", obj$name,
                "' is a scalar but the indexing arg has dimension ",
                index_args[[i]]$type$nDim, "."
              )
            ), call. = FALSE
          )
        ## no errors were triggered so increment nDrop if the arg is scalar
        if (index_args[[i]]$type$nDim == 0) nDrop <- nDrop + 1
      }
    }

    drop <- TRUE
    if (nDim == 0) {
      ## if we're indexing a scalar, just ignore the drop arg
      drop <- FALSE
    } else if (inherits(drop_arg, 'exprClass')) {
      if (drop_arg$isLiteral) {
        ## if the user provided a literal NA or NaN drop arg and even when drop
        ## is passed in explicity as NA or NaN R treats it as TRUE
        if (is.na(drop_arg$name) || is.nan(drop_arg$name)) {
          drop_arg <- literalLogicalExpr()
        } else if (is.null(drop_arg$type) || drop_arg$type$type != 'logical') {
          drop <- as.logical(drop_arg$name)
          drop_arg <- literalLogicalExpr(drop)
        } else { ## drop is logical
          drop <- drop_arg$name
        }
      } else {
        ## TODO: what if user provided a vector? R would use first element...
        stop(
          exprClassProcessingErrorMsg(
            code,
            'In IndexingBracket: the drop argument must be a literal.'
          ), call. = FALSE
        )
      }
    } else if (is.null(drop_arg)) { ## drop arg wasn't provided
      drop_arg <- literalLogicalExpr()
    }

    if (isTRUE(drop)) {
      nDim <- nDim - nDrop
    }

    if (nDim != 0) {
      ## set 'drop' as the last arg in the AST
      setArg(code, 'drop', drop_arg)
    }
    
    code$type <- symbolBasic$new(nDim = nDim, type = obj$type$type)
    invisible(NULL)
  }
)

inLabelAbstractTypesEnv(
  Literal <- function(code, symTab, auxEnv, handlingInfo) {
    if (length(code$args) > 2)
      stop(exprClassProcessingErrorMsg(
        code,
        'cppLiteral has argument length > 2.'
      ),
      call. = FALSE)
    if (length(code$args) == 2) {
      ## Add types that the user specified in their literal C++ code to symTab
      type_list <- lapply(code$args[[2]]$args, `[[`, 'name')
      symbols <- argTypeList2symbolTable(type_list)$getSymbols()
      for (sym in symbols) symTab$addSymbol(sym)
    }
    invisible(NULL)
  }
)

inLabelAbstractTypesEnv(
    Return <- 
        function(code, symTab, auxEnv, handlingInfo) {
            if(length(code$args) > 1)
                stop(exprClassProcessingErrorMsg(
                    code,
                    'return has argument length > 1.'
                ),
                call. = FALSE)
            if(!exists('return', envir = auxEnv))
                stop(exprClassProcessingErrorMsg(
                    code,
                    'There was no returnType declaration and the default is missing.'
                ),
                call. = FALSE)
            insertions <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
            code$type <- code$args[[1]]$type
            invisible(insertions)
        }
)

sizeProxyForDebugging <- function(code, symTab, auxEnv) {
    browser()
    origValue <- nOptions$debugSizeProcessing
    message('Entering into size processing debugging. You may need to donOptions(debugSizeProcessing = FALSE) if this exits in any non-standard way.')
    set_nOption('debugSizeProcessing', TRUE)
    ans <- recurseSetSizes(code, symTab, auxEnv)
    removeExprClassLayer(code$caller, 1)
    set_nOption('debugSizeProcessing', origValue)
    return(ans)
}

## promote numeric output to most information-rich type, double > integer > logical
## Note this will not be correct for logical operators, where output type should be logical
arithmeticOutputType <- function(t1, t2 = NULL, returnTypeCode = NULL) {
  if (!is.null(returnTypeCode) && returnTypeCode %in% c(1L, 2L, 3L))
    return(names(returnTypeCodes)[[returnTypeCode]])
  if (t1 == 'double') return('double')
  if (!is.null(t2) && t2 == 'double') return('double')
  if (t1 == 'integer') return('integer')
  if (!is.null(t2) && t2 == 'integer') return('integer')
  if (returnTypeCode == 5L) return('integer') ## no logical
  return('logical')
}
