
labelAbstractTypesEnv <- new.env()
labelAbstractTypesEnv$.debug <- FALSE

compile_labelAbstractTypes <- function(code,
                                       symTab,
                                       auxEnv) { 
    nErrorEnv$stateInfo <- paste0("handling labelAbstractTypes for ",
                                       code$name,
                                       ".")

    logging <- loggingIsOn()
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
        if(!is.null(opInfo)) {
            handlingInfo <- opInfo[["labelAbstractTypes"]]
            if(!is.null(handlingInfo)) {
                handler <- handlingInfo[['handler']]
                if(!is.null(handler)) {
                    ans <- eval(call(handler, code, symTab, auxEnv, handlingInfo),
                                envir = labelAbstractTypesEnv)
                    nErrorEnv$stateInfo <- character()
                    return(ans)
                }
            }
        }
        ## To-Do: update handling nClass method calls
        if(symTab$symbolExists(code$name, TRUE)) { ## could be a nFunction object
            return(size_nFunction(code, symTab, auxEnv) )
        }
        ## To-do: update RCfunction 
        ## Finally, it could be an RCfunction (a nFunction with no setup == a simple function) {
        if(exists(code$name)) {
            obj <- get(code$name)
            if(is.rcf(obj)) { ## it is an RC function
                nfmObj <- environment(obj)$nfMethodRCobject
                uniqueName <- nfmObj$uniqueName
                if(length(uniqueName)==0)
                    stop(
                        exprClassProcessingErrorMsg(
                            code,
                            'In size processing: A no-setup nFunction with no internal name is being called.'),
                        call. = FALSE)
                if(is.null(auxEnv$needed_nFunctions[[uniqueName]])) {
                    auxEnv$needed_nFunctions[[uniqueName]] <- nfmObj
                }
                ## new with nCompilerLists: we need to initiate compilation here so we can get full returnType information, including of nimbleLists
                RCfunProc <-
                    auxEnv$.nCompilerProject$compileRCfun(obj,
                                                        initialTypeInference = TRUE)
                return(sizeRCfunction(code, symTab, auxEnv, nfmObj, RCfunProc))
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
    Assign <- 
        function(code, symTab, auxEnv, handlingInfo) {
            logging <- loggingIsOn()
            if (logging)
              appendToLog(paste('Calling handler Assign for', code$name))
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
            if (logging) {
              appendToLog(paste('Finished handling Assign for', code$name))
              logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
            }
            if(length(inserts) == 0) NULL else inserts
        }
)

inLabelAbstractTypesEnv(
    AssignAfterRecursing <- 
        function(code, symTab, auxEnv, handlingInfo) {
            logging <- loggingIsOn()
            if (logging)
              appendToLog(paste('Calling handler AssignAfterRecursing for', code$name))
            LHS <- code$args[[1]]
            RHS <- code$args[[2]]
            RHStype <- RHS$type
            if(LHS$isName) {
                if(!symTab$symbolExists(LHS$name, TRUE)) {
                    newSym <- RHStype$clone()
                    newSym$name <- LHS$name
                    symTab$addSymbol(newSym)
                    LHS$type <- newSym
                    code$type <- newSym
                }
            }
            if (logging) {
              appendToLog(paste('Finished handling AssignAfterRecursing for', code$name))
              logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
            }
            NULL
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
            logging <- loggingIsOn()
            if (logging)
              appendToLog(paste('Calling handler UnaryCwise for', code$name))
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
            nDim <- argType$nDim
            ## If we need different unary operators to have different scalar return types,
            ## we can add a field to the handlerInfo for that.  It could default to double.
            resultScalarType <- 'double'
            resultType <- symbolBasic$new(nDim = nDim,
                                          type = resultScalarType)
            code$type <- resultType
            if (logging) {
              appendToLog(paste('Finished handling UnaryCwise for', code$name))
              logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
            }
            invisible(NULL)

        }
)

## Handler for binary component-wise operators
inLabelAbstractTypesEnv(
    BinaryCwise <- 
        function(code, symTab, auxEnv, handlingInfo) {
            logging <- loggingIsOn()
            if (logging)
              appendToLog(paste('Calling handler BinaryCwise for', code$name))
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
            resultScalarType <- arithmeticOutputType(a1Type$type, a2Type$type)
            resultType <- symbolBasic$new(nDim = nDim,
                                          type = resultScalarType)
            code$type <- resultType
            ##code$typeName <- class(resultType)[1]
            if (logging) {
              appendToLog(paste('Finished handling BinaryCwise for', code$name))
              logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
            }
            invisible(NULL)
            ## 
        }
)

inLabelAbstractTypesEnv(
    BinaryCwiseLogical <- 
        function(code, symTab, auxEnv, handlingInfo) {
            logging <- loggingIsOn()
            if (logging)
              appendToLog(paste('Calling handler BinaryCwiseLogical for', code$name))
            ans <- BinaryCwise(code, symTab, auxEnv, handlingInfo)
            code$type$type <- 'logical'
            if (logging) {
              appendToLog(paste('Finished handling BinaryCwiseLogical for', code$name))
              logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
            }
            ans
        }
)

inLabelAbstractTypesEnv(
    UnaryReduction <-
        function(code, symTab, auxEnv, handlingInfo) {
            logging <- loggingIsOn()
            if (logging)
              appendToLog(paste('Calling handler UnaryReduction for', code$name))
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
            if (logging) {
              appendToLog(paste('Finished handling UnaryReduction for', code$name))
              logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
            }
            inserts
        }
)


inLabelAbstractTypesEnv(
    Return <- 
        function(code, symTab, auxEnv, handlingInfo) {
            logging <- loggingIsOn()
            if (logging)
              appendToLog(paste('Calling handler Return for', code$name))
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
            if (logging) {
              appendToLog(paste('Finished handling Return for', code$name))
              logAST(code, paste('Resulting AST for', code$name), showImpl = FALSE)
            }
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
arithmeticOutputType <- function(t1, t2) {
    if(t1 == 'double') return('double')
    if(t2 == 'double') return('double')
    if(t1 == 'integer') return('integer')
    if(t2 == 'integer') return('integer')
    return('logical')
}


