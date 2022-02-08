##############
## Eigenize ##
##############

eigenizeUseArgs <- c(
    list(
        setWhich = c(FALSE, TRUE),
        setRepVectorTimes = c(FALSE, TRUE, TRUE)
        ))

eigenizeEnv <- new.env()
eigenizeEnv$.debug <- FALSE
inEigenizeEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = eigenizeEnv)
}

compile_eigenize <- function(code,
                             symTab,
                             auxEnv,
                             ## It is unclear if workEnv will be needed.
                             workEnv = new.env()) {
  nErrorEnv$stateInfo <- paste0("handling eigenize for ",
                                code$name,
                                ".")
  setupExprs <- list()
  if(code$isLiteral) {
    return(list())
  }
  if(code$isName) {
    return(list())
  }
  if(code$isCall) {
    if(code$name == '{') {
      for(i in seq_along(code$args)) {
        ## recurse <- FALSE
        ## if(code$args[[i]]$name == 'eigenize') {
        ##     removeExprClassLayer(code$args[[i]]) ## strip the eigenize()
        ##     recurse <- TRUE
        ## }
        ## if(code$args[[i]]$name %in%
        ##    c('for', ifOrWhile, '{', 'nimSwitch'))
        ##     recurse <- TRUE
        recurse <- TRUE
        if(recurse) {
          setupCalls <- unlist(
            compile_eigenize(code$args[[i]],
                             symTab,
                             auxEnv,
                             workEnv = new.env())) ## a new line
          if(length(setupCalls) > 0) {
            newExpr <- newBracketExpr(args = c(setupCalls,
                                               code$args[[i]])
                                      )
            setArg(code, i, newExpr)
          }
        }
      }
      return(invisible(NULL))
    }
    if(code$name == 'for') {
      compile_eigenize(code$args[[3]], symTab, auxEnv)
      return(invisible(NULL))
    }
    if(code$name %in% ifOrWhile) {
      compile_eigenize(code$args[[2]], symTab, auxEnv)
      if(length(code$args)==3)
        compile_eigenize(code$args[[3]], symTab, auxEnv)
      return(invisible(NULL))
    }
    opInfo <- operatorDefEnv[[code$name]]
    if(!is.null(opInfo)) {
      handlingInfo <- opInfo[["eigenImpl"]]
      if(!is.null(handlingInfo)) {
        beforeHandler <- handlingInfo[['beforeHandler']]
        ##eCall <- eigenizeCallsBeforeRecursing[[code$name]] ## previous cases can be absorbed into this.  This allows catching expressions that evaluate to something numeric, like nfVar(nf, 'x')
        if(!is.null(beforeHandler)) {
          setupExprs <- c(setupExprs,
                          eval(call(beforeHandler,
                                    code,
                                    symTab,
                                    auxEnv,
                                    workEnv,
                                    handlingInfo),
                               envir = eigenizeEnv))
          return(if(length(setupExprs) == 0) NULL else setupExprs)
        }
      }
    }
    IsetAliasRisk <- FALSE
    if(code$name %in% c('t', 'asRow')) {
      IsetAliasRisk <- workEnv[['aliasRisk']] <- TRUE
    }


    iArgs <- seq_along(code$args)
    useArgs <- eigenizeUseArgs[[code$name]]
    if(!is.null(useArgs)) iArgs <- iArgs[-which(!useArgs)] ## this allows iArgs to be longer than useArgs.  if equal length, iArgs[useArgs] would work 
    
    for(i in iArgs) {
      if(inherits(code$args[[i]], 'exprClass'))
        setupExprs <- c(setupExprs,
                        compile_eigenize(code$args[[i]], symTab, auxEnv, workEnv))
    }
    ## finally, call any special handlers
    if(!is.null(opInfo)) {
      handlingInfo <- opInfo[["eigenImpl"]]
      if(!is.null(handlingInfo)) {
        handler <- handlingInfo[['handler']]
        
        if(!is.null(handler)) {
          setupExprs <- c(setupExprs,
                          eval(call(handler,
                                    code,
                                    symTab,
                                    auxEnv,
                                    workEnv,
                                    handlingInfo),
                               envir = eigenizeEnv))
        }
      }
    }
    if(IsetAliasRisk) workEnv[['aliasRisk']] <- NULL
  }
  return(if(length(setupExprs) == 0) NULL else setupExprs)
}

inEigenizeEnv(
  promoteTypes <- function(code, which_args = seq_along(code$args)) {
    resultType <- code$type$type
    for(i in which_args) {
      if(inherits(code$args[[i]], 'exprClass')) {
        if(code$args[[i]]$type$type != resultType)
          eigenCast(code, i, resultType)
      }
    }
    NULL
  }
)

inEigenizeEnv(
  promoteArgTypes <- function(code) {
    if(!(inherits(code$args[[1]], 'exprClass') & inherits(code$args[[2]], 'exprClass'))) return(NULL)
    a1type <- code$args[[1]]$type$type
    a2type <- code$args[[2]]$type$type
    if(a1type == a2type) return(NULL)
    ## at this point, we know both args are exprClass objects and their type doesn't match
    argID <- 0
    newType <- 'double'

    if(code$name == '<-') {argID <- 2; newType <- a1type}
    ## because we know arg types don't match, pairs of conditions are mutually exclusive
    if(argID == 0) {
      if(a2type == 'double') {argID <- 1; newType <- 'double'}
      if(a1type == 'double') {argID <- 2; newType <- 'double'}
    }
    if(argID == 0) {
      if(a2type == 'integer') {argID <- 1; newType <- 'integer'}
      if(a1type == 'integer') {argID <- 2; newType <- 'integer'}
    }
    if(argID == 0) {
      ## Last two should never be needed, but for completeness:
      if(a2type == 'logical') {argID <- 1; newType <- 'logical'}
      if(a1type == 'logical') {argID <- 2; newType <- 'logical'}
    }
    if(argID != 0) 
      eigenCast(code, argID, newType) # becomes static_cast<>() for scalars at generateCpp stage
    invisible(NULL)
  }
)

inEigenizeEnv(
  eigenCast <- function(code, argIndex, newType) {
    ## for a scalar, eigenCast(x) becomes static_cast<type>(x) at generateCpp stage
    ## for a non-scalar, generateCpp uses eigen cast method.
    castExpr <- insertExprClassLayer(code, argIndex, 'eigencast')
    cppType <- exprClass$new(isName = TRUE, isCall = FALSE, isAssign = FALSE,
                             name = scalarTypeToCppType(newType))
    setArg(castExpr, 2, cppType)
    castExpr$type <- symbolBasic$new(name = castExpr$name,
                                     type = newType,
                                     nDim = castExpr$args[[1]]$type$nDim)
    castExpr
  }
)

inEigenizeEnv(
  Assign <- function(code, symTab, auxEnv, workEnv,
                     handlingInfo) {
    if(isTRUE(get_nOption("use_flexible_assignment"))) {
      insertExprClassLayer(code, 1, 'flex_')
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  replaceCodeName <- function(code, handlingInfo) {
    replacement <- handlingInfo$nameReplacement
    if (!is.null(replacement)) {
      ## replace the name in the AST
      code$name <- replacement
    }
  }
)

inEigenizeEnv(
  maybe_convertToMethod <- function(code, handlingInfo, force = FALSE) {
    if(isTRUE(handlingInfo$method) || isTRUE(force)) {
      methodName <- code$name
      code$name <- '.method'
      if(length(code$args) > 1) {
        for(i in length(code$args):2) {
          setArg(code, i+1, code$args[[i]])
        }
      }
      setArg(code, 2, nParse(paste0("\"", methodName, "\"")))
    }
    NULL
  }
)

inEigenizeEnv(
  ## could be combined with cWiseBinary and Reduction?
  cWiseUnary <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if(code$args[[1]]$type$nDim == 0) {
      if(!is.null(handlingInfo$scalarCall))
        code$name <- handlingInfo$scalarCall
      return(invisible(NULL))
    }
    promoteTypes(code)
    maybe_convertToMethod(code, handlingInfo)
    invisible(NULL)
  }
)

inEigenizeEnv(
  scalarTypeToCppType <- function(typeString) {
    if (!typeString %in% c('double', 'integer', 'logical'))
      stop(
        paste0("Don't know the correct C++ fundamental type keyword for ",
               typeString, "."), call. = FALSE
      )
    switch(
      typeString,
      double = 'double',
      integer = 'int',
      logical = 'bool'
    )
  }
)

inEigenizeEnv(
  cWiseUnary_external <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    ## replace the operator name with the equivalent C++ method name if needed
    replaceCodeName(code, handlingInfo)    
    ## All other steps are only needed for non-scalars
    if(code$args[[1]]$type$nDim == 0) return(invisible(NULL))
    
    promoteTypes(code)
    ## Get C++ type strings, e.g. int for integer
    inputType <- scalarTypeToCppType(code$args[[1]]$type$type)
    returnType <- scalarTypeToCppType(code$type$type)
    ## the operator name becomes the argument to std::ptr_fun
    ## Old version, using std::ptr_fun
    ## newName <- paste0('std::ptr_fun<', inputType, ', ',
    ##                   returnType, '>(', code$args[[2]]$name, ')')
    ##
    ## New version: Use C++11 lambda expression
    ## [](double x)->double{return cos(x);}
    ## We generate it as explicitly as possible (e.g. including return type)

    lambdaContent <- copyExprClass(code)
    setArg(lambdaContent, 1, exprClass$new(isName=TRUE, isCall=FALSE, isAssign=FALSE, name = 'x__'))
    #     lambdaContent$args[[1]]$name <- 'x__'
    lambdaContent$args[[1]]$type <- code$args[[1]]$type$clone(deep=TRUE)
    lambdaContent$args[[1]]$type$nDim <- 0
    lambdaContent$type <- lambdaContent$args[[1]]$type
    
    returnExpr <- nParse(quote(return(dummy)))
    setArg(returnExpr, 1, lambdaContent)
    
    lambdaDecl <- nParse(as.name(paste0('[](', inputType, ' x__)->', returnType)))
    newExpr <- nParse(quote(LambdaFun_(decl, def)))
    setArg(newExpr, 1, lambdaDecl)
    setArg(newExpr, 2, returnExpr)
    
    ## Convert foo(x) to x.foo(), i.e. .method(x, foo) 
    maybe_convertToMethod(code, handlingInfo)
    code$args[[2]]$name <- 'unaryExpr'
    setArg(code, 3, newExpr)
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseAddSub <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if (length(code$args) == 1)
      return(cWiseUnary(code, symTab, auxEnv, workEnv, handlingInfo))
    promoteTypes(code)
    d1 <- code$args[[1]]$type$nDim
    d2 <- code$args[[2]]$type$nDim
    if(d1 != d2) {
      # perform operation with reshaping, i.e., for matrix-vector operations
      replacementName <- handlingInfo$replacements[[code$name]]
      if(!is.null(replacementName)) {
        if(d2 > d1) {
          # perform operation w/reshaping the LHS argument
          code$name <- replacementName$LHS
        } else {
          # perform operation w/reshaping the RHS argument
          code$name <- replacementName$RHS
        }
      }
    }
    return(invisible(NULL))
  }
)

inEigenizeEnv(
  makeEigenArgsMatch <- function(code) {
    if(xor(code$args[[1]]$implementation$eigMatrix,
           code$args[[2]]$implementation$eigMatrix)) {
      ## default to matrix:
      if(!code$args[[1]]$implementation$eigMatrix)
        eigenizeMatricize(code$args[[1]])
      else
        eigenizeMatricize(code$args[[2]])
    }
  }
)

inEigenizeEnv(
  ## This hasn't been updated in redesign.
  eigenizeArrayize <- function(code) {
    newExpr <- exprClass$new(name = 'eigArray', args = list(code), eigMatrix = FALSE,
                             isName = FALSE, isCall = TRUE, isAssign = FALSE,
                             nDim = code$type$nDim, sizeExprs = code$sizeExprs, type = code$type,
                             caller = code$caller, callerArgID = code$callerArgID)
    setArg(code$caller, code$callerArgID, newExpr)
    setCaller(code, newExpr, 1)
    invisible(NULL)
  }
)

inEigenizeEnv(
  ## This hasn't been updated in redesign.
  eigenizeMatricize <- function(code) {
    newExpr <- exprClass$new(name = 'eigMatrix', args = list(code), eigMatrix = TRUE,
                             isName = FALSE, isCall = TRUE, isAssign = FALSE,
                             nDim = code$type$nDim, sizeExprs = code$sizeExprs, type = code$type,
                             caller = code$caller, callerArgID = code$callerArgID)
    setArg(code$caller, code$callerArgID, newExpr)
    setCaller(code, newExpr, 1)
    invisible(NULL)
  }
)


inEigenizeEnv(
  ## If the first arg is a scalar and the second has nDim in the
  ## specified range, then swap the positions of the two arguments.
  maybeSwapBinaryArgs <- function(code, handlingInfo, nDimRange = c(1, Inf)) {
    nDim <- code$args[[2]]$type$nDim
    nDimOK <- nDim >= nDimRange[1] && nDim <= nDimRange[2]
    if(code$args[[1]]$type$nDim == 0 && nDimOK) {
      tmpArg <- code$args[[1]]
      setArg(code, 1, code$args[[2]])
      setArg(code, 2, tmpArg)
      if (!is.null(handlingInfo$swapOp))
        code$name <- handlingInfo$swapOp ## op is noncommutative
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  Reduction <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    if (!isTRUE(handlingInfo$noPromotion)) promoteTypes(code)
    if(code$args[[1]]$type$nDim == 0) {
      if(isTRUE(handlingInfo$removeForScalar)) #used for mean(scalar) = scalar and similar cases.
        removeExprClassLayer(code)
      else if(!is.null(handlingInfo$replaceForScalar)) {## used for length(scalar)=1
        setArg(code, 1, exprClass$new(isLiteral = TRUE, isName = FALSE, isCall=FALSE, isAssign=FALSE,
                                      name = handlingInfo$replaceForScalar))
        removeExprClassLayer(code)
      }
    } else
      maybe_convertToMethod(code, handlingInfo) # used for mean(vector) = vector.mean() if method=TRUE
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseMultDiv <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if (isEigScalar(code$args[[1]]) && isEigScalar(code$args[[2]]))
      return(invisible(NULL))
    promoteTypes(code)
    d1 <- code$args[[1]]$type$nDim
    d2 <- code$args[[2]]$type$nDim
    if(d1 != d2) {
      # perform operation with reshaping, i.e., for matrix-vector operations
      replacementName <- handlingInfo$replacements[[code$name]]
      if(!is.null(replacementName)) {
        if(d2 > d1) {
          # perform operation w/reshaping the LHS argument
          code$name <- replacementName$LHS
        } else {
          # perform operation w/reshaping the RHS argument
          code$name <- replacementName$RHS
        }
      }
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  convert_cWiseBinaryToUnaryExpr <- function(code, handlingInfo) {
      input1Type <- scalarTypeToCppType(code$args[[1]]$type$type)
      input2RawType <- code$args[[2]]$type$type
      returnType <- scalarTypeToCppType(code$type$type)

      lambdaContent <- copyExprClass(code)
      setArg(lambdaContent, 1, exprClass$new(isName=TRUE, isCall=FALSE, isAssign=FALSE, name = 'x__'))
#      lambdaContent$args[[1]]$name <- 'x__'
      lambdaContent$args[[1]]$type <- code$args[[1]]$type$clone(deep=TRUE)
      lambdaContent$args[[1]]$type$nDim <- 0
      lambdaContent$type <- lambdaContent$args[[1]]$type
      
      eigenCast(lambdaContent, 2, input2RawType)

      returnExpr <- nParse(quote(return(dummy)))
      setArg(returnExpr, 1, lambdaContent)

      lambdaDecl <- nParse(as.name(paste0('[&](', input1Type, ' x__)->', returnType)))
      newExpr <- nParse(quote(LambdaFun_(decl, def)))
      setArg(newExpr, 1, lambdaDecl)
      setArg(newExpr, 2, returnExpr)

      maybe_convertToMethod(code, handlingInfo, force=TRUE)
      code$args[[2]]$name <- 'unaryExpr'
      setArg(code, 3, newExpr)
      invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseBinary <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    promoteTypes(code)
    maybeSwapBinaryArgs(code, handlingInfo)
    if(code$args[[1]]$type$nDim > 0) { # arg1 is non-scalar
      if(code$args[[2]]$type$nDim == 0) # arg2 is scalar
        convert_cWiseBinaryToUnaryExpr(code, handlingInfo)
      else
        maybe_convertToMethod(code, handlingInfo) # arg2 is non-scalar.
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseByScalar <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    ## key difference for ByScalar case:
    ## The second argument must be scalar
    if(!is.numeric(code$args[[2]]$name)) checkArgDims(code, 2, c(0, 0))
    promoteTypes(code)
    if(code$args[[1]]$type$nDim > 0)
      convert_cWiseBinaryToUnaryExpr(code, handlingInfo)
    ## Are there any cases where a built-in Eigen method is sufficient:
    ##maybe_convertToMethod(code, handlingInfo)
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseBinaryLogical <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    ## key difference for logical case:
    ## promote args to match each other, not logical return type
    promoteArgTypes(code)
    maybeSwapBinaryArgs(code, handlingInfo)
    if(code$args[[1]]$type$nDim > 0) { # arg1 is non-scalar
      if(code$args[[2]]$type$nDim == 0) # arg2 is scalar
        convert_cWiseBinaryToUnaryExpr(code, handlingInfo)
      else
        maybe_convertToMethod(code, handlingInfo) # arg2 is non-scalar.
    }
    d1 <- code$args[[1]]$type$nDim
    d2 <- code$args[[2]]$type$nDim
    if(d1 != d2) {
      # perform operation with reshaping, i.e., for matrix-vector operations
      replacementName <- handlingInfo$replacements[[code$name]]
      if(!is.null(replacementName)) {
        if(d2 > d1) {
          # perform operation w/reshaping the LHS argument
          code$name <- replacementName$LHS
        } else {
          # perform operation w/reshaping the RHS argument
          code$name <- replacementName$RHS
        }
      }
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  PromoteAllButLastArg <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if (length(code$args) == 0)
      stop(exprClassProcessingErrorMsg(
        code, 'In PromoteAllButLastArg: expected at least one arg, but code has none.'
      ), call. = FALSE)
    promoteTypes(code, 1:(length(code$args) - 1))
    invisible(NULL)
  }
)

inEigenizeEnv(
  Bracket <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    n_args <- length(code$args)
    if (code$type$nDim == 0) {
      # Either we're indexing a vector and we keep '[' in the AST, or we're
      # indexing a non-vector object and we use 'index(' instead.
      # TODO: if (code$args[[1]]$type$nDim == 0)
      if (code$args[[1]]$type$nDim == 1) code$name <- 'index['
      else if (code$args[[1]]$type$nDim > 1) code$name <- 'index('
      ## Enforce C++ type long for all indices using static_cast<long>(index_expr)
      ## We see inconsistent C++ compiler behavior around casting a double index
      ## to a long index, so we do it explicitly.
      ## Right now we will hard-code the type "long" assuming it is the underlying
      ## type. We could more generally use EigenType::Index where EigenType is the type
      ## of the indexed variable, assuming it can't be an expression.
      if(length(code$args)>1) {
        for(i in 2:length(code$args)) {
          insertExprClassLayer(code, i, "static_cast<long>")
        }
      }
      return(invisible(NULL))
    }
    code$name <- paste0('Eigen::MakeStridedTensorMap<', code$type$nDim, '>::make')

    ## labelAbstractTypes IndexingBracket handler put named arg 'drop'
    drop <- code$args$drop$name
    ## remove the drop arg from AST
    code$args$drop <- NULL

    ## the labelAbstractTypes IndexingBracket handler ensures that index_args
    ## is not just the empty argument ""
    index_args <- code$args[-1]
    code$args[-1] <- NULL ## clear indexing args from AST

    blocks_expr <- setArg(
      code, 2,
      exprClass$new(name = 'Eigen::MakeIndexBlocks',
                    isName = FALSE, isCall = TRUE)
    )

    for (i in seq_along(index_args)) {
      ## create b__ call
      b_expr <- exprClass$new(name = 'b__',
                              isName = FALSE, isCall = TRUE)

      if (index_args[[i]]$isCall && index_args[[i]]$name == ':') {
        ## the labelAbstractTypes handler ensures ':' is the only non-scalar call
        setArg(b_expr, 1, index_args[[i]]$args[[1]])
        setArg(b_expr, 2, index_args[[i]]$args[[2]])

      } else if (index_args[[i]]$name != '') { ## not a call
        ## the labelAbstractTypes handler ensures index_args[[i]] is scalar
        setArg(b_expr, 1, index_args[[i]])
        if (!drop)
          setArg(b_expr, 2, copyExprClass(index_args[[i]]))

      }

      ## subtract 1 from each of this b__ call's numeric arguments
      for (j in seq_along(b_expr$args)) {
        if (b_expr$args[[j]]$type$type %in% c('integer', 'double')) {
          insertExprClassLayer(b_expr, j, '-')
          setArg(b_expr$args[[j]], 2, literalIntegerExpr(1))
        }
      }

      ## add the b__ call to the AST as arg to blocks_expr
      setArg(blocks_expr, i, b_expr)
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  TensorCreation <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    code_args <- code$args
    code$args <- NULL
    value_provided <- 'value' %in% names(code_args)
    if (value_provided)
      setArg(code, 1, code_args[['value']])
    else {
      value_expr <- switch(
        code$type$type,
        double = literalDoubleExpr(0),
        integer = literalIntegerExpr(0),
        logical = literalLogicalExpr(FALSE)
      )
      setArg(code, 1, value_expr)
    }
    if (code$name %in% c('nNumeric', 'nInteger', 'nLogical')) {
      if ('length' %in% names(code_args))
        setArg(code, 2, code_args[['length']])
      else {
        if (value_provided && code_args[['value']]$type$nDim != 0)
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste("In eigenization handler TensorCreation: when non-scalar",
                    "'value' is provided, 'length' must also be provided.")
            ), call. = FALSE
          )
        setArg(code, 2, literalIntegerExpr(0))
      }
    } else if (code$name == 'nMatrix') {
      nrow_provided <- 'nrow' %in% names(code_args)
      ncol_provided <- 'ncol' %in% names(code_args)
      ## TODO: calcMissingMatrixSize
      if ((nrow_provided || ncol_provided) &&
            !(nrow_provided && ncol_provided))
        stop(
          exprClassProcessingErrorMsg(
            code,
            paste("In eigenization handler TensorCreation: if either 'nrow'",
                  "or 'ncol' is provided, they must both be provided.")
          ), call. = FALSE
        )
      if (!(nrow_provided || ncol_provided)) {
        if (value_provided && code_args[['value']]$type$nDim != 0)
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste("In eigenization handler TensorCreation: when non-scalar",
                    "'value' is provided, 'nrow' and 'ncol' must also be",
                    "provided.")
            ), call. = FALSE
          )
        setArg(code, 2, literalIntegerExpr(1)) ## nrow
        setArg(code, 3, literalIntegerExpr(1)) ## ncol
      } else {
        setArg(code, 2, code_args[['nrow']])
        setArg(code, 3, code_args[['ncol']])
      }
    } else if (code$name == 'nArray') {
      if ('dim' %in% names(code_args)) {
        if (code_args[['dim']]$name == 'nC') {
          ## TODO: this won't be needed when 'nC' is implemented
          nC_arg <- code_args[['dim']]
          promoteTypes(nC_arg)
          for (i in seq_along(nC_arg$args)) {
            setArg(code, i + 1, nC_arg$args[[i]])
          }
        } else {
          setArg(code, 2, code_args[['dim']])
        }
      } else {
        if (value_provided && code_args[['value']]$type$nDim != 0)
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste("In eigenization handler TensorCreation: when non-scalar",
                    "'value' is provided, 'dim' must also be provided.")
            ), call. = FALSE
          )
        setArg(code, 2, literalIntegerExpr(1)) ## nrow
        setArg(code, 3, literalIntegerExpr(1)) ## ncol
      }
    }
    type_string <- scalarTypeToCppType(code$type$type)
    code$name <- paste0(
      'createTensor<', type_string, ', ', code$type$nDim, '>'
    )
    invisible(NULL)
  }
)

inEigenizeEnv(
  Which <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    ## the labelAbstractTypes handler Which guarantees that 'code' has one
    ## logical arg
    code$name <- paste0('setWhich', code$args[[1]]$type$nDim)
    invisible(NULL)
  }
)

inEigenizeEnv(
  Rep <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    ## TODO: this assumes proper arg ordering and naming
    ## TODO: handle zero-dim first arg
    arg_names <- names(code$args)
    if (length(code$arg) > 3) {
      ## the second arg is 'times' which is ignored whenever 'length.out' is
      ## included
      removeArg(code, 2)
      ## any other args are unused and can be removed
      code$args <- code$args[1:3]
      code$name <- 'repLenEach'
    } else if (length(code$args) == 3) {
      if (!'each' %in% arg_names) {
        ## the second arg must be 'times' and the third is 'length.out'
        removeArg(code, 2)
        code$name <- 'repLen'
      } else {
        if ('length.out' %in% arg_names)
          code$name <- 'repLenEach'
        else
          code$name <- 'repTimesEach'
      }
    } else if (length(code$args) == 2) {
      if ('length.out' %in% arg_names)
        code$name <- 'repLen'
      else if ('each' %in% arg_names)
        code$name <- 'repEach'
      else
        code$name <- 'repTimes'
    } else {
      ## If length(code$args) == 1, this takes 'rep' out of the AST and
      ## replaces it with its one arg. If length(code$args) == 1, 'rep' is
      ## replaced with NULL which is exactly what the call rep() returns.
      removeExprClassLayer(code)
      return(invisible(NULL))
    }
    ## In C++, eval = true when the call to rep is part of a larger expression
    ## and rep's arg is a call to avoid costly repeated computation when
    ## reshaping and broadcasting the tensor.
    if (!code$caller$name %in% assignmentOperators && code$args[[1]]$isCall)
      setArg(code, length(code$args) + 1, literalLogicalExpr())
    invisible(NULL)
  }
)

inEigenizeEnv(
 Colon <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if (!code$caller$name == '[') {
      code$name <- 'seq'
      by_arg <- literalIntegerExpr(1)
      setArg(code, 'by', by_arg, add = TRUE)
      compile_eigenize(code, symTab, auxEnv, workEnv)
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  Seq <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if (length(code$args) == 0 ||
          ## seq(by = x) always returns 1
          (length(code$args) == 1 && 'by' %in% names(code$args))) {
      integer1 <- literalIntegerExpr(1)
      setArg(code$caller, code$callerArgID, integer1)
      return(invisible(NULL))
    }
    if (length(code$args) == 1) {
      ## the only arg becomes 'to'... note that `seq(from = 10)` gives 1:10
      integer1 <- literalIntegerExpr(1)
      if ('length.out' %in% names(code$args))
        insertExprClassLayer(code, 1, 'ceil')
      insertArg(code, 1, integer1, 'from')
      names(code$args)[2] <- 'to'
    }
    byProvided <- 'by' %in% names(code$args)
    lengthProvided <- 'length.out' %in% names(code$args) 
    if (!byProvided && !lengthProvided) {
      if (length(code$args) >= 3) {
        ## by was provided as a positional arg
        byProvided <- TRUE
        if (length(code$args) >= 4)
          ## length.out was also provided as a positional arg
          lengthProvided <- TRUE
        ## TODO: what if more than 4 args provided (e.g. 'along.with')
      } else {
        by_arg <- literalIntegerExpr(1)
        setArg(code, 'by', by_arg, add = TRUE)
        byProvided <- TRUE
      }
    }
    if (byProvided) {
      code$name <- 'nSeqBy'
      if(lengthProvided)
        code$name <- paste0(code$name, 'Len')
    } else { ## must be lengthProvided
      code$name <- 'nSeqLen'
    }
    code$name <- paste0(
      code$name,
      switch(
        code$type$type,
        'double' = 'D',
        'integer' = 'I'
      )
    )
    invisible(NULL)
  }
)

inEigenizeEnv(
  asSparse <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if(length(code$args) > 2) {
      stop(exprClassProcessingErrorMsg(
        code, 'asSparse is expected to be called with at most two arguments'
      ), call. = FALSE)
    }
    if(length(code$args) == 1) {
      if(inherits(code$args[[1]]$type, 'symbolSparse')) {
        # argument is already a sparse type, so no work needs to be done; remove 
        # code expression from AST as we prepare to gnerate C++ code to compile
        removeExprClassLayer(code)
      }
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  asDense <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if(length(code$args) > 1) {
      stop(exprClassProcessingErrorMsg(
        code, 'asDense is expected to be called with at most one argument'
      ), call. = FALSE)
    }
    if(length(code$args) == 1) {
      if(!inherits(code$args[[1]]$type, 'symbolSparse')) {
        # argument is already a dense type, so no work needs to be done; remove 
        # code expression from AST as we prepare to gnerate C++ code to compile
        removeExprClassLayer(code)
      }
    }
    invisible(NULL)
  }
)

