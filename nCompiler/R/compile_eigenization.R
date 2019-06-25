######################################
## System to label for eigenization ##
######################################

## exprClasses_labelForEigenization 
##
## This works recursively through a parse tree (exprClass object)
## Any expression in which any piece has a size expression (in its exprClass object) that is not all 1s
## will be wrapped in eigenize(). The exception is expressions that have indexing.
##
## e.g. In Y <- t(A) %*% A
## The result of t(A) %*% A might be length 1, but the A and t(A) have size expressions != 1, so this whole line becomes
## eigenize(Y <- t(A) %*% A)
##
## On the other hand, Y <- B[3,4] is not wrapped in eigenize() because, even though B might have non-1 size expressions,
## the exprClass object for `[`(B, 3, 4) has size expressions == list(1, 1)
##
## two kinds of intermediates should have already been pulled out:
## 1. arguments to nFunctions or keywords like return() that ever involve non-scalar-equivalent steps
## 2. nFunctions that return non-scalar-equivalent and are used within a larger expression

exprClasses_labelForEigenization <- function(code) {

    if(code$isCall) {
        if(code$name == '{') {
            for(i in seq_along(code$args)) {
                exprClasses_labelForEigenization(code$args[[i]])
            }
            return(invisible(NULL))
        }
        if(code$name == 'for') {
            exprClasses_labelForEigenization(code$args[[3]])
            return(invisible(NULL))
        }
        if(code$name %in% ifOrWhile) {
            exprClasses_labelForEigenization(code$args[[2]])
            if(length(code$args) == 3) exprClasses_labelForEigenization(code$args[[3]])
            return(invisible(NULL))
        }
        if(code$name == 'nimSwitch') {
            if(length(code$args) > 2)
                for(iSwitch in 3:length(code$args))
                    exprClasses_labelForEigenization(code$args[[iSwitch]])
            return(invisible(NULL))
        }
        if(code$name %in% callToSkipInEigenization) return(invisible(NULL))

        if(length(code$implementation$toEigen) > 0) {
          if(code$implementation$toEigen == 'yes') {
         ##   if(anyNonScalar(code)) {
                output <- insertExprClassLayer(code$caller, code$callerArgID, 'eigenize')
        	return(output)
         ##   }
            }
        }
    }
    invisible(NULL)
}



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
    if(argID != 0) eigenCast(code, argID, newType)

    NULL
  }
)

inEigenizeEnv(
  eigenCast <- function(code, argIndex, newType) {
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
  convertToCppName <- function(code, handlingInfo) {
    cppName <- handlingInfo$cppName
    if (!is.null(cppName)) {
      ## replace the name in the AST
      code$name <- cppName
    }
  }
)

inEigenizeEnv(
  convertToMethod <- function(code, handlingInfo) {
    if(isTRUE(handlingInfo$method)) {
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
    promoteTypes(code)
    convertToMethod(code, handlingInfo)
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
    promoteTypes(code)
    inputType <- scalarTypeToCppType(code$args[[1]]$type$type)
    returnType <- scalarTypeToCppType(code$type$type)
    convertToMethod(code, handlingInfo)
    ## the operator name becomes the argument to std::ptr_fun
    newName <- paste0('std::ptr_fun<', inputType, ', ',
                      returnType, '>(', code$args[[2]]$name, ')')
    newExpr <- exprClass$new(isName = TRUE, isCall = FALSE, isAssign = FALSE,
                             name = newName)
    code$args[[2]]$name <- 'unaryExpr'
    setArg(code, 3, newExpr)
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseByScalar <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if(!is.numeric(code$args[[2]]$name)) checkArgDims(code, 2, c(0, 0))
    promoteTypes(code)
    convertToMethod(code, handlingInfo)
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseAddSub <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if (length(code$args) == 1)
      return(cWiseUnary(code, symTab, auxEnv, workEnv, handlingInfo))
    promoteTypes(code)
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
    ## if(code$type$nDim == 0) return(invisible(NULL))
    promoteTypes(code)
    convertToMethod(code, handlingInfo)
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseMultDiv <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    if (isEigScalar(code$args[[1]]) && isEigScalar(code$args[[2]]))
      return(invisible(NULL))
    promoteTypes(code)
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseBinary <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    promoteTypes(code)
    maybeSwapBinaryArgs(code, handlingInfo)
    convertToMethod(code, handlingInfo)
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseBinaryLogical <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    ## key difference for logical case:
    ## promote args to match each other, not logical return type
    promoteArgTypes(code)
    maybeSwapBinaryArgs(code, handlingInfo)
    convertToMethod(code, handlingInfo)
    invisible(NULL)
  }
)

inEigenizeEnv(
  PromoteAllButLastArg <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    if (length(code$args) == 0)
      stop(exprClassProcessingErrorMsg(
        code, 'In PromoteAllButLastArg: expected at least one arg, but code has none.'
      ), call. = FALSE)
    promoteTypes(code, 1:(length(code$args) - 1))
    invisible(NULL)
  }
)

inEigenizeEnv(
  Bracket <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
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
