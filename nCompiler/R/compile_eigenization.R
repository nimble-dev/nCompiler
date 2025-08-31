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
                             # auxEnv is for sharing across lines in the same code body
                             # workEnv is for sharing within a line.  This
                             # could be done via auxEnv, but having workEnv is cleaner
                             # and more safely avoids having residual information left around.
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

    handlingInfo <- getOperatorDef(code$name, "eigenImpl")
    # operatorDefEnv[[code$name]]
    # if(!is.null(opInfo)) {
    #   handlingInfo <- opInfo[["eigenImpl"]]
      if(!is.null(handlingInfo)) {
        beforeHandler <- handlingInfo[['beforeHandler']]
        if(!is.null(beforeHandler)) {
          setupExprs <- c(setupExprs,
                          eval(call(beforeHandler,
                                    code,
                                    symTab,
                                    auxEnv,
                                    workEnv,
                                    handlingInfo),
                               envir = eigenizeEnv))
          # return(if(length(setupExprs) == 0) NULL else setupExprs)
        }
      }
    # }

    iArgs <- seq_along(code$args)
    useArgs <- eigenizeUseArgs[[code$name]]
    if(!is.null(useArgs)) iArgs <- iArgs[-which(!useArgs)] ## this allows iArgs to be longer than useArgs.  if equal length, iArgs[useArgs] would work 

    for(i in iArgs) {
      if(inherits(code$args[[i]], 'exprClass')) {
        setupExprs <- c(setupExprs,
                        compile_eigenize(code$args[[i]], symTab, auxEnv, workEnv))
      }
    }

    ## finally, call any special handlers
    # if(!is.null(opInfo)) {
    #   handlingInfo <- opInfo[["eigenImpl"]]
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
    # }
  }
  return(if(length(setupExprs) == 0) NULL else setupExprs)
}

inEigenizeEnv(
  # promoteTypes takes foo(a, b) and promotes types of a and b
  # to match type of foo(a, b).  promoting means casting from logical -> integer -> double.
  promoteTypes <- function(code, which_args = seq_along(code$args)) {
    resultType <- code$type$type
    for(i in which_args) {
      if(inherits(code$args[[i]], 'exprClass')) {
        if(code$args[[i]]$type$type != resultType) {
          # There are cases (e.g. from nSeq)
          # where output type can be integer even from inputs of double
          # We need to avoid reducing information by checking strictly
          # here whether a promotion should really be done.
          promote <- resultType == "double" ||
            (resultType == "integer" && code$args[[i]]$type$type == "logical")
          if(promote)
            eigenCast(code, i, resultType)
        }
      }
    }
    NULL
  }
)

inEigenizeEnv(
  # promoteArgTypes is similar to promote types but the type of
  # foo(a, b) is not considered.  Instead the types of a and b
  # are promoted to match each other, using the most information-rich
  # type (i.e. logical -> integer -> double)
  #
  # promoteArgTypes is used for the special case of assignment to
  # ensure that it is always the RHS that is cast to match the
  # LHS.
  promoteArgTypes <- function(code, assignment = FALSE) {
    if(!(inherits(code$args[[1]], 'exprClass') & inherits(code$args[[2]], 'exprClass'))) return(NULL)
    a1type <- code$args[[1]]$type$type
    a2type <- code$args[[2]]$type$type
    if(a1type == a2type) return(NULL)
    ## at this point, we know both args are exprClass objects and their type doesn't match
    argID <- 0
    newType <- 'double'

    # if(code$name == '<-')
    if(assignment) {argID <- 2; newType <- a1type}
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
  scalarCast <- function(code, argIndex, newType) {
    castExpr <- insertExprClassLayer(code, argIndex, 'scalarcast')
    cppType <- exprClass$new(isName = TRUE, isCall = FALSE, isAssign = FALSE,
                             name = scalarTypeToCppType(newType))
    setArg(castExpr, 2, cppType)
    castExpr$type <- symbolBasic$new(name = castExpr$name,
                                     type = newType,
                                     nDim = castExpr$args[[1]]$type$nDim)
    castExpr
  }
)

## Alias-checking system needs a complete revamp.
## This rough draft just won't do. It's too simple for:
## - arguments passed in by reference
## - nLists
## - LHS operators like diag()
## We may ideally want a combination of compile-time determination (where possible)
##  and a system for run-time determination.

## inEigenizeEnv(
##   # We need a way to check for easy-to-catch aliasing risks
##   # Aliasing risks refer to situations like
##   # x[2:5] <- x[1:4]
##   # In eigen, this will replace x[2] <- x[1]
##   # then x[3] <- x[2] , but the RHS will be the just-replaced
##   # value of x[2].  If we catch an alias risk,
##   # we will add .eval() on the end of the RHS Eigen expression
##   # This forces evaluation of the RHS into separate memory
##   # before copying into LHS elements.
##   #
##   # We could have situations like A$B$x[1:4].
##   # We define an EigenVarInfo object as a list of two elements:
##   # First element would be c("A", "B", "x")
##   # Second element would be TRUE to indicate there is a '[' involved
##   getVarInfoForAliasChecking <- function(LHS) {
##     # How intricate will we go in matching objects for alias risk?
##     LHSvarName <- NULL
##     hasBracket <- FALSE
##     if(LHS$isName) {
##       return(list(LHS$name, hasBracket))
##     } else if(LHS$name == '[') {
##       nested <- getVarInfoForAliasChecking(LHS$args[[1]])
##       return(list(nested[[1]], TRUE))
##     } else if(LHS$name %in% c('->member')) { # Expand to handle nDiag and other possible LHS operators
##       nested <- getVarInfoForAliasChecking(LHS$args[[1]])
##       return(list(c(nested[[1]], LHS$args[[2]]$name), FALSE ))
##     }
##     NULL
##   }
## )

## inEigenizeEnv(
##   checkRHSaliasing <- function(code, LHSvarInfo, insidePermutingFxn = FALSE) {
##     if(code$isLiteral) return(FALSE)
##     thisIsAvar <- code$isName
##     if(!thisIsAvar) {
##       thisIsAvar <- code$name == "[" || code$name == "$"
##     }
##     if(thisIsAvar) {
##       RHSvarInfo <- getVarInfoForAliasChecking(code)
##       aliasing <- identical(LHSvarInfo[[1]], RHSvarInfo[[1]])
##       if(aliasing) {
##         if(!insidePermutingFxn) {
##           aliasing <- isTRUE(LHSvarInfo[[2]]) || isTRUE(RHSvarInfo[[2]]) # future can check if indexing is the same, x[2:5] <- x[2:5] + 1
##         }
##       }
##       if(aliasing) return(TRUE)
##     }
##     if(code$isCall) { # must be true at this point
##       skip <- code$name %in% c("nFunction", "chainedCall",
##                                "nMul", "nDiag", "nDiagonal",
##                                "nSvd", "nChol", "nEigen",
##                                "nSolve", "nForwardSolve", "nBacksolve")
##       # operators that force evaluation anyway, so no need to recurse
##       # These should really be identified by an entry within their operatorDefEnv entries
##       if(!skip) {
##         iArgs <- seq_along(code$args)
##         if(code$name == '[') iArgs <- iArgs[iArgs != 1] # recurse into '[' index args only, for say x[2:5] <- y[ x ]
##         permutingFxn <- insidePermutingFxn || code$name %in% c("t", "asRow") # possibly add others
##         for(i in iArgs) {
##           if(checkRHSaliasing(code$args[[i]], LHSvarInfo, permutingFxn)) return(TRUE)
##         }
##       }
##     }
##     FALSE
##   }
## )

inEigenizeEnv(
  Assign_Before <- function(code, symTab, auxEnv, workEnv,
                           handlingInfo) {
    LHS <- code$args[[1]]
    ## See not on need for new alias checking system
    ##    LHSvarInfo <- getVarInfoForAliasChecking(LHS)
    ## if(is.null(LHSvarInfo))
    ##  stop("Problem determining LHS variable name for ", nDeparse(code))
    ## aliasRisk <- checkRHSaliasing(code$args[[2]], LHSvarInfo)
    ##
    ## Default for now to always assuming alias risk!
    aliasRisk <- TRUE
    workEnv$aliasRisk <- aliasRisk
    # If the LHS has no indexing, use nEval_
    ##
    ## We have a template nEval_ needed when the LHS is the entire object.
    ## See note in C++ code for nEval_.
    if(LHS$name != "[") workEnv$need_nEval <- TRUE
    ##    if(!isTRUE(LHSvarInfo[[2]])) workEnv$need_nEval <- TRUE
    invisible(NULL)
  }
)

inEigenizeEnv(
  Assign <- function(code, symTab, auxEnv, workEnv,
                     handlingInfo) {
    ## For scalar LHS, promoting arg types is not needed because flex_() handles
    ## so much on the C++ side including casting, and the casting is
    ## tricky because Eigen reduction operations (e.g. sum()) return
    ## 0-dimensional tensors rather than true scalars.
    if(inherits(code$args[[1]]$type, "symbolBasic"))
      if(code$args[[1]]$type$nDim != 0) {
        promoteArgTypes(code, assignment=TRUE)
      }
    ##
    ## Right now this is done in generateCpp$MidOperator, and for scalars only
    ## if(isTRUE(get_nOption("use_flexible_assignment"))) {
    ##   insertExprClassLayer(code, 1, 'flex_')
    ## }
    if(isTRUE(workEnv$aliasRisk)) {
      use_eval <- TRUE
      if(!inherits(code$args[[2]]$type, 'symbolBasic')) use_eval <- FALSE
      else if(code$args[[2]]$type$nDim == 0) use_eval <- FALSE
      if(use_eval) {
        if(isTRUE(workEnv$need_nEval)) {
          insertExprClassLayer(code, 2, 'nEval_')
          code$args[[2]]$type <- code$args[[2]]$args[[1]]$type
        } else {
          insertExprClassLayer(code, 2, 'eval')
          code$args[[2]]$type <- code$args[[2]]$args[[1]]$type
          eigenizeEnv$maybe_convertToMethod(code$args[[2]], handlingInfo = list(), force = TRUE)
        }
      }
    }
    workEnv$aliasRisk <- NULL
    workEnv$need_nEval <- NULL
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
  RandomGeneration <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    # determine arguments that parameterize the dist'n.
    size_ind = match('n', names(code$args))
    parameterArgInds = seq_along(code$args)[-size_ind]
    # promote argument types
    promoteTypes(code, which_args = parameterArgInds)
    invisible(NULL)
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
    if(d1 > 0 && d2 > 0) {
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
    }
    return(invisible(NULL))
  }
)

## inEigenizeEnv(
##   makeEigenArgsMatch <- function(code) {
##     if(xor(code$args[[1]]$implementation$eigMatrix,
##            code$args[[2]]$implementation$eigMatrix)) {
##       ## default to matrix:
##       if(!code$args[[1]]$implementation$eigMatrix)
##         eigenizeMatricize(code$args[[1]])
##       else
##         eigenizeMatricize(code$args[[2]])
##     }
##   }
## )

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
  BinaryReduction <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    if (!isTRUE(handlingInfo$noPromotion)) promoteTypes(code)
    scalarCast(code$caller, code$callerArgID, code$type$type)
    invisible(NULL)
  }
)

inEigenizeEnv(
  Reduction <- function(code, symTab, typeEnv, workEnv, handlingInfo) {
    if (!isTRUE(handlingInfo$noPromotion)) promoteTypes(code)
    if (isTRUE(handlingInfo$castLogical)) {
      ## There is some bizarre behavior for eigen's x.any() method.
      ## If x is non-bool, then naturally we want to cast to bool, resulting in
      ## x.cast<bool>().any()
      ## However, if x is bool, then x.any() DOES NOT WORK! It always returns FALSE.
      ## We work around that by casting to int: x.cast<int>().any(), which works.
      if(code$args[[1]]$type$type != "logical")
        eigenCast(code, 1, "logical")
      else
        eigenCast(code, 1, "integer")
    }
    if(code$args[[1]]$type$nDim == 0) {
      if(isTRUE(handlingInfo$removeForScalar)) #used for mean(scalar) = scalar and similar cases.
        code <- removeExprClassLayer(code)
      else if(!is.null(handlingInfo$replaceForScalar)) {## used for length(scalar)=1
        setArg(code, 1, literalIntegerExpr(1)) #exprClass$new(isLiteral = TRUE, isName = FALSE, isCall=FALSE, isAssign=FALSE,
                                      #name = handlingInfo$replaceForScalar))
        code <- removeExprClassLayer(code)
      }
    } else
      maybe_convertToMethod(code, handlingInfo) # used for mean(vector) = vector.mean() if method=TRUE
    scalarCast(code$caller, code$callerArgID, code$type$type)
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
    if(d1 > 0 && d2 > 0) {
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
    if(isTRUE(handlingInfo$allScalar)) {
      ## The first argument must be scalar
      if(!is.numeric(code$args[[1]]$name)) checkArgDims(code, 1, c(0, 0))
    }
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
  cWiseBinaryLogicalAfterHandlingTypes <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    maybeSwapBinaryArgs(code, handlingInfo)
    d1 <- code$args[[1]]$type$nDim
    d2 <- code$args[[2]]$type$nDim
    if(code$args[[1]]$type$nDim > 0) { # arg1 is non-scalar
      if(code$args[[2]]$type$nDim == 0) # arg2 is scalar
        convert_cWiseBinaryToUnaryExpr(code, handlingInfo)
      else
        maybe_convertToMethod(code, handlingInfo) # arg2 is non-scalar.
    }
    if(d1 > 0 && d2 > 0) {
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
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  cWiseBinaryLogical <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    ## key difference for logical case:
    ## promote args to match each other, not logical return type
    promoteArgTypes(code)
    cWiseBinaryLogicalAfterHandlingTypes(code, symTab, auxEnv, workEnv, handlingInfo)
  }
)

inEigenizeEnv(
  cWiseBinaryLogicalAndOr <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    ## key difference for logical case:
    ## promote args to match each other, not logical return type
    a1type <- code$args[[1]]$type$type
    a2type <- code$args[[2]]$type$type
    if(a1type != "logical") eigenCast(code, 1, "logical")
    if(a2type != "logical") eigenCast(code, 2, "logical")
    cWiseBinaryLogicalAfterHandlingTypes(code, symTab, auxEnv, workEnv, handlingInfo)
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

nCompiler:::inEigenizeEnv(
  Bracket_to_StridedTensorMap <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
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
  })


nCompiler:::inEigenizeEnv(
  MakeIndexByScalarCall <- function(index_slot, index_value, x) {
    ans <-  exprClass$new(isName = FALSE, isCall = TRUE, isAssign = FALSE,
                          name = "IndexByScalar")
    setArg(ans, 1, literalIntegerExpr(index_slot-1))
    setArg(ans, 2, index_value)
    setArg(ans, 3, x)
    ans
  }
  )

nCompiler:::inEigenizeEnv(
  MakeIndexByVecCall <- function(index_slot, index_value, x) {
    ans <-  exprClass$new(isName = FALSE, isCall = TRUE, isAssign = FALSE,
                          name = "IndexByVec")
    setArg(ans, 1, literalIntegerExpr(index_slot-1))
    setArg(ans, 2, index_value)
    setArg(ans, 3, x)
    ans
  }
  )

nCompiler:::inEigenizeEnv(
  MakeIndexBySeqsCall <- function(index_slots, index_value, x) {
    ans <-  exprClass$new(isName = FALSE, isCall = TRUE, isAssign = FALSE,
                          name = "IndexBySeqs")
    setArg(ans, 1, literalIntegerExpr(length(index_slots)))
    iArg <- 2
    for(i in seq_along(index_slots)) {
      setArg(ans, iArg, literalIntegerExpr(index_slots[i]-1))
      setArg(ans, iArg + 1, index_value[[i]][[1]])
      setArg(ans, iArg + 2, index_value[[i]][[2]])
      iArg <- iArg + 3
    }
    setArg(ans, iArg, x)
    ans
  }
  )

nCompiler:::inEigenizeEnv(
  Bracket <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
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
    if(code$caller$name == "nFunction") {
      return(Bracket_to_StridedTensorMap(code, symTab, auxEnv, workEnv, handlingInfo))
    }
    ## Work: new handling here
    indexVec_inds <- integer()
    singleton_inds <- integer()
    indexSeq_inds <- integer()
    indexSeq_info <- list()
    indexVec_info <- list()
    drop <- code$args$drop$name
    if(!is.logical(drop))
      warning("Problem determining whether to drop dimensions.")
    
    if(isTRUE(nOptions("nimble") || isTRUE(nOptions("dropSingleSizes")))) {
      ## Here we imitate nimble's blockIndexInfo to determine
      ## scalar indices from ":" or c() (or possibly other?) cases
      ## but here what we do is replace the argument right now and
      ## then the rest of the logic below should work correctly.
      ## Currently in nimble there are three cases:
      ##   "3:3" Take the first "3"
      ##   A name with sizeExpr 1, which would be from a lifted c(scalar)
      ##   here it won't be lifted so it is like ":", take the first arg
      ##   There are also cases where a name is deparsed or not, and
      ##    I am not sure which arises when or what I might be missing.
      if(drop) {
        for(argInd in 2:(length(code$args)-1)) {
          if(code$args[[argInd]]$type$nDim > 0
             && !is.null(code$args[[argInd]]$type$knownSize)
             && code$args[[argInd]]$type$knownSize == 1) {
              if(code$args[[argInd]]$isCall) {
                if(code$args[[argInd]]$name %in% c(':', 'nC')) {
                  singleValueArg <- code$args[[argInd]]$args[[1]]$clone()
                  setArg(code, argInd, singleValueArg)
                }
              }
            }
        }
      }
    }

    for(argInd in 2:(length(code$args)-1)) {
      ind <- argInd - 1
      isBlank <- code$args[[argInd]]$isName & code$args[[argInd]]$name == ""
      if(!isBlank) {
        this_nDim <- code$args[[argInd]]$type$nDim
        if(this_nDim == 0) {
          if(drop)
            singleton_inds <- c(singleton_inds, ind)
          else {
            indexSeq_inds <- c(indexSeq_inds, ind)
            indexSeq_info <- c(indexSeq_info, list(c(code$args[[argInd]]$clone(),
                                                     code$args[[argInd]]$clone()))) # could flag this in some other way
          }
        } else if(this_nDim == 1) {
          if(code$args[[argInd]]$name == ':') {
            indexSeq_inds <- c(indexSeq_inds, ind)
            indexSeq_info <- c(indexSeq_info, list(c(code$args[[argInd]]$args[[1]]$clone(),
                                                     code$args[[argInd]]$args[[2]]$clone())))
          } else {
            indexVec_inds <- c(indexVec_inds, ind)
            indexVec_info <- c(indexVec_info, list(code$args[[argInd]]$clone()))
          }
        } else {
          warning("PROBLEM")
        }
      }
    }

    currentInput <- code$args[[1]]
    for(iInd in rev(seq_along(singleton_inds))) {
      thisInd <- singleton_inds[iInd]
      newExpr <- MakeIndexByScalarCall(thisInd, code$args[[thisInd+1]]$clone(), currentInput )
      bool <- indexSeq_inds > thisInd
      indexSeq_inds[bool] <- indexSeq_inds[bool] - 1
      bool <- indexVec_inds > thisInd
      indexVec_inds[bool] <- indexVec_inds[bool] - 1
      currentInput <- newExpr
    }
    for(iInd in rev(seq_along(indexVec_inds))) {
      thisInd <- indexVec_inds[iInd]
      newExpr <- MakeIndexByVecCall(thisInd, indexVec_info[[iInd]], currentInput )
      bool <- indexSeq_inds > thisInd
      indexSeq_inds[bool] <- indexSeq_inds[bool] - 1
      currentInput <- newExpr
    }
    if(length(indexSeq_inds)) {
      newExpr <- MakeIndexBySeqsCall(indexSeq_inds, indexSeq_info, currentInput)
    }

    newExpr$type <- code$type
    setArg(code$caller, code$callerArgID, newExpr)

    # index vecs have dim == 1 and are not ':'
    # singletons have dim == 0
    # index seqs have dim == 1 and are ':'
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
    has_times <- "times" %in% arg_names
    has_lengthout <- "length.out" %in% arg_names
    has_each <- "each" %in% arg_names

    call_to_use <-
      if(has_times) {
        if(has_lengthout) { #length.out moots times
          removeArg(code, which(arg_names == "times"))
          if(has_each) "repLenEach" else "repLen"
        } else {
          if(has_each) "repTimesEach" else "repTimes"
        }
      } else {
        if(has_lengthout) {
          if(has_each) "repLenEach" else "repLen"
        } else {
          if(has_each) "repEach" else "REMOVE"
        }
      }

    # This is the only use of Make_Length1_Tensor in the code
    # Can we replace it with CreateTensor?
    if(code$args[[1]]$type$nDim == 0) {
      insertExprClassLayer(code, 1, "Eigen::Make_Length1_Tensor")
    }

    if(call_to_use == "REMOVE") {
      code <- removeExprClassLayer(code)
      return(invisible(NULL))
    }

    code$name <- call_to_use
    ## Should we still try to control eval of x or will this be handled by Eigen internally?
    ##
    ## ## In C++, eval = true when the call to rep is part of a larger expression
    ## ## and rep's arg is a call to avoid costly repeated computation when
    ## ## reshaping and broadcasting the tensor.
    ## if (!code$caller$name %in% assignmentOperators && code$args[[1]]$isCall)
    ##   setArg(code, length(code$args) + 1, literalLogicalExpr())
    invisible(NULL)
  }
)

inEigenizeEnv(
 Colon <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    if (!code$caller$name == '[') { # To check: A '[' case might no longer ever come here.
      code$name <- "nSeqFromTo"
      promoteTypes(code) # redundant in this case
      return(invisible(NULL))
    }
    invisible(NULL)
  }
)

inEigenizeEnv(
  Seq <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    # In C++ we have
    # nSeqBy, nSeqLen, nSeqLenFrom, nSeqLenTo, nSeqFromTo, nSeqTo, nSeqSingle
    if(length(code$args) == 4) {
      stop(exprClassProcessingErrorMsg(
        code, 'seq() was coded with too many arguments.'
      ), call. = FALSE)
    }
    if(length(code$args) == 0) {
      insertArg(code, 1, copyExprClass(handlingInfo$i1), 'from')
      insertArg(code, 2, copyExprClass(handlingInfo$i1), 'to')
      code$name <- 'nSeqFromTo'
      return(invisible(NULL))
    }
    fromProvided <- 'from' %in% names(code$args)
    toProvided <- 'to' %in% names(code$args)
    byProvided <- 'by' %in% names(code$args)
    lengthProvided <- 'length.out' %in% names(code$args)

    if (length(code$args) == 1) {
      if(!byProvided) {
        if(fromProvided) {
          code$name <- "nSeqSingle"
      ## } else if(byProvided) {
      ##   code$name <- 'nSeqEmpty'
      ##   removeArg(code, 1) # equivalent to code$args <- list()
        } else if(toProvided) {
          code$name <- "nSeqTo"
        } else { # lengthProvided must be TRUE
         insertArg(code, 1, copyExprClass(handlingInfo$i1), 'from')
          code$name <- "nSeqLenFrom"
        }
        return(invisible(NULL))
      }
    }
    #  At this point, length(code$args) >= 2, or by was provided will end up in nSeqBy case.
    if(byProvided) {
      if(toProvided && lengthProvided) {
        code$name <- "nSeqByLenTo"
        promoteTypes(code)
      } else {
        if(!fromProvided) insertArg(code, 1, copyExprClass(handlingInfo$d1), 'from')
        if(lengthProvided) {
          code$name <- "nSeqByLenFrom"
          promoteTypes(code)
        } else {
          if(!toProvided) insertArg(code, 2, copyExprClass(handlingInfo$d1), 'to')
          code$name <- "nSeqBy"
          promoteTypes(code)
        }
      }
      return(invisible(NULL))
    }

    if(lengthProvided) {
      if(!fromProvided) {
        code$name <- "nSeqLenTo"
        promoteTypes(code)
      } else if(!toProvided) {
        code$name <- "nSeqLenFrom"
        promoteTypes(code)
      } else code$name <- "nSeqLen"
      return(invisible(NULL))
    }

    if(fromProvided && toProvided) {
      code$name <- "nSeqFromTo"
      promoteTypes(code) # redundant in this case
      return(invisible(NULL))
    }



    stop(exprClassProcessingErrorMsg(
      code, 'unexpected case of seq()'
    ), call. = FALSE)
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

nCompiler:::inEigenizeEnv(
  
  Diag <- function(code, symTab, auxEnv, workEnv, handlingInfo) {
    
    # handle "diag(x)" when x is a matrix and goal is to set or extract diagonal
    if(length(code$args) == 1) {
      if(code$args[[1]]$type$nDim == 2) {
        # default assumption is that "diag" is being used to extract diagonal
        DiagAsAssign <- FALSE
        # "diag" is being used as assignment if nDiag is LHS of "<-" caller
        if(!is.null(code$caller)) {
          if(code$caller$isAssign) {
            if(code$callerArgID == 1) {
              DiagAsAssign <- TRUE
            }
          }
        }
        # generated c++ code requires static_cast to assign to tensor output
        if(!DiagAsAssign) {
          # StaticCast wraps the original argument, will get c++ type from
          # it's type member during code generation
          e <- wrapExprClassOperator(code = code, funName = 'StaticCast', 
                                     isName = FALSE, isCall = TRUE, 
                                     isAssign = FALSE, type = code$type)
        }
        
        return(invisible(NULL)) # no further action necessary for eigenization
      }
    }
    
    #
    # handle "diag()" when used to create dense matrices
    #
    
    # goal: specify all arguments for c++ implementation of the diagonal matrix
    # creation operator "nDiag(x, nrow, ncol)"
    
    # define complete set of function arguments in canonical order
    canonicalArgs <- c('x', 'nrow', 'ncol')
    
    # extract existing argument names
    argNames <- names(code$args)
    
    # check if input is a condition that is not supported by base::diag()
    if(identical(argNames, 'ncol')) {
      stop(exprClassProcessingErrorMsg(
        code, 'Use of nDiag is not supported when ncol is the only argument'
      ))
    }
    
    # make sure all arguments make sense
    if(!all(argNames %in% canonicalArgs)) {
      badArgs <- argNames[!(argNames %in% canonicalArgs)]
      stop(exprClassProcessingErrorMsg(
        code, 
        paste('Unexpected arguments to nDiag:', paste(badArgs, collapse = ', '))
      ))
    }
    
    # handle "nDiag(x)"
    if(identical(argNames, 'x')) {
      
      # extract argument
      xArg <- code$args[[1]]
      
      # scalar input yields identity matrix with dimensions provided by "x"
      if(xArg$type$nDim == 0) {
        removeArg(expr = code, ID = 1)
        diagValue <- literalDoubleExpr(1.0)
        insertArg(expr = code, ID = 1, value = diagValue, name = 'x')
        insertArg(expr = code, ID = 1, value = xArg$clone(), name = 'nrow')
        insertArg(expr = code, ID = 1, value = xArg$clone(), name = 'ncol')
        argNames <- names(code$args)
      } 
      # vector input yields arbitrary diagonal matrix
      else if(xArg$type$nDim == 1) {
        nrowValue <- nParse(paste0('cppLiteral("MakeSquareDiag__{}")'))
        ncolValue <- nParse(paste0('cppLiteral("MakeSquareDiag__{}")'))
        insertArg(expr = code, ID = 1, value = nrowValue, name = 'nrow')
        insertArg(expr = code, ID = 1, value = ncolValue, name = 'ncol')
        argNames <- names(code$args)
      } 
      else {
        stop(exprClassProcessingErrorMsg(
          code,
          'The argument x in nDiag(x) must be a scalar or vector'
        ))
      }
      
    } # handle "nDiag(x)"
    
    # fill in default arguments if needed: processing order is important!
    if(length(argNames) < 3) {
      
      # default diagonal value is 1 if x is missing
      if(!('x' %in% argNames)) {
        diagValue <- literalDoubleExpr(1.0)
        insertArg(expr = code, ID = 1, value = diagValue, name = 'x')
        argNames <- names(code$args)
      }
      
      # create square matrix for "nDiag(x, nrow)"
      if(!('ncol' %in% argNames)) {
        nrowArg <- code$args[[which(argNames == 'nrow')]]
        insertArg(expr = code, ID = 1, value = nrowArg$clone(), name = 'ncol')
        argNames <- names(code$args)
      }
      
      # create 1-row matrix for "nDiag(x, ncol)"
      if(!('nrow' %in% argNames)) {
        rowValue <- literalIntegerExpr(1) #exprClass$new(isLiteral = TRUE, isName = FALSE,
#                                  isCall = FALSE, isAssign = FALSE, name = 1)
        insertArg(expr = code, ID = 1, value = rowValue, name = 'nrow')
        argNames <- names(code$args)
      }
      
    } # default arguments
    
    # permute arguments so they appear in canonical order for C++ calls
    o = as.numeric(factor(x = argNames, levels = canonicalArgs))
    reorderArgs(expr = code, perm = o)
    
    # recursively eigenize function arguments, for example, to properly eigenize
    # automatically generated "length(x)" default arguments
    for(i in 1:3) {
      compile_eigenize(code = code$args[[i]], symTab = symTab, auxEnv = auxEnv,
                       workEnv = workEnv)
    }
    
    invisible(NULL)
  }
)
