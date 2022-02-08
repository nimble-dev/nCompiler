
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
        ## TO-DO: Look up NCgenerators
        ##        and add to needed_[types? nClasses?]
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
            # code$nDim <- 0
            ##code$typeName <- 'double'
          } else {
            stop(paste0("variable '",
                        code$name,
                        "' has not been created yet."),
                 call.=FALSE) 
          }
      }
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

    ## Check for nFunctions or nClass methods (also nFunctions)
    if(is.null(opInfo)) {
      ## First we check if we are in an nClass and code$name is a method.
      obj <- NULL
      if(isNCgenerator(auxEnv$where)) {## We are in a class method
        obj <- auxEnv$where$public_methods[[code$name]]
        if(!is.null(obj)) {
          if(isNF(obj)) {
            opInfo <- operatorDefEnv[['nClass_method']]
          } else {
            stop(exprClassProcessingErrorMsg(code,
                                             paste0('method ', code$name, 'is being called, but it is not a nFunction.')),
                 call. = FALSE)
          }
        }
      }
      ## Next we check if code$name exists in the where.
      ## Note that if we are in a method, auxEnv$where will be the 
      ## generator, which is an environment.  However, we need
      ## to use nGet instead of exists and get. 
      if(is.null(obj)) {
        obj <- nGet(code$name, where = auxEnv$where)
        ## An nFunction will be transformed to
        ## have code$name 'nFunction'.
        if(!is.null(obj)) {
          if(isNF(obj)) {
            opInfo <- operatorDefEnv[['nFunction']]
            uniqueName <- NFinternals(obj)$uniqueName
            if(length(uniqueName)==0)
              stop(
                exprClassProcessingErrorMsg(
                  code,
                  paste0('nFunction ', code$name, 'is being called, ',
                         'but it is malformed because it has no internal name.')),
                call. = FALSE)
            if(is.null(auxEnv$needed_nFunctions[[uniqueName]])) {
              ## We could put the nFunction itself in the needed_nFunctions list,
              ## but we do not as a way to avoid having many references to R6 objects
              ## in a blind attempt to facilitate garbage collection based on past experience.
              ## Instead, we provide what is needed to look up the nFunction again later.
              auxEnv$needed_nFunctions[[uniqueName]] <- list(code$name, auxEnv$where)
            }
          } else
            stop(exprClassProcessingErrorMsg(
              code,
              paste0(code$name, ' is being used as a function, but it is not a nFunction.')),
              call. = FALSE)
        }
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
    switch(
      returnTypeCode,
      'double', ##1
      'integer', ##2
      'logical', ##3
      argType, ##4
      if(argType == 'logical') 'integer' else argType ##5
    )
  }
)

inLabelAbstractTypesEnv(
  setReturn_nDim <- function(handlingInfo, arg_nDim) {
    return_nDim <- handlingInfo[['return_nDim']]
    if (is.null(return_nDim)) return(arg_nDim)
    return_nDim
  }
)

## chainedCall
## nParse converts something like foo(a)(b) to chainedCall(foo(a), b),
##     (although there is no support for a function returning a function.)
## The relevant case is a$foo(b), which becomes chainedCall( a$foo, b).
## chainedCall handler:
## Recurse on first argument.
## Convert to nFunction( [result of recursion], b).  Will that work?
##
inLabelAbstractTypesEnv(
  ChainedCall <-
    function(code, symTab, auxEnv, handlingInfo) {
      inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv,
                                            handlingInfo)
      ## TO-DO: Add check that first arg is symbolNF
      code$name <- 'nFunction'
      if(!inherits(code$args[[1]]$type, "symbolNF"))
        stop(exprClassProcessingErrorMsg(
          code,
          'trying to make a function call from something that is not an nFunction.'
        ), call. = FALSE)
      code$type <- code$args[[1]]$type$returnSym$clone(deep = TRUE)
      if(length(inserts) == 0) NULL else inserts
    }
)

## DollarSign
## Recurse on LHS.  Expect nClass.
## Look-up RHS. Expect a member or method.
## Convert to nClass_member(LHS, RHS).
inLabelAbstractTypesEnv(
  DollarSign <-
    function(code, symTab, auxEnv, handlingInfo) {
      ## TO-DO: Check for exactly 2 arguments
      inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv,
                                            handlingInfo,
                                            useArgs = c(TRUE, FALSE))
      ## TO-DO: Check that LHS type is symbolNC or symbolNCgenerator
      ## TO-DO: Improve these error messages
      if(!inherits(code$args[[1]]$type, 'symbolNC'))
        stop(exprClassProcessingErrorMsg(
          code,
          'left-hand-side of `$` is not an nClass object.'
        ), call. = FALSE)
      if(!code$args[[2]]$isName)
        stop(exprClassProcessingErrorMsg(
          code,
          'right-hand-side of `$` is not a name.'
        ), call. = FALSE)

      ## 1. Check if RHS is a method
      ## 2. Check if RHS is a field
      method <- code$args[[1]]$type$NCgenerator$public_methods[[
        code$args[[2]]$name]]
      if(!is.null(method)) { ## Is RHS a method?
        returnSym <- symbolNF$new(
          name = '',
          returnSym = NFinternals(method)$returnSym$clone(deep = TRUE)
        )
        code$type <- returnSym
        ## Logically it might seem this should become ->method.
        ## However it appears in nFunction(->member(A, foo), x) for A->foo(x).
        ## In stage generateCpp, the nFunction packs the arguments after A->foo,
        ## so we mark that here as a member.
        code$name <- '->member'
        code$args[[2]]$name <- NFinternals(method)$cpp_code_name
          
      } else {  ## Is RHS a field?
        symbol <- NCinternals(code$args[[1]]$type$generator)$symbolTable$getSymbol(code$args[[2]]$name)
        if(is.null(symbol))
          stop(exprClassProcessingErrorMsg(
            code,
            'right-hand-side of `$` could not be found.'
          ), call. = FALSE)
        code$type <- symbol$clone(deep = TRUE)
        code$name <- '->member'
      }
      ## TO-DO: Handle special case of "new", or put it in
      ##        the nClass symbol table.
      if(length(inserts) == 0) NULL else inserts
    }
)

## a$b would become nClass_member(a, b)
## a$b$foo(x) would become chainedCall(`$`(`$`(a, b), foo), x)
##     which would become nFunction( nClass_member(nClass$member(a, b), foo) , x)

## Called by Generic_nFunction and Generic_nFunction_method
## This converts foo(x) to nFunction(foo, x)
## if foo is either an nFunction or a method of the current class
inLabelAbstractTypesEnv(
  convert_nFunction_or_method_AST <-
    function(code, obj) {
      nFunctionName <- code$name
      ## Note that the string `nFunction` matches the operatorDef entry.
      ## Therefore the change-of-name here will automatically trigger use of
      ## the 'nFunction' operatorDef in later stages.
      code$name <- 'nFunction'
      cpp_code_name <- NFinternals(obj)$cpp_code_name
      fxnNameExpr <- exprClass$new(name = cpp_code_name, isName = TRUE,
                                   isCall = FALSE, isLiteral = FALSE, isAssign = FALSE)
      ## We may need to add content to this symbol if
      ## necessary for later processing steps.
      fxnNameExpr$type <- symbolNF$new(name = nFunctionName)
      insertArg(code, 1, fxnNameExpr)
      ## TO-DO: Add error-trapping of argument types
      returnSym <- NFinternals(obj)$returnSym
      if(is.null(returnSym))
        stop(
          exprClassProcessingErrorMsg(
            code, paste('In convert_nFunction_or_method_AST: the nFunction (or method) ',
                        code$name, 
                        ' does not have a valid returnType.')
          ), call. = FALSE
        )
      code$type <- returnSym$clone() ## Not sure if a clone is needed, but it seems safer to make one.
      invisible(NULL)
    }
)

inLabelAbstractTypesEnv(
  Generic_nFunction <-
    function(code, symTab, auxEnv, handlingInfo) {
      inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv,
                                            handlingInfo)
      ## This is slightly wasteful because obj was already looked up
      ## in compile_labelAbstractTypes (which recurses down the syntax tree).
      ## If it becomes noticeably costly, we can arrange to save it from
      ## the first look-up.
      obj <- nGet(code$name, where = auxEnv$where)
      convert_nFunction_or_method_AST(code, obj)
      if(length(inserts) == 0) NULL else inserts
    }
)

inLabelAbstractTypesEnv(
  Generic_nClass_method <-
    function(code, symTab, auxEnv, handlingInfo) {
      inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv,
                                            handlingInfo)
      obj <-  auxEnv$where$public_methods[[code$name]]
      convert_nFunction_or_method_AST(code, obj)
      if(length(inserts) == 0) NULL else inserts
    }
)

inLabelAbstractTypesEnv(
  RecurseAndLabel <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    type <- setReturnType(handlingInfo, code$args[[1]]$type$type)
    nDim <- setReturn_nDim(handlingInfo, code$args[[1]]$type$nDim)
    code$type <- symbolBasic$new(type = type, nDim = nDim)
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  nChol <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    argType <- code$args[[1]]$type
    if(inherits(argType, 'symbolSparse')) {
      # Cholesky factor of a sparse matrix is a collection of matrices
      # TODO: do we need to specify arguments for the initializer?
      code$type <- symbolSparseCholesky$new(name = code$name)
    } else {
      # Cholesky factor of a dense matrix is a dense matrix (i.e., same type)
      type <- setReturnType(handlingInfo, argType$type)
      nDim <- setReturn_nDim(handlingInfo, argType$nDim)
      code$type <- symbolBasic$new(type = type, nDim = nDim)
    }
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  InitData <- function(code, symTab, auxEnv, handlingInfo) {
    ## TODO: handle 'init' arg
    ## defaults:
    ## n{Numeric|Integer|Logical}(length = 0, value = 0, init = TRUE)
    ## nMatrix(value = 0, nrow = 1, ncol = 1, init = TRUE, type = 'double')
    ## nArray(value = 0, dim = c(1, 1), init = TRUE, type = 'double')
    if (code$name %in% c('nNumeric', 'nInteger', 'nLogical'))
      inserts <- RecurseAndLabel(code, symTab, auxEnv, handlingInfo)
    else if (code$name %in% c('nMatrix', 'nArray')) {
      inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
      if ('type' %in% names(code$args))
        code$type <- symbolBasic$new(type = code$args[['type']]$name)
      else if ('value' %in% names(code$args))
        code$type <- symbolBasic$new(type = code$args[['value']]$type$type)
      else
        code$type <- symbolBasic$new(type = 'double')
      if (code$name == 'nMatrix') code$type$nDim <- 2
      else {
        dim_provided <- 'dim' %in% names(code$args)
        nDim_provided <- 'nDim' %in% names(code$args)
        if (!(dim_provided || nDim_provided))
          code$type$nDim <- 2 ## default is a 1x1 array
        else {
          if (dim_provided) {
            if (inherits(code$args[['dim']], 'exprClass') &&
                  code$args[['dim']]$isCall && code$args[['dim']]$name == 'nC') {
              nDim_from_dim <- length(code$args[['dim']]$args)
              ## 'dim' must be of type integer
              code$args[['dim']]$type$type <- 'integer'
            } else {
              dim_nDim <- code$args[['dim']]$type$nDim
              if (dim_nDim > 1)
                stop(
                  exprClassProcessingErrorMsg(
                    code,
                    paste("In labelAbstractTypes handler InitData: 'dim'",
                          "argument must be scalar- or vector-valued.")
                  ), call. = FALSE
                )
              if (dim_nDim != 0 && !nDim_provided)
                stop(
                  exprClassProcessingErrorMsg(
                    code,
                    paste("In labelAbstractTypes handler InitData: if 'nDim'",
                          "argument is not provided, 'dim' argument must",
                          "be a scalar-valued expression or a call to nC().")
                  ), call. = FALSE
                )
              nDim_from_dim <- if (dim_nDim == 0) 1 else -1
            }
          }
          if (nDim_provided) {
            nDim <- code$args[['nDim']]$name
            if (!code$args[['nDim']]$isLiteral || !is.numeric(nDim))
              stop(
                exprClassProcessingErrorMsg(
                  code,
                  paste('In labelAbstractTypes handler InitData:',
                        "'nDim' argument must be a numeric literal.")
                ), call. = FALSE
              )
            if (dim_provided && nDim_from_dim != -1 && nDim != nDim_from_dim) {
              warning("In labelAbstractTypes handler InitData: both 'nDim' ",
                      "and 'dim' provided as args to '", code$name,
                      "' but they do not match. Using 'dim'.")
              nDim <- nDim_from_dim
            }
            code$type$nDim <- nDim
          } else {
            ## dim_provided must be TRUE and nDim_from_dim is not -1
            code$type$nDim <- nDim_from_dim
          }
        }
      }
    }
    invisible(inserts)
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
  Which <- function(code, symTab, auxEnv, handlingInfo) {
    if (length(code$args) != 1)
      stop(
        exprClassProcessingErrorMsg(
          code, paste("In labelAbstractTypes handler Which: 'arr.ind' and",
                      "'useNames' args are not implemented for 'which'.")
        ), call. = FALSE)
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    if (code$args[[1]]$type$type != 'logical')
      stop(
        exprClassProcessingErrorMsg(
          code, paste0("In labelAbstractTypes handler Which: The argument to ",
                       "'which' must be logical, but got",
                       code$args[[1]]$type$type, '.')
        ), call. = FALSE)
    ## TODO: remove the nDim restriction
    if (code$args[[1]]$type$nDim > 1)
      stop(
        exprClassProcessingErrorMsg(
          code,
          paste0("In labelAbstractTypes handler Which: handling of arg",
                 "with dimension greater than 1 not yet implemented.")
        ), call. = FALSE)
    code$type <- symbolBasic$new(name = code$args[[1]]$name,
                                 nDim = 1, type = 'integer')
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  For <- function(code, symTab, auxEnv, handlingInfo) {
    if(length(code$args) != 3)
      stop(exprClassProcessingErrorMsg(
        code,
        paste('In labelAbstractTypes handler For:',
              'expected 3 arguments to a for-loop')), call. = FALSE)
    ## first handle type of the indexing variable
    if(!inherits(code$args[[2]], 'exprClass'))
      stop(
        exprClassProcessingErrorMsg(
          code, paste('In labelAbstractTypes handler For: expected the index',
                      'range to be an expression (exprClass).')
        ), call. = FALSE
      )

    inserts <- compile_labelAbstractTypes(code$args[[2]], symTab, auxEnv)

    code$args[[1]]$type <-
      symbolBasic$new(name = code$args[[1]]$name,
                      nDim = 0, type = code$args[[2]]$type$type)

    if (!symTab$symbolExists(code$args[[1]]$name, inherits = TRUE))
      if (TRUE) 
        symTab$addSymbol(code$args[[1]]$type)

    ## Now the 3rd arg, the body of the loop, can be processed
    inserts <- c(inserts, compile_labelAbstractTypes(code$args[[3]], symTab, auxEnv))
    ## I think there shouldn't be any inserts returned since the body should be a bracket expression.
    return(if (length(inserts) == 0) invisible(NULL) else inserts)
  }
)

inLabelAbstractTypesEnv(
  ParallelFor <- function(code, symTab, auxEnv, handlingInfo) {
    if(length(code$args) != 5) 
      stop(exprClassProcessingErrorMsg(
        code,
        paste('In labelAbstractTypes handler ParallelFor:',
              'expected 5 arguments to a parallel_for-loop')), call. = FALSE)
    ## first handle type of the indexing variable
    if(!inherits(code$args[[2]], 'exprClass'))
      stop(
        exprClassProcessingErrorMsg(
          code, paste('In labelAbstractTypes handler ParallelFor: expected the index',
                      'range to be an expression (exprClass).')
        ), call. = FALSE
      )
    
    inserts <- compile_labelAbstractTypes(code$args[[2]], symTab, auxEnv)
    
    code$args[[1]]$type <-
      symbolBasic$new(name = code$args[[1]]$name,
                      nDim = 0, type = code$args[[2]]$type$type)
    
    ## If index is unknown, create it in typeEnv and in the symTab (old nimble comment)
    if (!symTab$symbolExists(code$args[[1]]$name, inherits = TRUE))
      if (TRUE) 
        symTab$addSymbol(code$args[[1]]$type)

    ## Now the 3rd arg, the body of the loop, can be processed
    inserts <- c(inserts, compile_labelAbstractTypes(code$args[[3]], symTab, auxEnv))
    ## I think there shouldn't be any inserts returned since the body should be a bracket expression.
    return(if (length(inserts) == 0) invisible(NULL) else inserts)
  }
)

inLabelAbstractTypesEnv(
  ParallelReduce <- function(code, symTab, auxEnv, handlingInfo) {
    if (length(code$args) != 3)
      stop(exprClassProcessingErrorMsg(
        code,
        paste('In labelAbstractTypes handler ParallelReduce:',
              'expected 3 arguments but got', length(code$args))),
        call. = FALSE)
    ## process the initial value
    inserts <- compile_labelAbstractTypes(code$args[[3]], symTab, auxEnv)
    if (code$args[[3]]$type$nDim != 0)
      stop(exprClassProcessingErrorMsg(
        code,
        paste('In labelAbstractTypes handler ParallelReduce:',
              'initial value for parallel_reduce should be scalar but got',
              ' nDim = ', code$args[[3]]$type$nDim)),
        call. = FALSE)
    if (isFALSE(code$args[[3]]$isLiteral))
      stop(exprClassProcessingErrorMsg(
        code,
        paste('In labelAbstractTypes handler ParallelReduce:',
              'initial value for parallel_reduce must be a literal')),
        call. = FALSE)
    ## process the reduce operator
    if (isTRUE(code$args[[1]]$isLiteral)) {
      if (!is.character(code$args[[1]]$name))
        stop(exprClassProcessingErrorMsg(
          code,
          paste('In labelAbstractTypes handler ParallelReduce:',
                'do not know how to use a reduce operator of type',
                typeof(code$args[[1]]$name))),
          call. = FALSE)
      code$args[[1]]$isLiteral <- FALSE
      code$args[[1]]$isCall <- TRUE
    }
    ## give reduce operator the same return type as the initial value
    ## TODO: Maybe symbolNF is the right type for the reduction op.
    code$args[[1]]$type <-
      symbolBasic$new(name = code$args[[1]]$name,
                      nDim = 0, type = code$args[[3]]$type$type)
    ## finish by processing the vector arg
    inserts <- c(inserts, compile_labelAbstractTypes(code$args[[2]], symTab, auxEnv))
    if (code$args[[2]]$type$nDim != 1)
      stop(exprClassProcessingErrorMsg(
        code,
        paste('In labelAbstractTypes handler ParallelReduce:',
              'expected the second argument to be a vector but got nDim = ',
              code$args[[2]]$type$nDim)),
        call. = FALSE)
    code$type <- symbolBasic$new(name = code$name, nDim = 0,
                                 type = code$args[[3]]$type$type)
    return(if (length(inserts) == 0) invisible(NULL) else inserts)
  }
)

inLabelAbstractTypesEnv(
  Colon <- function(code, symTab, auxEnv, handlingInfo, recurse = TRUE) {
    if (length(code$args) != 2)
      stop(
        exprClassProcessingErrorMsg(
          code, paste(
            "In sizeCgetolonOperator: Problem determining ",
            "size for ':' without two arguments."
          )
        ), call. = FALSE
      )    

    inserts <-
      if (recurse)
        recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    else list()

    ## this isn't quite right... if the first arg is a whole number double, :
    ## returns an integer regardless of the second arg's type
    arg1_type <- code$args[[1]]$type$type
    code_type <- if (arg1_type == 'logical') 'integer' else arg1_type
    code$type <- symbolBasic$new(nDim = 1, type = code_type)

    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  Seq <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    if (length(code$args) == 0 ||
          ## seq(by = x) always returns 1
          (length(code$args) == 1 && 'by' %in% names(code$args))) {
      code$type <- symbolBasic$new(nDim = 0, type = 'integer')
    } else {
      arg1_type <- code$args[[1]]$type$type
      code_type <- if (arg1_type == 'logical') 'integer' else arg1_type
      ## TODO: What about when from and to have same value?
      code$type <- symbolBasic$new(nDim = 1, type = code_type)
    }
    inserts
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
      resultSymbolType <- arithmeticOutputSymbol(arg)
      resultType <- resultSymbolType$new(nDim = argType$nDim,
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
      
      # except for matrix-vector like ops, tensor args must have same nDims
      if(all(c(a1Type$nDim, a2Type$nDim) > 1)) {
        if(a1Type$nDim != a2Type$nDim) {
          stop(exprClassProcessingErrorMsg(
            code,
            paste('sizeBinaryCwise called with non-conformable tensors with ',
                  'dimensions ', a1Type$nDim, ', ', a2Type$nDim, '.', sep ='')
          ),
          call. = FALSE)
        }
      }
      
      resultScalarType <- arithmeticOutputType(
        a1Type$type, a2Type$type, handlingInfo$returnTypeCode
      )
      resultSymbolType <- arithmeticOutputSymbol(a1, a2)
      resultType <- resultSymbolType$new(nDim = nDim,
                                         type = resultScalarType)
      code$type <- resultType
      ##code$typeName <- class(resultType)[1]
      invisible(NULL)
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
  Bracket <- function(code, symTab, auxEnv, handlingInfo) {
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
              "In Bracket: '", obj$name,
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
            "In Bracket: number of indexing arguments does not match the dimension of '",
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
              "In Bracket: non-scalar indexing expressions other than ':' not currently supported."
            ), call. = FALSE
          )

      if (index_args[[i]]$name != '') {
        ## not a call resulting in non-scalar other than ':'
        if (is.null(index_args[[i]]$type) || ## missing index nDim info
              is.null(index_args[[i]]$type$nDim)) ## would this ever happen?
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0("In Bracket: '", index_args[[i]]$name,
                     "' has no dimension.")
            ), call. = FALSE
          )
        ## TODO: allow for scalar logicals?
        if (index_args[[i]]$type$type == 'logical') ## index logical
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0("In Bracket: '", index_args[[i]]$name,
                     "' is a logical which is not allowed when indexing.")
            ), call. = FALSE
          )
        if (index_args[[i]]$type$nDim > 1) ## bad index nDim
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0(
                "In Bracket: the dimension of '", index_args[[i]]$name,
                " is ", index_args[[i]]$type$nDim, " but must be 0 or 1."
              )
            ), call. = FALSE
          )
        if (nDim == 0 && index_args[[i]]$type$nDim != 0) ## indexing a scalar with non-scalar
          stop(
            exprClassProcessingErrorMsg(
              code,
              paste0(
                "In Bracket: '", obj$name,
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
      # If we're indexing a scalar, any drop arg provided is ignored.
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
            'In Bracket: the drop argument must be a literal.'
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
      setArg(code, 'drop', drop_arg, add = TRUE)
    }

    # TODO: double check the assumption that output will always be a 
    # symbolBasic type as it is understood today.  this is handling for the
    # subsetting operator, [], but will it always be subsetted to a symbolBasic
    # type?
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
      # see if the returned object differs from the nFunction's return type
      if(!identical(class(auxEnv$returnSymbol), class(code$type))) {
        warning(exprClassProcessingErrorMsg(
          code,
          "Object type for return() does not match the nFunction's return type."
        ),
        call. = FALSE)
      }
      if(auxEnv$returnSymbol$type != code$type$type) {
        warning(exprClassProcessingErrorMsg(
          code, 
          paste0(
            "Scalar type (", code$type$type, ") for return() does not match ",
            "the nFunction's return type (", auxEnv$returnSymbol$type, ").",
            sep = ''
          )
        ),
        call. = FALSE)
      }
      if((!is.null(auxEnv$returnSymbol$nDim)) || !(is.null(code$type$nDim))) {
        if(auxEnv$returnSymbol$nDim != code$type$nDim) {
          warning(exprClassProcessingErrorMsg(
            code, 
            paste0(
              "Dimension (", code$type$nDim, ") for return() does not match ",
              "the nFunction's dimension (", auxEnv$returnSymbol$nDim, ").",
              sep = ''
            )
          ),
          call. = FALSE)
        }
      }
      invisible(insertions)
    }
)

inLabelAbstractTypesEnv(
  ## recurse and set code type via setReturnType()
  VectorReturnType <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    returnType <- setReturnType(handlingInfo, code$args[[1]]$type$type)
    # TODO: double check the assumption that output will always be a 
    # symbolBasic type as it is understood today.  Is a Vector always a dense 
    # vector?  Or do we really need a separate handler for vectors stored in 
    # different datastructures, such as SparseVectors, hashmaps, or lists?
    code$type <- symbolBasic$new(nDim = 1, type = returnType)
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  ## recurse and set code type via setReturnType()
  MatrixReturnType <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    returnType <- setReturnType(handlingInfo, code$args[[1]]$type$type)
    # TODO: double check the assumption that output will always be a 
    # symbolBasic type as it is understood today.  Is a Vector always a dense 
    # vector?  Or do we really need a separate handler for vectors stored in 
    # different datastructures, such as SparseVectors, hashmaps, or lists?
    code$type <- symbolBasic$new(nDim = 2, type = returnType)
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  ## recurse and set code type via setReturnType()
  nMul <- function(code, symTab, auxEnv, handlingInfo) {
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    returnType <- setReturnType(handlingInfo, code$args[[1]]$type$type)
    # R's matrix-multiplication promotes all output types to matrices; also 
    # important b/c matrix multiplication could be used to implement an outer 
    # product between two input vectors, which returns a matrix
    resDim <- 2
    # TODO: double check the assumption that output will always be a 
    # symbolBasic type as it is understood today.  Is a Vector always a dense 
    # vector?  Or do we really need a separate handler for vectors stored in 
    # different datastructures, such as SparseVectors, hashmaps, or lists?
    code$type <- symbolBasic$new(nDim = resDim, type = returnType)
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  ## recurse and use the nth argument's type as the return type
  ArgReturnType <- function(code, symTab, auxEnv, handlingInfo) {
    # determine which code$arg entry will be used to specify the return type
    argTypeInd <- handlingInfo[['argTypeInd']]
    if(is.null(argTypeInd)) {
      stop(exprClassProcessingErrorMsg(
        code,
        'Need to specify the input argument to use as a return type.'
      ), call. = FALSE)
    }
    # recurse to determine argument types
    inserts <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    # extract the return type
    code$type <- code$args[[argTypeInd]]$type
    invisible(inserts)
  }
)

inLabelAbstractTypesEnv(
  asSparse <- function(code, symTab, auxEnv, handlingInfo) {
    if(length(code$args) > 2) {
      stop(exprClassProcessingErrorMsg(
        code,
        'trying to make an ambiguous input sparse.'
      ), call. = FALSE)
    }
    # determine object's natural type
    insertions <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    argType <- code$args[[1]]$type
    # extract or construct a sparse type for argument
    if(inherits(argType, 'symbolSparse')) {
      code$type <- argType
    } else if(inherits(argType, 'symbolBasic')) {
      code$type = symbolSparse$new(
        name = argType$name,
        type = argType$type,
        isArg = argType$isArg,
        isRef = argType$isRef,
        nDim = argType$nDim,
        size = argType$size
      )
    } else {
      stop(exprClassProcessingErrorMsg(
        code,
        'unable to determine sparse type for input'
      ), call. = FALSE)
    }
    invisible(NULL)
  }
)

inLabelAbstractTypesEnv(
  asDense <- function(code, symTab, auxEnv, handlingInfo) {
    if(length(code$args) > 1) {
      stop(exprClassProcessingErrorMsg(
        code,
        'trying to make an ambiguous input sparse.'
      ), call. = FALSE)
    }
    # determine object's natural type
    insertions <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    argType <- code$args[[1]]$type
    # extract or construct a sparse type for argument
    if(!inherits(argType, 'symbolSparse')) {
      code$type <- argType
    } else if(inherits(argType, 'symbolSparse')) {
      code$type = symbolBasic$new(
        name = argType$name,
        type = argType$type,
        isArg = argType$isArg,
        isRef = argType$isRef,
        nDim = argType$nDim,
        size = argType$size
      )
    } else {
      stop(exprClassProcessingErrorMsg(
        code,
        'unable to determine dense type for input'
      ), call. = FALSE)
    }
    invisible(NULL)
  }
)

inLabelAbstractTypesEnv(
  Transpose <- function(code, symTab, auxEnv, handlingInfo) {
    # validate usage of transpose function
    if(length(code$args) > 1) {
      stop(exprClassProcessingErrorMsg(
        code,
        'trying to take the transpose of an ambiguous input.'
      ), call. = FALSE)
    }
    # determine argument's type
    insertions <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    argType <- code$args[[1]]$type
    # validate input of transpose function
    if(argType$nDim > 2) {
      stop(exprClassProcessingErrorMsg(
        code,
        'cannot transpose an object with more than two dimensions.'
      ), call. = FALSE)
    }
    # create type object
    returnType <- setReturnType(handlingInfo, code$args[[1]]$type$type)
    # TODO: double check the assumption that output will always be a 
    # symbolBasic type as it is understood today.  Is a Vector always a dense 
    # vector?  Or do we really need a separate handler for vectors stored in 
    # different datastructures, such as SparseVectors, hashmaps, or lists?
    code$type <- symbolBasic$new(nDim = 2, type = returnType)
    invisible(insertions)
  }
)

inLabelAbstractTypesEnv(
  nEigen <- function(code, symTab, auxEnv, handlingInfo) {
    if(length(code$args) > 1) {
      stop(exprClassProcessingErrorMsg(
        code,
        'trying to eigen decompose an ambiguous input.'
      ), call. = FALSE)
    }
    # determine object's natural type
    insertions <- recurse_labelAbstractTypes(code, symTab, auxEnv, handlingInfo)
    argType <- code$args[[1]]$type
    # extract or construct a sparse type for argument
    if(!inherits(argType, 'symbolSparse')) {
      browser()
      code$type <- xxx # TODO: instantiate a new EigenDecomp object
    } else if(inherits(argType, 'symbolSparse')) {
      stop(exprClassProcessingErrorMsg(
        code,
        'eigendecompositions not supported for sparse matrices.'
      ), call. = FALSE)
    } else {
      stop(exprClassProcessingErrorMsg(
        code,
        'unable to handle input type.'
      ), call. = FALSE)
    }
    invisible(NULL)
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
arithmeticOutputType <- function(t1, t2 = NULL, returnTypeCode = NULL) {
  if (!is.null(returnTypeCode) && returnTypeCode %in% c(1L, 2L, 3L))
    return(names(returnTypeCodes)[[returnTypeCode]])
  if (t1 == 'double') return('double')
  if (!is.null(t2) && t2 == 'double') return('double')
  if (t1 == 'integer') return('integer')
  if (!is.null(t2) && t2 == 'integer') return('integer')
  if (!is.null(returnTypeCode) && returnTypeCode == 5L) return('integer')
  return('logical')
}

## promote symbol with the most dense storage type, symbolBasic > symbolSparse
arithmeticOutputSymbol <- function(s1, s2 = NULL, returnTypeCodes) {
  if(inherits(s1$type, 'symbolSparse') && inherits(s2$type, 'symbolSparse')) {
    return(symbolSparse)
  } else {
    return(symbolBasic)
  }
}
