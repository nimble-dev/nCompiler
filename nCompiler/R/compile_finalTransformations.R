finalTransformationsEnv <- new.env()
finalTransformationsEnv$.debug <- FALSE

inFinalTransformationsEnv <- function(expr) {
  expr <- substitute(expr)
  eval(expr, envir = finalTransformationsEnv)
}


inFinalTransformationsEnv(
  replace_nameSubList <- function(vars, nameSubList) {
    for(iv in seq_along(vars)) {
      ## Look for a mangled argument name in nameSubList.
      ## It is unfortunate to have to do this here instead of earlier
      ## when other names are replaced, but here the names are given
      ## as character objects (potentially from R evaluation).
      thisVar <- nameSubList[[vars[iv] ]]
      if(isTRUE(is.null(thisVar)))
        thisVar <- vars[iv]
      else
        thisVar <- deparse(thisVar)
      vars[iv] <- thisVar
    }
    vars
  }
)

inFinalTransformationsEnv(
  ParallelExpr <- function(parallel_expr_name, loop_body_name, auxEnv_field,
                           code, symTab, auxEnv, allVars, info) {
    auxEnv[[auxEnv_field]] <- c(auxEnv[[auxEnv_field]], code)
    ##  parallel_for(blocked_range<size_t>(0, n), parallel_loop_body(x));
    ## blocked_range_expr will be blocked_range<int>(start, end + 1)
    blocked_range_expr <- exprClass$new(name = "tbb::blocked_range<int>",
                                        isCall = TRUE,
                                        isName = FALSE, isLiteral = FALSE, isAssign = FALSE)
    ## first arg is start
    setArg(blocked_range_expr, 1, copyExprClass(code$args[[2]]$args[[1]]))
    ## end_plus_one_expr will be end + 1
    end_plus_one_expr <- exprClass$new(name = '+', isCall = TRUE,
                                       isName = FALSE, isLiteral = FALSE, isAssign = FALSE)
    setArg(end_plus_one_expr, 1, copyExprClass(code$args[[2]]$args[[2]]))
    setArg(end_plus_one_expr, 2, exprClass$new(name = 1, isLiteral = TRUE,
                                               isCall = FALSE, isName = FALSE, isAssign = FALSE))
    ## second arg is end + 1
    setArg(blocked_range_expr, 2, end_plus_one_expr)
    ## parallel_for_expr will be parallel_for( <blocked_range_expr>, <loop_body_expr>)
    parallel_expr <- exprClass$new(name = parallel_expr_name, isCall = TRUE,
                                   isName = FALSE, isLiteral = FALSE,
                                   isAssign = FALSE)
    setArg(parallel_expr, 1, blocked_range_expr)
    ## loop_body_expr will be parallel_loop_body_<id>(var1, var2, etc.)
    loop_body_expr <- exprClass$new(name = loop_body_name,
                                    isCall = TRUE,
                                    isName = FALSE, isLiteral = FALSE, isAssign = FALSE)
    for(iv in seq_along(allVars)) {
      ## Look for a mangled argument name in nameSubList.
      ## It is unfortunate to have to do this here instead of earlier
      ## when other names are replaced, but here the names are given
      ## as character objects (potentially from R evaluation).
      thisVar <- allVars[iv]
      setArg(loop_body_expr, iv, 
             exprClass$new(name = thisVar, 
                           isCall = FALSE, isName = TRUE, isLiteral = FALSE, isAssign = FALSE))
    }
    if(length(code$aux$localMethods) || isTRUE(code$aux$liftedSelf))
      setArg(loop_body_expr, iv+1, nParse('cppLiteral("*this")'))
    setArg(parallel_expr, 2, loop_body_expr)
    setArg(code$caller, code$callerArgID, parallel_expr)

    nThreads_arg <- removeArg(code, 'nThreads')
    setArg(parallel_expr, 3, nThreads_arg)
    NULL
  }
)

inFinalTransformationsEnv(
  ParallelFor <- function(code, symTab, auxEnv, info) {
    nThreads_arg <- removeArg(code, 'nThreads')
    ## TODO: not sure if we will do more work on arg matching such that
    ## code$args[[4]] and code$args[[5]] will always exist and correspond to `copyVars` and `shareVars`
    ## respectively for `parallel_for`. But if so, we might rework/simplify this.
    ## Check for "" is because it seems valid to do: `parallel_for(i,1:5,{},}` or `parallel_for(i,1:5,{},,}`.
    if(!'copyVars' %in% names(code$args) || nDeparse(code$args[['copyVars']]) == "") {
      copyVars <- NULL
    } else copyVars <- eval(nDeparse(code$args[['copyVars']], toR = TRUE), 
                     envir = auxEnv$where)
    if(!'shareVars' %in% names(code$args) || nDeparse(code$args[['shareVars']]) == "") {
      shareVars <- NULL
    } else shareVars <- eval(nDeparse(code$args[['shareVars']], toR = TRUE),
                     envir = auxEnv$where)
    if(any(shareVars %in% copyVars))
              stop(exprClassProcessingErrorMsg(
      code,
      paste('In finalTransformations handler ParallelExpr:',
            'arguments `shareVars` and `copyVars` to `parallel_for`',
            'both contain the same variable')), call. = FALSE)
    ## Look for a mangled argument name in nameSubList.
    ## It is unfortunate to have to do this here instead of earlier
    ## when other names are replaced, but here the names are given
    ## as character objects (potentially from R evaluation).
    copyVars <- replace_nameSubList(copyVars, auxEnv$nameSubList)
    shareVars <- replace_nameSubList(shareVars, auxEnv$nameSubList)

    ## Add default vars:
    ## Any argument, class member variable, nFunction local variable by default is shared.
    ## Any local variable in the loop body by default is copied.
    vars <- all.vars(code$args[[3]]$Rexpr)
    nms <- all.names(code$args[[3]]$Rexpr)

    vars2 <- vars[!vars %in% c("self", nDeparse(code$args[[1]]))]  # Last item is index variable.
    # Omit vars only referenced by `self`.
    # This reliance on ordering of result of `all.names` feels fragile.
    if("self" %in% nms) {
      nonSelfNames <- nms[-(which(nms == "self") + 1)]
    } else nonSelfNames <- nms
    vars2 <- vars2[vars2 %in% nonSelfNames]
    
    inST <- vars2 %in% c(symTab$getSymbolNames(), symTab$parentST$getSymbolNames())
    defaultCopyVars <- code$aux$localVars  # Local vars in for loop body.
    defaultCopyVars <- defaultCopyVars[!defaultCopyVars %in% shareVars]
    defaultShareVars <- vars2[inST]     # All other vars.
    defaultShareVars <- defaultShareVars[!defaultShareVars %in% code$aux$localVars]
    defaultShareVars <- defaultShareVars[!defaultShareVars %in% copyVars]

    ## Find nClass objects (if methods are used; members would have been found above).
    nms <- nms[!nms %in% c(vars, "self")]
    objects <- nms[nms %in% c(symTab$getSymbolNames(), symTab$parentST$getSymbolNames())]
    ## Make sure the items are actually nClass objects.
    if(length(objects))  
      objects <- objects[sapply(objects, 
         function(x) !is.null(symTab$getSymbol(x)$NCgenerator) || !is.null(symTab$parentST$getSymbol(x)$NCgenerator))]
    
    shareVars <- unique(c(shareVars, defaultShareVars, objects))
    copyVars  <- unique(c(copyVars, defaultCopyVars))
      
    ## NULL cannot hold a position in `code$args`.
    if(is.null(copyVars)) copyVars <- character(0)
    if(is.null(shareVars)) shareVars <- character(0)
        
    code$args[[4]] <- copyVars ## This is no longer an exprClass
    code$args[[5]] <- shareVars ## Ditto
    setArg(code, 6, nThreads_arg)
    names(code$args)[4:6] <- c('copyVars','shareVars','nThreads')

      
    ## We have already found the local method calls and set the `opInfo$case` to be 'nClass_method_in_lifted',
    ## such that C++ calls to the method will be handled by cppOutput handler.
    ## The following checks for such methods in a different way (so perhaps worry an inconsistency could arise).
    ## Perhaps there is a better way to get this information.
    ## This information is used to ensure that the self object is passed into the lifted TBB code.
    ## Currently we don't use the actual identified `localMethods` values, just whether there are any.
    nms <- all.names(code$args[[3]]$Rexpr)
    code$aux$localMethods <- nms[nms %in% c(names(auxEnv$where$public_methods), names(auxEnv$where$private_methods))]
    if("self" %in% nms)  # This is needed to catch case of `self$<field>` but is redundant with the above line for `self$<method>`.
      code$aux$liftedSelf <- TRUE
    code$aux$class <- auxEnv$where$classname

    code$aux$bodyName <- parallelForBodyLabelMaker()
      
    ParallelExpr('parallel_for', code$aux$bodyName, 'parallelContent', code,
                 symTab, auxEnv, allVars = c(copyVars, shareVars), info)
  }
)

inFinalTransformationsEnv(
  ParallelReduce <- function(code, symTab, auxEnv, info) {
    ## copyVars <- eval(nDeparse(code$args[[4]], toR = TRUE), 
    ##                  envir = auxEnv$closure)
    ## shareVars <- eval(nDeparse(code$args[[5]], toR = TRUE),
    ##                   envir = auxEnv$closure)
    ## Look for a mangled argument name in nameSubList.
    ## It is unfortunate to have to do this here instead of earlier
    ## when other names are replaced, but here the names are given
    ## as character objects (potentially from R evaluation).
    ## copyVars <- replace_nameSubList(copyVars, auxEnv$nameSubList)
    ## shareVars <- replace_nameSubList(shareVars, auxEnv$nameSubList)
    ## code$args[[4]] <- copyVars ## This is no longer an exprClass
    ## code$args[[5]] <- shareVars ## Ditto

    ## We have already found the local method calls and set the `opInfo$case` to be 'nClass_method_in_lifted',
    ## such that C++ calls to the method will be handled by cppOutput handler.
    ## The following checks for such methods in a different way (so perhaps worry an inconsistency could arise).
    ## Perhaps there is a better way to get this information.
    ## This information is used to ensure that the self object is passed into the lifted TBB code.
    ## Currently we don't use the actual identified `localMethods` values, just whether there are any.
    if ("self" %in% all.names(code$Rexpr))
      code$aux$liftedSelf <- TRUE
    nm <- code$args[[1]]$Rexpr
    if(is.character(nm) && nm %in% c(names(auxEnv$where$public_methods), names(auxEnv$where$private_methods)))
      code$aux$localMethods <- nm else code$aux$localMethods <- character(0)
      
    code$aux$class <- auxEnv$where$classname
    code$aux$bodyName <- parallelReduceBodyLabelMaker()
    ## remove the vector, initial value, and nThreads args and save for later
    vector_arg <- removeArg(code, 'object')
    init_arg <- removeArg(code, 'init')
    nThreads_arg <- removeArg(code, 'nThreads')  
    ## add an index var
    index_arg <- exprClass$new(name = 'i__', isName = TRUE, isCall = FALSE,
                               isLiteral = FALSE, isAssign = FALSE)
    index_arg$type <- symbolBasic$new(name = index_arg$name, nDim = 0,
                                      type = 'integer')
    insertArg(code, 1, index_arg)

    if (!symTab$symbolExists(index_arg$name, inherits = TRUE))
        symTab$addSymbol(index_arg$type)

    ## ParallelFor will expect the : op to be in the AST and the for loop range
    ## will come in handy when constructing to C++ call to parallel_reduce().
    colon <- insertArg(code, 2, exprClass$new(name = ':', isCall = TRUE,
                                              isName = FALSE,
                                              isLiteral = FALSE,
                                              isAssign = FALSE))
    setArg(colon, 1, exprClass$new(name = 1, isLiteral = TRUE, isCall = FALSE,
                                   isName = FALSE, isAssign = FALSE))
    size_expr <- setArg(
      colon, 2, nParse(paste0('cppLiteral("', vector_arg$name, '.size()")')))
    ## make the vector an argument of the reduce op and index it
    reduce_op <- code$args[[3]]
    inc <- 0  
    if(reduce_op$name == 'chainedCall')
      inc <- 1
          
    setArg(reduce_op, 1+inc, copyExprClass(vector_arg))
    insertIndexingBracket(reduce_op, 1+inc, copyExprClass(index_arg))
    ## the other arg to the reduce op is a local aggregation var called 'val__'
    val <- setArg(reduce_op, 2+inc, exprClass$new(name = 'val__', isName = TRUE,
                                              isCall = FALSE,
                                              isLiteral = FALSE,
                                              isAssign = FALSE))
    val$type <- symbolBasic$new(name = val$name,
                                nDim = 0,
                                type = init_arg$type$type)
    ## the body of the for loop assigns the result from the reduce op to
    ## 'val__'
    assign_expr <- setArg(code, 3, newAssignmentExpression())
    setArg(assign_expr, 1, copyExprClass(val))
    setArg(assign_expr, 2, reduce_op)

    ## Put the vector arg and an aggregation variable called 'value__' into the
    ## AST as literals. These will be noncopyVars in the cppParallelReduceBodyClass.
    setArg(code, 4, exprClass$new(name = vector_arg$name, isName = FALSE, isCall = FALSE,
                                  isLiteral = TRUE, isAssign = FALSE))
    value_name <- 'value__'
    setArg(code, 5, exprClass$new(name = value_name,
                                  isName = FALSE, isCall = FALSE,
                                  isLiteral = TRUE, isAssign = FALSE))
    ## add value__ to the symbolTable
    if (!symTab$symbolExists(value_name, inherits = TRUE)) {
      value_type <- symbolBasic$new(name = value_name, nDim = 0,
                                    type = init_arg$type$type)
      symTab$addSymbol(value_type)
    }

    instName <- sub("_body", "_inst__", code$aux$bodyName)

    inputVar <-  eval(nDeparse(code$args[[4]], toR = TRUE), 
                   envir = auxEnv$where)
    outputVar <- eval(nDeparse(code$args[[5]], toR = TRUE),
                    envir = auxEnv$where)
    ## Look for a mangled argument name in nameSubList.
    ## It is unfortunate to have to do this here instead of earlier
    ## when other names are replaced, but here the names are given
    ## as character objects (potentially from R evaluation).
    inputVar <- replace_nameSubList(inputVar, auxEnv$nameSubList)

    nms <- all.vars(code$Rexpr)
    nClass_object <- nms[nms %in% c(symTab$getSymbolNames(), symTab$parentST$getSymbolNames()) &
                  !nms %in% c(inputVar, "self")]
    if(length(nClass_object) > 1)
            stop(exprClassProcessingErrorMsg(
                code$Rexpr,
                paste('In finalTransformations handler ParallelReduce:',
                      'Unexpectedly found multiple objects in parallel_reduce reduction function')),
                call. = FALSE)
    ## Make sure the items are actually nClass objects.
    if(length(nClass_object) && is.null(symTab$getSymbol(nClass_object)$NCgenerator) &&
       is.null(symTab$parentST$getSymbol(nClass_object)$NCgenerator))
      nClass_object <- character(0)
    
    ## TODO: consider reworking how we handle these items as it doesn't map cleanly onto the
    ## `args`, which was really set up for `parallel_for`.
    code$args[[4]] <- inputVar  ## This is no longer an exprClass
    code$args[[5]] <- outputVar ## Ditto
    code$args[[6]] <- nClass_object    ## Ditto
    setArg(code, 7, nThreads_arg)
    names(code$args)[4:7] <- c('input','output','nClass_object','nThreads')    

    ParallelExpr('parallel_reduce',
                 paste(code$aux$bodyName, instName, collapse = ' '),
                 'parallelReduceContent', code, symTab, auxEnv, allVars = c(inputVar, outputVar, nClass_object), info)
  
    outerCall <- code$caller
    level <- 1  
    while(!isTRUE(outerCall$isAssign) && !outerCall$name == "return" && !outerCall$name == "{") {
      outerCall <- outerCall$caller  # Find correct level to insert the reduction code.
      level <- level + 1
      if(level > 100)  ## Not sure what situation could lead to this.
        stop(exprClassProcessingErrorMsg(
           code$Rexpr,
           paste('In finalTransformations handler ParallelReduce:',
                 'Unexpected levels of nesting in use of parallel_reduce')),
           call. = FALSE)
    }
    if(outerCall$name == "{")  # No assignment or return. Handle these gracefully but no known use cases.
      if(outerCall$args[[1]]$name == 'parallel_reduce') {  # A lone `parallel_reduce()` 
        ## Add layer so that parallel_reduce call is within a call so can be handled
        ## as other cases are handled.
        code$caller <- wrapInExprClass(code$caller, "{")
        setCaller(code, code$caller$args[[1]], 1)
      } else outerCall <- outerCall$args[[1]]  # A case like `3 + parallel_reduce()`
      
    code$aux$init <- init_arg
    assign_argID <- outerCall$callerArgID  # Always 1, presumably.

    ## Check for `tbb::blocked_range<int>` handles cases such as `parallel_reduce() + parallel_reduce()`,
    ## distinguishing which one is currently being processed.
    reduce_argID <- which(sapply(code$caller$args, function(x) 
        x$name == "parallel_reduce" && x$args[[1]]$name == "tbb::blocked_range<int>"))
      
    if(length(reduce_argID) != 1)  
      stop(exprClassProcessingErrorMsg(
         code$Rexpr,
         paste('In finalTransformations handler ParallelReduce:',
               'Unable to process code - missing or too many uses of parallel_reduce')),
         call. = FALSE)
    parallel_reduce_expr <- removeArg(code$caller, reduce_argID)
    ## the instantiation of the parallel_reduce_body object will happen
    ## before the call to parallel_reduce
    instance_expr <- removeArg(parallel_reduce_expr, 2)
    ## the second argument should be the initial value provided by the user
    setArg(instance_expr, 2, init_arg)
    ## TODO: this doesn't have the effect I hoped for... is there a way to
    ## add type annotation to a call (such as object instantiation)?
    instance_expr$type <- symbolBase$new(name = code$aux$bodyName,
                                         type = 'parallel_reduce_body')
    ## the parallel_reduce_body instance name is the second arg to the
    ## parallel_reduce call (note that this isn't an exprClass)
    insertArg(parallel_reduce_expr, 2,
           exprClass$new(name = instName, isName = TRUE,
                         isCall = FALSE, isLiteral = FALSE, isAssign = FALSE))
    ## move the parallel_reduce_body instantiation to before the assignment    
    insertArg(outerCall$caller, assign_argID, instance_expr)
    ## put the parallel_reduce call between the parallel_reduce_body
    ## instantiation and the assign
    insertArg(outerCall$caller, assign_argID + 1, parallel_reduce_expr)
    ## now the RHS of the assign is the aggregation value after the
    ## parallel_reduce
    insertArg(code$caller, reduce_argID,
              nParse(paste0('cppLiteral("', instName, '.value__")')))
    NULL
  }
)
