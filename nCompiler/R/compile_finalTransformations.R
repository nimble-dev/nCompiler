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
                           code, symTab, auxEnv, info) {
    copyVars <- eval(nDeparse(code$args[[4]], toR = TRUE), 
                     envir = auxEnv$closure)
    shareVars <- eval(nDeparse(code$args[[5]], toR = TRUE),
                      envir = auxEnv$closure)
    ## Look for a mangled argument name in nameSubList.
    ## It is unfortunate to have to do this here instead of earlier
    ## when other names are replaced, but here the names are given
    ## as character objects (potentially from R evaluation).
    copyVars <- replace_nameSubList(copyVars, auxEnv$nameSubList)
    shareVars <- replace_nameSubList(shareVars, auxEnv$nameSubList)

    code$args[[4]] <- copyVars ## This is no longer an exprClass
    code$args[[5]] <- shareVars ## Ditto
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
    ## loop_body_expr will be parallel_loop_body(var1, var2, etc.)
    loop_body_expr <- exprClass$new(name = loop_body_name,
                                    isCall = TRUE,
                                    isName = FALSE, isLiteral = FALSE, isAssign = FALSE)
    allVars <- c(copyVars, shareVars)
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
    setArg(parallel_expr, 2, loop_body_expr)
    setArg(code$caller, code$callerArgID, parallel_expr)
    NULL
  }
)

inFinalTransformationsEnv(
  ParallelFor <- function(code, symTab, auxEnv, info) {
    ParallelExpr('parallel_for', 'parallel_loop_body', 'parallelContent', code,
                 symTab, auxEnv, info)
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

    ## remove the vector and initial value arg and save for later
    vector_arg <- removeArg(code, 2)
    ## TODO: don't remove the init arg unless isTRUE(code$caller$isAssign)
    init_arg <- removeArg(code, 2)
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
    ## TODO: was hoping there was a generateCpp handler for this... use .size()
    ## directly instead?
    length_call <- setArg(colon, 2, exprClass$new(name = 'length',
                                                  isLiteral = FALSE,
                                                  isCall = TRUE,
                                                  isName = FALSE,
                                                  isAssign = FALSE))
    setArg(length_call, 1, vector_arg)
    ## make the vector an argument of the reduce op and index it
    reduce_op <- code$args[[3]]
    setArg(reduce_op, 1, copyExprClass(vector_arg))
    insertIndexingBracket(reduce_op, 1, copyExprClass(index_arg))
    ## the other arg to the reduce op is a local aggregation var called 'val__'
    val <- setArg(reduce_op, 2, exprClass$new(name = 'val__', isName = TRUE,
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

    ## The class name is hard-wired expecting only a single case of parallel
    ## reduce content.
    ## TO-DO: generalize the name with unique identifier.
    ParallelExpr('parallel_reduce',
                 'parallel_reduce_body parallel_reduce_inst__',
                 'parallelReduceContent', code, symTab, auxEnv, info)

    if (isTRUE(code$caller$isAssign)) {
      assign_argID <- code$caller$callerArgID
      parallel_reduce_expr <- removeArg(code$caller, 2)
      ## the instantiation of the parallel_reduce_body object will happen
      ## before the call to parallel_reduce
      instance_expr <- removeArg(parallel_reduce_expr, 2)
      ## the second argument should be the initial value provided by the user
      setArg(instance_expr, 2, init_arg)
      ## TODO: this doesn't have the effect I hoped for... is there a way to
      ## add type annotation to a call (such as object instantiation)?
      instance_expr$type <- symbolBase$new(name = 'parallel_reduce_body',
                                           type = 'parallel_reduce_body')
      ## the parallel_reduce_body instance name is the second arg to the
      ## parallel_reduce call (note that this isn't an exprClass)
      setArg(parallel_reduce_expr, 2,
             exprClass$new(name = 'parallel_reduce_inst__', isName = TRUE,
                           isCall = FALSE, isLiteral = FALSE, isAssign = FALSE))
      ## move the parallel_reduce_body instantiation to before the assignment
      insertArg(code$caller$caller, assign_argID, instance_expr)
      ## put the parallel_reduce call between the parallel_reduce_body
      ## instantiation and the assign
      insertArg(code$caller$caller, assign_argID + 1, parallel_reduce_expr)
      ## now the RHS of the assign is the aggregation value after the
      ## parallel_reduce
      setArg(code$caller, 2,
             nParse(paste0('cppLiteral("parallel_reduce_inst__.value__")')))
    }
    NULL
  }
)
