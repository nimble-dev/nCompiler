#' nFunction Derivatives
#' 
#' Computes the value, Jacobian, and Hessian of a given  
#' \code{nFunction} or compiled \code{nClass} method.  
#' 
#' @param nFxn a call to a compiled or uncompiled \code{nFunction} method with
#'   arguments included.
#' @param order an integer vector with values within the set {0, 1, 2},
#'   corresponding to whether the function value, Jacobian, and Hessian should
#'   be returned respectively.  Defaults to \code{c(0, 1, 2)}.
#' @param dropArgs a vector of integers specifying any arguments to \code{nFxn}
#'   that derivatives should not be taken with respect to.  For example,
#'   \code{dropArgs = 2} means that the second argument to \code{nFxn} will not
#'   have derivatives taken with respect to it.  Defaults to an empty vector
#'   and is ignored if \code{wrt} is provided.
#' @param wrt a vector of either: names of function arguments to take
#'   derivatives with respect to or integers for the positions in the flattened
#'   input vector of the elements with respect to which to differentiate.  If
#'   left empty, derivatives will be taken with respect to all arguments to
#'   \code{nFxn}.
#' @param NC An nClass generator (returned from a call to \link{nClass})
#'   which has a method corresponding to the one found in \code{nFxn}.  Only
#'   required when \code{nFxn} is a compiled method call from a generic or full
#'   interface.
#' @details Derivatives for uncompiled nFunctions are calculated using the
#'   \code{numDeriv} package.  If this package is not installed, an error will
#'   be issued.  Derivatives for matrix valued arguments will be returned in
#'   column-major order.
#' 
#' @return a \code{list} with elements \code{value}, \code{gradient}, and
#'   \code{hessian}.
#' 
#' @export
nDerivs <- function(nFxn = NA, order = c(0,1,2), dropArgs = NA, wrt = NULL,
                    NC = NULL) {
  fxnEnv <- parent.frame()
  fxnCall <- match.call()
  if(is.null(fxnCall[['order']])) fxnCall[['order']] <- order
  derivFxnCall <- fxnCall[['nFxn']]

  if (!is.call(derivFxnCall))
    stop("'nFxn' argument should be a call to an nFunction or compiled nClass method.")

  fxn <- try(eval(derivFxnCall[[1]], fxnEnv))

  if (isNF(fxn))
    nDerivs_nf(fxnCall = derivFxnCall, order = order, dropArgs = dropArgs,
               wrt = wrt, fxnEnv = fxnEnv)
  else if (length(derivFxnCall[[1]]) == 3 &&
             deparse(derivFxnCall[[1]][[1]]) %in% c('$', '[[')) {

    ## this could be an nClass or a compiled nClass with full interface
    nClass_obj <- try(eval(derivFxnCall[[1]][[2]], envir = fxnEnv))

    if (!isNC(nClass_obj))
      stop(paste0(
        "nDerivs does not know how to use the object ",
        deparse(fxnCall[[2]][[1]][[2]]), "."
      ))

    nDerivs_full(fxnCall = derivFxnCall, order = order, dropArgs = dropArgs,
                 wrt = wrt, fxnEnv = fxnEnv, NC = NC)

  } else if (is.call(derivFxnCall[[1]]) &&
               derivFxnCall[[1]][[1]] == 'method') {

    loadedObjEnv <- try(eval(derivFxnCall[[1]][[2]], envir = fxnEnv))

    if (!is.loadedObjectEnv(loadedObjEnv))
      stop(paste0(
        "nDerivs does not know how to use the object ",
        deparse(derivFxnCall[[1]][[2]]), "."
      ))

    nDerivs_generic(fxnCall = derivFxnCall, order = order, dropArgs = dropArgs,
                    wrt = wrt, fxnEnv = fxnEnv, loadedObjEnv = loadedObjEnv,
                    NC = NC)
  } else
    stop(paste0(
      "nDerivs does not know how to use the object ",
      deparse(derivFxnCall[[1]]), "."
    ))
}

#' Computes the value, Jacobian, and Hessian of a given  
#' \code{nFunction} method.
#' 
#' @param nFxn a call to a compiled or uncompiled \code{nFunction} method with
#'   arguments included.
#' @param dropArgs a vector of integers specifying any arguments to \code{nFxn}
#'   that derivatives should not be taken with respect to.  For example,
#'   \code{dropArgs = 2} means that the second argument to \code{nFxn} will not
#'   have derivatives taken with respect to it.  Defaults to an empty vector.
#' @param wrt a character vector of either: names of function arguments to take
#'   derivatives with respect to.  If left empty, derivatives will be taken
#'   with respect to all arguments to \code{nFxn}.
#' @param NC An nClass generator (returned from a call to \link{nClass})
#'   which has a method corresponding to the one found in \code{nFxn}. Only
#'   required when \code{nFxn} is a compiled method call from a generic or full
#'   interface.
#'
#' @return an integer vector of the positions in the flattened input of the
#'   elements with respect to which to differentiate \code{nFxn}.
#' 
#' @export
setup_wrt <- function(nFxn = NA, dropArgs = NA, wrt = NULL, NC = NULL) {
  fxnCall <- match.call()$nFxn

  if (is.call(fxnCall) && length(fxnCall) == 3 &&
        deparse(fxnCall[[1]]) %in% c('$', '[[')) {

    nClass_obj <- try(eval(fxnCall[[2]], envir = parent.frame()))

    if (!isNC(nClass_obj))
      stop(paste0(
        "setup_wrt does not know how to use the object ",
        deparse(fxnCall[[2]]), "."
      ))
    if (is.null(NC))
      stop(paste(
        "'setup_wrt' was called for a method from a full nClass interface but",
        "the 'NC' argument is NULL."
      ))
    if (!isNCgenerator(NC))
      stop(paste(
        "The 'NC' argument to 'setup_wrt' must be an nClass generator (returned",
        "from a call to 'nClass')."))

    fxn <- eval(fxnCall, envir = parent.frame())
    fxnName <- fxnCall[[3]]
    if (!is.character(fxnName)) fxnName <- deparse(fxnName)
    nf <- NC_get_Cpub_class(NC)$public_methods[[fxnName]]
    fxnArgs <- NFinternals(nf)$argSymTab$symbols

  } else if (is.call(fxnCall) && fxnCall[[1]] == 'method') {

    loadedObjEnv <- try(eval(fxnCall[[2]], envir = parent.frame()))

    if (!is.loadedObjectEnv(loadedObjEnv))
      stop(paste0(
        "setup_wrt does not know how to use the object ",
        deparse(fxnCall[[2]]), "."
      ))

    if (is.null(NC))
      stop(paste(
        "'setup_wrt' was called for a method from a generic nClass interface but",
        "the 'NC' argument is NULL."
      ))
    if (!isNCgenerator(NC))
      stop(paste(
        "The 'NC' argument to 'setup_wrt' must be an nClass generator (returned",
        "from a call to 'nClass')."))

    fxnName <- fxnCall[[3]]

    nf <- NC_get_Cpub_class(NC)$public_methods[[fxnName]]
    if (is.null(nf))
      stop(paste0(
        "The 'NC' argument provided to 'setup_wrt' has no public method named ",
        fxnName, "."))

    fxnArgs <- NFinternals(nf)$argSymTab$symbols

  } else {
    if (is.call(fxnCall))
      stop(paste0("setup_wrt expected the 'nFxn' arg to be either an ",
                  "nFunction or a compiled nClass method but instead got the ",
                  "the call ", deparse(fxnCall), "."))
    if (!isNF(nFxn))
      stop(paste0("setup_wrt does not know how to use the object ",
                  deparse(fxnCall, ".")))
    fxnName <- deparse(nFxn)
    fxnArgs <- NFinternals(nFxn)$argSymTab$symbols
  }
  
  setup_wrt_internal(wrt = wrt, fxnArgs = fxnArgs, fxnName = fxnName,
                     dropArgs = dropArgs)
}

setup_wrt_internal <- function(wrt, fxnArgs, fxnName, dropArgs = NA) {
  ## TODO: why return -1?
  ## if(all(is.na(wrt))){
  ##   return(-1) 
  ## }
  if (is.null(wrt)) {
    wrt <- names(fxnArgs)
    if (!is.na(dropArgs) && is.character(dropArgs))
      wrt <- wrt[!wrt %in% dropArgs]
  } else if (!is.character(wrt)) {
    wrt <- deparse(wrt)
  }
  ## convert 'x[2]' to quote(x[2]).
  wrt_code <- lapply(wrt,
                     function(x) parse(text = x, keep.source = FALSE)[[1]])

  ## convert quote(x[2]) to quote(x)
  wrt_names <- lapply(wrt_code,
                      function(x) if(is.name(x)) x else x[[2]])

  wrt_name_strings <- as.character(wrt_names)
  fxnArg_names <- names(fxnArgs)
  wrtMatchArgs <- which(fxnArg_names %in% wrt_name_strings)
  argNameCheck <- wrt_name_strings %in% fxnArg_names
  ## Compare wrt args to actual function args and make sure no erroneous args
  ## are present.
  ## TODO: sometimes called by nDerivs(), but sometimes by setup_wrt()
  if (!all(argNameCheck)) stop('Incorrect names passed to wrt argument of
                                     nDerivs: ', fxnName, 
                               ' does not have arguments named: ',
                               paste(wrt_name_strings[!argNameCheck], 
                                     collapse = ', ' ), '.')
  ## Make sure all wrt args have type declarations (i.e., are not just names without types)
  if (inherits(fxnArgs[[1]], 'symbolBase'))
    arg_symbols <- fxnArgs
  else {
    arg_symbols <- lapply(fxnArgs, argType2symbol)
  }
  nameCheck <- sapply(wrtMatchArgs, function(i) class(arg_symbols[[i]]))
  if (any(nameCheck == 'name')) stop('Derivatives of ', fxnName, ' being taken 
                                    WRT an argument that does not have a valid type.')
  ## Make sure all wrt args have type double.
  doubleCheck <- sapply(wrtMatchArgs, function(i)
    arg_symbols[[i]]$type == 'double')
  if (!all(doubleCheck)) stop('Derivatives of ', fxnName, 
                              ' being taken WRT an argument that does not
                                    have type double().')
  ## Make sure that all wrt arg dims are < 2.
  fxnArgsDims <- sapply(wrtMatchArgs, function(i) arg_symbols[[i]]$nDim)
  names(fxnArgsDims) <- names(fxnArgs)[wrtMatchArgs]
  if (any(fxnArgsDims > 2)) stop('Derivatives cannot be taken WRT an argument
                                with dimension > 2')
  ## Determine sizes of each function arg.
  fxnArgsDimSizes <- lapply(arg_symbols, function(x) {
    if(!is.numeric(x$size)) stop('Sizes of arguments to nFunctions must be
                           explictly specified (e.g. x = double(1, 4)) in order
                           to take derivatives.')
    x$size
  })
  ## Same as above sizes, except that matrix sizes are reported as nrow*ncol
  ## instead of c(nrow, ncol).
  fxnArgsTotalSizes <- sapply(fxnArgsDimSizes, prod)
  fxnArgsTotalSizes <- c(0, fxnArgsTotalSizes)
  ## fxnArgsIndexVector is a named vector with the starting index of each
  ## argument to the nFxn,
  ## if all arguments were flattened and put into a single vector.
  ## E.g. if nFxn has arguments x = double(2, c(2, 2)), y = double(1, 3), 
  ## and z = double(0),
  ## then fxnArgsIndexVector will be:    
  ##   x  y  z
  ##   1  5  8
  fxnArgsIndexVector <- cumsum(fxnArgsTotalSizes) + 1
  names(fxnArgsIndexVector) <- c(names(fxnArgsIndexVector)[-1], '')
  fxnArgsIndexVector <- fxnArgsIndexVector[-length(fxnArgsIndexVector)]
  wrtArgsIndexVector <- c()
  for (i in seq_along(wrt_name_strings)) {
    hasIndex <- is.call(wrt_code[[i]]) && wrt_code[[i]][[1]] == '['
    if (fxnArgsDims[wrt_name_strings[i]] == 0) ## scalar case
      wrtArgsIndexVector <- c(wrtArgsIndexVector, 
                              fxnArgsIndexVector[wrt_name_strings[i]])
    else if (fxnArgsDims[wrt_name_strings[i]] == 1) { ## vector case
      if (hasIndex) {
        if (length(tail(wrt_code[[i]], -2)) != 1)
          stop(paste0('Incorrect indexing provided for wrt ',
                      'argument to nDerivs(): ', wrt[i]))
        if (is.blank(wrt_code[[i]][[3]]))
          argIndices <- 1:fxnArgsTotalSizes[wrt_name_strings[i]]
        else {
          argIndices <- eval(wrt_code[[i]][[3]])
          if (any(argIndices < 1 | argIndices > fxnArgsTotalSizes[wrt_name_strings[i]]))
            stop(paste0('Index too large (or < 0) provided in wrt argument: ',
                        wrtArgs[i], ' for derivatives of ', fxnName))
        }
        wrtArgsIndexVector <- c(wrtArgsIndexVector, 
                                fxnArgsIndexVector[wrt_name_strings[i]] +
                                  argIndices - 1)
      }
      else{
        wrtArgsIndexVector <- c(wrtArgsIndexVector, 
                                fxnArgsIndexVector[wrt_name_strings[i]] +
                                  0:(fxnArgsTotalSizes[wrt_name_strings[i]] - 1))
      }
    }
    else if(fxnArgsDims[wrt_name_strings[i]] == 2) { ## matrix case
      if (hasIndex){
        if (length(tail(wrt_code[[i]], -2)) != 2)
          stop(paste0('Incorrect indexing provided for wrt ',
                      'argument to nDerivs(): ', wrtArgs[i]))
        if (is.blank(wrt_code[[i]][[3]]))
          argIndicesRows <- 1:fxnArgsDimSizes[[wrt_name_strings[i]]][1]
        else {
          argIndicesRows <- eval(wrt_code[[i]][[3]])
          if (any(argIndicesRows < 1 | argIndicesRows > fxnArgsDimSizes[[wrt_name_strings[i]]][1]))
            stop(paste0('A row index that is too large (or < 0) ',
                        ' was provided in wrt argument: ',
                        wrtArgs[i], ' for derivatives of ', fxnName))
        }
        if (is.blank(wrt_code[[i]][[4]]))
          argIndicesCols <- 1:fxnArgsDimSizes[[wrt_name_strings[i]]][2]
        else { 
          argIndicesCols <- eval(wrt_code[[i]][[4]])
          if(any(argIndicesCols < 1 | argIndicesCols > fxnArgsDimSizes[[wrt_name_strings[i]]][2]))
            stop(paste0('A column index that is too large (or < 0) ',
                        ' was provided in wrt argument: ',
                        wrtArgs[i], ' for derivatives of ', fxnName))
          
        }
        ## Column major ordering
        for(col in argIndicesCols){
          wrtArgsIndexVector <- c(wrtArgsIndexVector, 
                                  fxnArgsIndexVector[wrt_name_strings[i]] + 
                                    (col - 1)*
                                    fxnArgsDimSizes[[wrt_name_strings[i]]][1] +
                                    argIndicesRows - 1)
        }
      }
      else{
        wrtArgsIndexVector <- c(wrtArgsIndexVector,  
                                fxnArgsIndexVector[wrt_name_strings[i]] +
                                  0:(fxnArgsTotalSizes[wrt_name_strings[i]] - 1))
      }
    }
  }
  return(unname(wrtArgsIndexVector))
}

calcDerivs_internal <- function(func, X, order, resultIndices ) {
  if(!require('numDeriv', quietly = TRUE))
    stop("The 'numDeriv' package must be installed to use derivatives in
         uncompiled nFunctions.")

  hessianFlag <- 2 %in% order
  gradientFlag <- 1 %in% order
  ## When called for a model$calculate, valueFlag will always be 0 here
  ## because value will be obtained later (if requested) after restoring
  ## model variables.
  valueFlag <- 0 %in% order
  if(hessianFlag) {
    ## If hessians are requested, derivatives taken using numDeriv's genD() 
    ## function.  After that, we extract the various derivative elements and 
    ## arrange them properly.
    derivList <- genD(func, X)
    if(valueFlag) outVal <- derivList$f0 
    if(gradientFlag) outGrad <- derivList$D[,1:derivList$p, drop = FALSE]
    outHessVals <- derivList$D[,(derivList$p + 1):dim(derivList$D)[2],
                               drop = FALSE]
    outHess <- array(NA, dim = c(derivList$p, derivList$p, length(derivList$f0)))
    singleDimMat <- matrix(NA, nrow = derivList$p, ncol = derivList$p)
    singleDimMatUpperTriDiag <- upper.tri(singleDimMat, diag = TRUE)
    singleDimMatLowerTriDiag <- lower.tri(singleDimMat)
    for(outDim in seq_along(derivList$f0)){
      singleDimMat[singleDimMatUpperTriDiag] <- outHessVals[outDim,]
      singleDimMat[singleDimMatLowerTriDiag] <- t(singleDimMat)[singleDimMatLowerTriDiag]
      outHess[,,outDim] <- singleDimMat
    }
  } else
    if(gradientFlag){
      ## If gradients are requested, derivatives taken using numDeriv's jacobian() 
      ## function.  After that, we extract the various derivative elements and 
      ## arrange them properly.
      outVal <- func(X) 
      outGrad <- jacobian(func, X)
    } else 
      if(valueFlag)
        outVal <- func(X)
  
  outList <- list()
  if(!missing(resultIndices)) {
    if(gradientFlag) outGrad <- outGrad[, resultIndices, drop=FALSE]
    if(hessianFlag) outHess <- outHess[resultIndices, resultIndices, , drop=FALSE]
  }
  if(valueFlag) outList$value <- outVal
  if(gradientFlag) outList$gradient <- outGrad
  if(hessianFlag) outList$hessian <- outHess
  return(outList)
}

nDerivs_nf <- function(fxnCall = NULL, order = c(0,1,2), dropArgs = NA,
                       wrt = NULL, fxnEnv = parent.frame()) {
  nf <- eval(fxnCall[[1]], envir = fxnEnv)

  ## standardize the fxnCall arguments
  fxnCall <- match.call(nf, fxnCall)

  fA <- formals(nf)

  # get the user-supplied arguments to the nFunction
  fxnCall_args <- as.list(fxnCall)[-1]

  ## Get the value of the args
  fxnArgs <- lapply(fxnCall_args,
                    function(x) 
                      eval(x, envir = fxnEnv))

  arg_symbols <- NFinternals(nf)$argSymTab$symbols

  ## check that supplied args have sizes we expect from the symbol table
  for (arg_name in names(fA)) {
    if (!identical(as.integer(arg_symbols[[arg_name]]$size),
                   as.integer(nDim(fxnArgs[[arg_name]]))))
      stop(paste0(
        "Error:  the '", arg_name, "' argument you provided to the nFunction '",
        deparse(fxnCall[[1]]), "' in nDerivs() does not have",
        ' the right sizes. Expected (',
        paste(arg_symbols[[arg_name]]$size, collapse = ', '), ') but got (',
        nDim(fxnArgs[[arg_name]]), ').'
      ))
  }

  unique_dims <- lapply(arg_symbols, '[[', 'size')

  ## Get product of dimensions, which we call size
  ## ordered by fxnArgs
  unique_sizes <- sapply(unique_dims, prod)

  scalar_index_objects <- lapply(unique_dims,
                                 function(x)
                                   array(1:prod(x), dim = x))

  if (is.null(wrt)) {
    wrt <- names(fA)
    if (!is.na(dropArgs) && is.character(dropArgs))
      wrt <- wrt[!wrt %in% dropArgs]
  }

  if (is.character(wrt)) {

    ## convert 'x[2]' to quote(x[2]).
    wrt_code <- lapply(wrt,
                       function(x) parse(text = x, keep.source = FALSE)[[1]])

    ## convert quote(x[2]) to quote(x)
    wrt_names <- lapply(wrt_code,
                        function(x) if(is.name(x)) x else x[[2]])

    wrt_name_strings <- as.character(wrt_names)
    
    ## Get unique names and track indices from wrt to unique names
    wrt_unique_names <- unique(wrt_names)

    if(!all(wrt_unique_names %in% names(fA))){
      stop('Error:  the wrt argument to nDerivs() contains names that are not
         arguments to the nFxn argument.')
    }
    
    wrt_names_orig_indices <- match(wrt_names, wrt_unique_names)
    wrt_unique_name_strings <- as.character(wrt_unique_names)
    
    eval_env <- new.env()
    ## iterate over names in any wrt.

    current_x_index <- 1
    fxnArgs_assign_code <- list()
    get_init_values_code <- list()
    result_x_indices_by_wrt <- list()
    for(i in seq_along(wrt_unique_names)) {
      ## Below, x represents the input argument to func
      this_unique_wrt_string <- wrt_unique_name_strings[i]
      ##
      dims <- nDim(fxnArgs[[this_unique_wrt_string]])
      ##
      assign(this_unique_wrt_string, array(1:prod(dims), dim = dims), envir = eval_env)
      
      ## which wrt arguments use this wrt variable
      i_wrt_orig <- which(this_unique_wrt_string == wrt_name_strings)     

      flat_indices <- lapply(wrt_code[i_wrt_orig], ## quote(x[2]) and so on for any wrt using x
                             function(wrt_code_) {
                               eval(wrt_code_, envir = eval_env)
                             })
      unique_flat_indices <- unique(unlist(flat_indices))
      ## I is this_unique_wrt_string
      ## J is an element of unique_flat_indices
      ## K is a running element of x

      x_indices <- current_x_index - 1 + 1:length(unique_flat_indices)
      
      fxnArgs_assign_code[[i]] <- mapply(
        function(jval, kval)
          substitute(fxnArgs[[ I ]][J] <<- x[K], list(I = this_unique_wrt_string,
                                                      J = jval,
                                                      K = kval)),
        unique_flat_indices,
        x_indices)
      get_init_values_code[[i]] <- mapply(
        function(jval, kval)
          substitute(currentX[K] <- fxnArgs[[ I ]][J], list(I = this_unique_wrt_string,
                                                            J = jval,
                                                            K = kval)),
        unique_flat_indices,
        x_indices)

      result_x_indices <- lapply(flat_indices,
                                 function(fi) x_indices[match(fi, unique_flat_indices)]
                                 )
      result_x_indices_by_wrt[i_wrt_orig] <- result_x_indices
      
      current_x_index <- current_x_index + length(unique_flat_indices)
    }
    fxnArgs_assign_code <- unlist(fxnArgs_assign_code, recursive = FALSE)
    get_init_values_code <- unlist(get_init_values_code, recursive = FALSE)
    result_x_indices_all <- unlist(result_x_indices_by_wrt)

    length_x <- current_x_index - 1
    currentX <- numeric(length_x)
    do.call("{", get_init_values_code)
    ## equivalent to:
    ##  for(i in 1:length_x) {
    ##      eval(get_init_values_code[[i]])
    ##  }

  } else if (is.numeric(wrt)) {
    ## create a column-wise flattened vector of the inputs
    flat_input <- unlist(sapply(fxnArgs, as.vector))
    wrt <- as.integer(wrt)
    currentX <- flat_input[wrt]
    ## get arg sizes and use prod to determine flattened size
    fxnArgs_flat_sizes <- sapply(
      lapply(arg_symbols, function(x) {
        if(!is.numeric(x$size)) stop('Sizes of arguments to nFunctions must be
                           explictly specified (e.g. x = double(1, 4)) in order
                           to take derivatives.')
        x$size
      }),
      prod
    )
    ## determine the last index for each arg in the flattened input vector
    fxnArgs_final_indices <- c(0, cumsum(fxnArgs_flat_sizes))
    fxnArgs_assign_code <- list()
    for (i in seq_along(wrt)) {
      ## get the index of the arg which this wrt indexes
      arg_index <- Position(function(x) wrt[i] <= x, fxnArgs_final_indices) - 1
      wrt_index_in_arg <- as.integer(wrt[i] - fxnArgs_final_indices[arg_index])
      this_arg <- names(fxnArgs)[arg_index]
      fxnArgs_assign_code[[i]] <- substitute(
        fxnArgs[[ARG]][J] <<- x[I], list(ARG = this_arg, J = wrt_index_in_arg,
                                         I = i)
      )
    }
    result_x_indices_all <- 1:length(wrt)
    fxnArgs_assign_code <- unlist(fxnArgs_assign_code, recursive = FALSE)
  } else {
    stop(paste("The 'wrt' argument to 'nDerivs' should either be a character vector",
               "or an integer vector."))
  }

  ## 'func' needs to reassign its input to its enclosing environment's
  ## 'fnxArgs' in the appropriate places
  func <- function(x) {
    do.call("{", fxnArgs_assign_code)
    ## for(i in 1:length_x) {
    ##     ## Each line is like fxnArgs[[ 2 ]][23] <- x[5]
    ##     eval(fxnArgs_assign_code[[i]])
    ## }
    c(do.call(nf, fxnArgs, envir = fxnEnv)) #c() unrolls any answer to a vector
  }

  ans <- calcDerivs_internal(func, currentX, order, result_x_indices_all)
  ans
}

nDerivs_full <- function(fxnCall = NULL, order = c(0, 1, 2), dropArgs = NA,
                         wrt = NULL, fxnEnv = parent.frame(), NC = NULL) {
  if (is.null(NC))
    stop(paste(
      "'nDerivs' was called for a method from a full nClass interface but",
      "the 'NC' argument is NULL."
    ))
  if (!isNCgenerator(NC))
    stop(paste(
      "The 'NC' argument to 'nDerivs' must be an nClass generator (returned",
      "from a call to 'nClass')."))

  derivFxnCall <- str2lang(paste0(deparse(fxnCall[[1]]), '_derivs_'))

  fxnName <- fxnCall[[1]][[3]]
  if (is.symbol(fxnName)) fxnName <- deparse(fxnName)
  nf <- NC_get_Cpub_class(NC)$public_methods[[fxnName]]
  fxnArgs <- NFinternals(nf)$argSymTab$symbols
  fxnCall[[1]] <- derivFxnCall
  fxnCall$order <- order
  fxnCall$wrt <- if (is.numeric(wrt)) wrt else setup_wrt_internal(
    wrt, fxnArgs, fxnName)
  eval(fxnCall, fxnEnv)
}

nDerivs_generic <- function(fxnCall = NULL, order = c(0, 1, 2), dropArgs = NA,
                            wrt = NULL, fxnEnv = parent.frame(),
                            loadedObjEnv = NULL, NC = NULL) {
  fxnName <- fxnCall[[1]][[3]]
  derivFxnCall <- fxnCall[[1]]
  derivFxnCall[[3]] <- paste0(fxnName, '_derivs_')

  if (is.null(loadedObjEnv))
    loadedObjEnv <- eval(fxnCall[[1]][[2]], envir = fxnEnv)
  if (is.null(NC))
    stop(paste(
      "'nDerivs' was called for a method from a generic nClass interface but",
      "the 'NC' argument is NULL."
    ))
  if (!isNCgenerator(NC))
    stop(paste(
      "The 'NC' argument to 'nDerivs' must be an nClass generator (returned",
      "from a call to 'nClass')."))

  nf <- NC_get_Cpub_class(NC)$public_methods[[fxnName]]
  if (is.null(nf))
    stop(paste0(
      "The 'NC' argument provided to 'nDerivs' has no public method named ",
      fxnName, "."))

  fxnArgs <- NFinternals(nf)$argSymTab$symbols

  fxnCall[[1]] <- derivFxnCall
  fxnCall$order <- order
  fxnCall$wrt <- if (is.numeric(wrt)) wrt else setup_wrt_internal(
    wrt, fxnArgs, fxnName)
  eval(fxnCall, fxnEnv)
}
