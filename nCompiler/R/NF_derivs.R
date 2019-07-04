#' nFunction Derivatives
#' 
#' EXPERIMENTAL Computes the value, Jacobian, and Hessian of a given  
#' \code{nFunction} method.  
#' 
#' @param nFxn a call to a compiled or uncompiled \code{nFunction} method with
#'   arguments included.
#' @param order an integer vector with values within the set {0, 1, 2},
#'   corresponding to whether the function value, Jacobian, and Hessian should
#'   be returned respectively.  Defaults to \code{c(0, 1, 2)}.
#' @param dropArgs a vector of integers specifying any arguments to \code{nFxn}
#'   that derivatives should not be taken with respect to.  For example,
#'   \code{dropArgs = 2} means that the second argument to \code{nFxn} will not
#'   have derivatives taken with respect to it.  Defaults to an empty vector.
#' @param wrt a character vector of either: names of function arguments to take
#'   derivatives with respect to.  If left empty, derivatives will be taken
#'   with respect to all arguments to \code{nFxn}.
#' @param silent a logical argument that determines whether warnings will be
#'   displayed.
#' @details Derivatives for uncompiled nFunctions are calculated using the
#'   \code{numDeriv} package.  If this package is not installed, an error will
#'   be issued.  Derivatives for matrix valued arguments will be returned in
#'   column-major order.
#' 
#' @return a \code{nimbleList} with elements \code{value}, \code{jacobian}, and
#'   \code{hessian}.
#' 
#' @export
nDerivs <- function(nFxn = NA,
                    order = c(0,1,2),
                    dropArgs = NA,
                    wrt = NULL){
  fxnEnv <- parent.frame()
  fxnCall <- match.call()
  if(is.null(fxnCall[['order']])) fxnCall[['order']] <- order
  derivFxnCall <- fxnCall[['nFxn']]

  if(!is.na(dropArgs)){
    removeArgs <- which(wrt == dropArgs)
    if(length(removeArgs) > 0)
      wrt <- wrt[-removeArgs]
  }

  if (length(derivFxnCall[[1]]) == 3 &&
        deparse(derivFxnCall[[1]][[1]]) == '$') {

    nClass_obj <- eval(derivFxnCall[[1]][[2]], envir = fxnEnv)

    if (!isNC(nClass_obj))
      stop(paste0(
        "Do not know what type of object ",
        deparse(fxnCall[[2]][[1]][[2]]), "is."
      ))

    nDerivs_full(fxnCall = derivFxnCall, order = order, wrt = wrt, fxnEnv = fxnEnv)
  } else if (FALSE) { ## TODO: add nDerivs_generic
  } else ## nFxn is a nimble function
    nDerivs_nf(fxnCall = derivFxnCall, order = order, wrt = wrt, fxnEnv = fxnEnv)
}

calcDerivs_internal <- function(func, X, order, resultIndices ) {
  if(!require('numDeriv'))
    stop("The 'numDeriv' package must be installed to use derivatives in
         uncompiled nFunctions.")

  hessianFlag <- 2 %in% order
  jacobianFlag <- 1 %in% order
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
    if(jacobianFlag) outGrad <- derivList$D[,1:derivList$p, drop = FALSE]
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
    if(jacobianFlag){
      ## If jacobians are requested, derivatives taken using numDeriv's jacobian() 
      ## function.  After that, we extract the various derivative elements and 
      ## arrange them properly.
      outVal <- func(X) 
      outGrad <- jacobian(func, X)
    } else 
      if(valueFlag)
        outVal <- func(X)
  
  outList <- list()
  if(!missing(resultIndices)) {
    if(jacobianFlag) outGrad <- outGrad[, resultIndices, drop=FALSE]
    if(hessianFlag) outHess <- outHess[resultIndices, resultIndices, , drop=FALSE]
  }
  if(valueFlag) outList$value <- outVal
  if(jacobianFlag) outList$jacobian <- outGrad
  if(hessianFlag) outList$hessian <- outHess
  return(outList)
}

nDerivs_nf <- function(fxnCall = NULL, order = c(0,1,2),
                       wrt = NULL, fxnEnv = parent.frame()) {

  nf <- eval(fxnCall[[1]], envir = fxnEnv)

  ## standardize the fxnCall arguments
  fxnCall <- match.call(nf, fxnCall)
  fA <- formals(nf)

  if(is.null(wrt)) {
    wrt <- names(fA)
  }

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

  # get the user-supplied arguments to the nFunction
  fxnCall_args <- as.list(fxnCall)[-1]

  ## Get the value of the args
  fxnArgs <- lapply(fxnCall_args,
                    function(x) 
                      eval(x, envir = fxnEnv))

  arg_symbols <- NFinternals(nf)$argSymTab$getSymbols()

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

  fxnName <- as.character(fxnCall[[1]])
  func <- function(x) {
    do.call("{", fxnArgs_assign_code)
    ## for(i in 1:length_x) {
    ##     ## Each line is like fxnArgs[[ 2 ]][23] <- x[5]
    ##     eval(fxnArgs_assign_code[[i]])
    ## }
    c(do.call(fxnName, fxnArgs, envir = fxnEnv)) #c() unrolls any answer to a vector
  }

  ans <- calcDerivs_internal(func, currentX, order, result_x_indices_all)
  ##  jacobian(func, currentX)
  ans
}

nDerivs_full <- function(fxnCall = NULL, order = c(0,1,2),
                         wrt = NULL, fxnEnv = parent.frame()) {
  derivsFxnCall <- str2lang(paste0(deparse(fxnCall[[1]]), '_derivs_'))
  fxnCall[[1]] <- derivsFxnCall
  fxnCall$order <- order
  ## TODO: handle wrt argument
  fxnCall$wrt <- wrt
  deriv_obj <- eval(fxnCall, fxnEnv)
  derivs <- list()
  if (0 %in% order) derivs_obj$value <- value(obj_env, 'value')
  if (1 %in% order) derivs_obj$gradient <- value(obj_env, 'gradient')
  if (2 %in% order) derivs_obj$hessian <- value(obj_env, 'hessian')
  derivs
}
