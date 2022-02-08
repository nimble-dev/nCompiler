## This file contains R versions of functions supported in nFunction compilation.

#' @export
parallel_for <- function(index, range, body, ...) {
  index <- substitute(index)
  range <- substitute(range)
  body <- substitute(body)
  for_loop <- quote(for(i in range) body)
  for_loop[[2]] <- index
  for_loop[[3]] <- range
  for_loop[[4]] <- body
  eval(for_loop, envir = parent.frame())
}

#' @export
parallel_reduce <- function(f, x, init, ...) {
  Reduce(f, x, init)
}

#' @export
square <- function(x) x*x
#' @export
cube <- function(x) x*x*x
## #' @export
## rsqrt <- function(x) 1/sqrt(x)
#' @export
logit <- function(x) log(x/(1-x))
#' @export
ilogit <- function(x) 1/(1+exp(-x))
#' @export
expit <- ilogit
#' @export
cloglog <- function(x) log(-log(1-x))
#' @export
icloglog <- function(x) 1-exp(-exp(x))
#' @export
probit <- function(x) qnorm(x)
#' @export
iprobit <- function(x) pnorm(x)
#' @export
phi <- iprobit
#' @export
loggam <- lgamma
#' @export
logfact <- lfactorial
#' @export
nC <- c

#' Creates numeric, integer or logical vectors for use in nFunctions
#'
#' In a \code{nFunction}, \code{numeric}, \code{integer} and \code{logical} are identical to \code{nNumeric}, \code{nInteger} and \code{nLogical}, respectively.
#'
#' @aliases nInteger nLogical numeric integer logical
#'
#' @param length the length of the vector (default = 0)
#' @param value value(s) for initializing the vector (default = 0).  This may be a vector, matrix or array but will be used as a vector.
#' @param init logical, whether to initialize elements of the vector (default = TRUE)
#' @param fillZeros logical, whether to initialize any elements not filled by (possibly recycled) \code{value} with 0 (or FALSE for \code{nLogical}) (default = TRUE)
#' @param recycle logical, whether \code{value} should be recycled to fill the entire \code{length} of the new vector (default = TRUE)
#'
#' @details
#' These functions are similar to R's \code{\link{numeric}}, \code{\link{integer}}, \code{\link{logical}} functions, but they can be used in a nFunction and then compiled using \code{nCompile}.  Largely for compilation purposes, finer control is provided over initialization behavior.  If \code{init = FALSE}, no initialization will be done, and \code{value}, \code{fillZeros} and \code{recycle} will be ignored.  If \code{init=TRUE} and \code{recycle=TRUE}, then \code{fillZeros} will be ignored, and \code{value} will be repeated (according to R's recycling rule) as much as necessary to fill a vector of length \code{length}.  If \code{init=TRUE} and \code{recycle=FALSE}, then if \code{fillZeros=TRUE}, values of 0 (or FALSE for \code{nLogical}) will be filled in after \code{value} up to length \code{length}.  Compiled code will be more efficient if unnecessary initialization is not done, but this may or may not be noticeable depending on the situation.
#' 
#' When used in a \code{nFunction} (in \code{run} or other member function), \code{numeric}, \code{integer} and \code{logical} are immediately converted to \code{nNumeric}, \code{nInteger} and \code{nLogical}, respectively.  
#' 
#' @author Daniel Turek, Chris Paciorek, Perry de Valpine
#' @aliases numeric
#' @seealso \code{\link{nMatrix}}, \code{\link{nArray}}
#' @export
nNumeric <- function(length = 0, value = 0, init = TRUE, fillZeros = TRUE, recycle = TRUE) {
    fillValue <- makeFillValue(value, 'double', init)
    makeReturnVector(fillValue, length, recycle)
}

#' @rdname nNumeric
#' @export
nInteger <- function(length = 0, value = 0, init = TRUE, fillZeros = TRUE, recycle = TRUE) {
    fillValue <- makeFillValue(value, 'integer', init)
    makeReturnVector(fillValue, length, recycle)
}

#' @rdname nNumeric
#' @export
nLogical <- function(length = 0, value = 0, init = TRUE, fillZeros = TRUE, recycle = TRUE) {
    fillValue <- makeFillValue(value, 'logical', init)
    makeReturnVector(fillValue, length, recycle)
}

#' Creates matrix or array objects for use in nFunctions
#' 
#' In a \code{nFunction}, \code{matrix} and \code{array} are identical to \code{nMatrix} and \code{nArray}, respectively
#'
#' @aliases nArray matrix array
#'
#' @param value value(s) for initialization (default = 0).  This can be a vector, matrix or array, but it will be used as a vector.
#' @param nrow the number of rows in a matrix (default = 1)
#' @param ncol the number of columns in a matrix (default = 1)
#' @param dim vector of dimension sizes in an array (default = \code{c(1, 1)})
#' @param init logical, whether to initialize values (default = \code{TRUE})
#' @param fillZeros logical, whether to initialize any elements not filled by (possibly recycled) \code{value} with 0 (or \code{FALSE} for \code{nLogical}) (default = \code{TRUE})
#' @param recycle logical, whether \code{value} should be recycled to fill the entire contents of the new object (default = \code{TRUE})
#' @param type character representing the data type, i.e. \code{'double'}, \code{'integer'}, or \code{'logical'} (default = \code{'double'})
#' @param nDim number of dimensions in an array.  This is only necessary for \code{nCompile} if the length of \code{dim} cannot be determined during compilation.
#'
#' @details
#' These functions are similar to R's \code{\link{matrix}} and \code{\link{array}} functions, but they can be used in a nFunction and compiled using \code{nCompile}.  Largely for compilation purposes, finer control is provided over initialization behavior, similarly to \code{\link{nNumeric}}, \code{\link{nInteger}}, and \code{\link{nLogical}}. If \code{init = FALSE}, no initialization will be done, and \code{value}, \code{fillZeros} and \code{recycle} will be ignored.  If \code{init=TRUE} and \code{recycle=TRUE}, then \code{fillZeros} will be ignored, and \code{value} will be repeated (according to R's recycling rule) as much as necessary to fill the object.  If \code{init=TRUE} and \code{recycle=FALSE}, then if \code{fillZeros=TRUE}, values of 0 (or FALSE for \code{nLogical}) will be filled in after \code{value}.  Compiled code will be more efficient if unnecessary initialization is not done, but this may or may not be noticeable depending on the situation.
#'
#' When used in a \code{nFunction} (in \code{run} or other member function), \code{matrix} and \code{array} are immediately converted to \code{nMatrix} and \code{nArray}, respectively.
#'
#' The \code{nDim} argument is only necessary for a use like \code{dim <- c(2, 3, 4); A <- nArray(0, dim = dim, nDim = 3)}.  It is necessary because the compiler must determine during compilation that \code{A} will be a 3-dimensional numeric array.  However, the compiler doesn't know for sure what the length of \code{dim} will be at run time, only that it is a vector.  On the other hand,   \code{A <- nArray(0, dim = c(2, 3, 4))} is allowed because the compiler can directly determine that a vector of length three is constructed inline for the \code{dim} argument.
#' 
#' @author Daniel Turek and Perry de Valpine
#' @seealso \code{\link{nNumeric}} \code{\link{nInteger}} \code{\link{nLogical}}
#' @export
nMatrix <- function(value = 0, nrow = NA, ncol = NA, init = TRUE, fillZeros = TRUE, recycle = TRUE, type = 'double') {
    ## the -1's are used because nCompiler does not allow both missingness and default value
    ## but R's matrix function relies on both possibilities
    fillValue <- makeFillValue(value, type, init)
    mnrow <- missing(nrow) || is.na(nrow)
    mncol <- missing(ncol) || is.na(ncol)
    if(mnrow)
        if(mncol) {
            base::matrix(fillValue)
        } else {
            nrow <- ceiling( length(fillValue) / ncol )
            fillValue <- makeReturnVector(fillValue, nrow * ncol, recycle)
            base::matrix(fillValue, ncol = ncol, nrow = nrow)
        }
    else
        if(mncol) {
            ncol <- ceiling( length(fillValue) / nrow )
            fillValue <- makeReturnVector(fillValue, nrow * ncol, recycle)
            base::matrix(fillValue, nrow = nrow, ncol = ncol)
        } else {
            fillValue <- makeReturnVector(fillValue, ncol*nrow, recycle)
            base::matrix(fillValue, nrow = nrow, ncol = ncol)
        }
}


#' @rdname nMatrix
#' @export
nArray <- function(value = 0, dim = c(1, 1), init = TRUE, fillZeros = TRUE, recycle = TRUE, nDim, type = 'double') {
    if(!missing(nDim)) dim <- dim[1:nDim]
    fillValue <- makeFillValue(value, type, init)
    fillValue <- makeReturnVector(fillValue, prod(dim), recycle)
    if(length(dim) == 1) fillValue
    else base::array(fillValue, dim)
}

makeFillValue <- function(value, type, init) {
    fillValue <- if(init) value else 0
    fillValueTyped <- switch(type,
                             double = as.numeric(fillValue),
                             integer = as.integer(fillValue),
                             logical = as.logical(fillValue),
                             stop('unknown type argument'))
    return(fillValueTyped)
}

makeReturnVector <- function(fillValue, length, recycle) {
    if(length(fillValue) == 1) {
        if(recycle)
            rep(fillValue, length)
        else
            c(fillValue, as(rep(0, max(length-1, 0)), class(fillValue)))
    }
    else {
        if(length(fillValue) != length) {
            if(length(fillValue) < length) {
                ##warning(paste0("Not enough values provided for vector of length ",length, ".")) 
                if(recycle)
                    rep(fillValue, length.out = length)
                else
                    c(fillValue, as(rep(0, length-length(fillValue)), class(fillValue)))
            } else {
                ##warning(paste0("Too many values provided for vector of length ",length, ".")) 
                fillValue[1:length]
            }
        } else {
            fillValue
        }
    }
}

#' Spectral Decomposition of a Matrix
#' 
#' In a \code{nFunction}, \code{nEigen} is identical to \code{eigen}
#'
#' @details This function is similar to R's \code{\link{eigen}} function, but 
#'   can be used in a nFunction and compiled using \code{nCompile}.  
#' 
#' @param x a numeric or complex matrix whose spectral decomposition is to be 
#'   computed. Logical matrices are coerced to numeric.
#'
#' @export
#' 
nEigen <- function(x) {
  eigen(x)
}

#' Extract or replace the diagonal of matrix
#' 
#' In a \code{nFunction}, \code{nDiag} is identical to \code{diag}
#'
#' @details This function is similar to R's \code{\link{diag}} function, but 
#'   can be used in a nFunction and compiled using \code{nCompile}.  
#' 
#' @param x a numeric or complex matrix
#'
#' @export
#' 
nDiag <- function(x) {
  diag(x)
}

#' Compute the cholesky decomposition of a matrix
#' 
#' In a \code{nFunction}, \code{nChol} is identical to \code{chol}
#'
#' @details This function is similar to R's \code{\link{diag}} function, but 
#'   can be used in a nFunction and compiled using \code{nCompile}.  
#' 
#' @param x a symmetric matrix
#'
#' @export
#' 
nChol <- function(x) {
  chol(x)
}

#' Replicate Elements of Vectors and Lists
#' 
#' In a \code{nFunction}, \code{nRep} is identical to \code{base::rep}
#'
#' @details This function is similar to R's \code{\link{rep}} function, but 
#'   can be used in a nFunction and compiled using \code{nCompile}.  
#' 
#' @param x a vector (of any mode including a list) or a factor or 
#'   (for rep only) a POSIXct or POSIXlt or Date object; or an S4 object 
#'   containing such an object.
#'
#' @param ... further arguments to be passed to or from other methods.
#'
#' @export
#' 
nRep <- function(x, ...) {
  base::rep(x, ...)
}

#' Converts a dense matrix or vector to a sparse matrix or vector
#'
#' @importFrom Matrix Matrix as
#' @param x object to convert to sparse representation
#' @param prune TRUE to remove 0's from an object if it is already stored in a 
#'   sparse format
#' @export
asSparse <- function(x, prune = TRUE) {
  if(inherits(x, c('dgCMatrix', 'dgTMatrix', 'dsparseVector', 'isparseVector', 
                   'lsparseVector', 'zsparseVector'))) {
    if(prune) {
      return(drop0(x))
    } else {
      return(x)
    }
  } else if(inherits(x, 'matrix')) {
    as(x, 'sparseMatrix')
  } else if(inherits(x, c('numeric', 'integer', 'logical', 'complex'))) {
    as(x, 'sparseVector')
  } else {
    stop('Attempting to convert to sparse format from unknown type')
  }
}

#' Converts a sparse matrix or vector to a dense sparse matrix or vector
#' 
#' @export
asDense <- function(x) {
  if(inherits(x, c('matrix', 'numeric', 'integer', 'logical', 'complex'))) {
    return(x)
  } else if(inherits(x, 'sparseMatrix')) {
    return(as(x, 'matrix'))
  } else if(inherits(x, 'sparseVector')) {
    return(as(x, 'vector'))
  } else {
    stop('Attempting to convert to dense format from unknown type')
  }
}

#' Wrapper for matrix multiplication
#' 
#' @export
nMul <- function(x, y) {
  x %*% y
}