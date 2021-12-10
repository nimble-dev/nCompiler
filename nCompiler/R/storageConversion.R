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
