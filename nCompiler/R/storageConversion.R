#' Converts a dense matrix or vector to a sparse matrix or vector
#'
#' @importFrom Matrix Matrix as
#' @export
asSparse <- function(x) {
  if(inherits(x, c('dgCMatrix', 'dsparseVector', 'isparseVector', 
                   'lsparseVector', 'zsparseVector'))) {
    return(x)
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
