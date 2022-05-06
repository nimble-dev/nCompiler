nKeyWords <- list(copy = 'nCopy',
                  print = 'nPrint',
                  cat = 'nCat',
                  step = 'nStep',
                  equals = 'nEquals',
                  dim = 'nDim',
                  stop = 'nStop',
                  numeric = 'nNumeric',
                  logical = 'nLogical',
                  integer = 'nInteger',
                  matrix = 'nMatrix',
                  array = 'nArray',
                  # round = 'nRound',
                  c = 'nC',
                  rep = 'nRep',
                  seq = 'nSeq',
                  eigen = 'nEigen',
                  diag = 'nDiag',
                  Diagonal = 'nDiagonal', # mirror Matrix::Diagonal to 
                                          # create sparse matrices
                  '%*%' = 'nMul',
                  svd = 'nSvd',
                  chol = 'nChol',
                  optim = 'nOptim',
                  optimDefaultControl = 'nOptimDefaultControl',
                  derivs = 'nDerivs',
                  solve = 'nSolve',
                  forwardsolve = 'nForwardsolve',
                  backsolve = 'nBacksolve')

nf_changeKeywords <- function(code){
    if(length(code) > 0){
        for(i in seq_along(code) ) {
            if(is.call(code) ) {
                if(!is.null(code[[i]]) ) {
                    code[[i]] <- nf_changeKeywordsOne(code[[i]])
                }
            }
        }
    }
    return(code)
}


nf_changeKeywordsOne <- function(code, first = FALSE){
    if(length(code) == 1){
        if(as.character(code) %in% names(nKeyWords)) {
            if(is.call(code)) {
                code[[1]] <- as.name( nKeyWords[[as.character(code)]] )
            } else {
                if(!is.character(code) & first)
                    code <- as.name( nKeyWords[[as.character(code)]] )
            }
        }
    }
    else if(length(code) > 1){ 
        for(i in seq_along(code) ) {
            if(!is.null(code[[i]]) )
                code[[i]] <- nf_changeKeywordsOne(code[[i]], first = i == 1)
        }
    }
    return(code)
}
