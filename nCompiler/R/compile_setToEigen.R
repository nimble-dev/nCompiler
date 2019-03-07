
setToEigenEnv <- new.env()
setToEigenEnv$.debug <- FALSE

compile_setToEigen <- function(code,
                               symTab,
                               auxEnv) {
    nErrorEnv$stateInfo <- paste0("handling setToEigen for ",
                                       code$name,
                                       ".")
    if(code$isLiteral) {
        return(list()) ## Possibly we'll want to set toEigen in literals,
        ## but it's not clear yet how we'd use that.
    }
    if(code$isName) {
        return(list()) ## Ditto
    }
    if(code$isCall) {
        if(code$name == '{') {
            ## recurse over lines
            for(i in seq_along(code$args)) {
                if(inherits(code$args[[i]], 'exprClass')) {
                    newInsertions <-
                        compile_setToEigen(code$args[[i]],
                                           symTab,
                                           auxEnv)
                    code$args[[i]]$insertions <-
                        c(code$args[[i]]$insertions,
                          if(is.null(newInsertions)) list() else newInsertions
                          )
                }
            }
            return(invisible(NULL))
        }
        opInfo <- operatorDefEnv[[code$name]]
        ##        thisCall <- toEigenCalls[[code$name]]
        if(!is.null(opInfo)) {
            handlingInfo <- opInfo[["eigenImpl"]]
            handler <- handlingInfo[["toEigen"]]
            if(!is.null(handler)) {
                ans <- eval(call(handler, code, symTab, auxEnv, handlingInfo),
                            envir = setToEigenEnv)
                return(ans)
            }
        }
        ## TO DO: handle nFunction
    }
    invisible(NULL)
}

inSetToEigenEnv <- function(expr) {
    expr <- substitute(expr)
    eval(expr, envir = setToEigenEnv)
}

inSetToEigenEnv(
    Yes <- function(code, symTab, auxEnv, handlingInfo) {
        ## We may need a handlingInfo field to control
        ## whether recursion should be done.
        inserts <- recurse_setToEigen(code,
                                      symTab,
                                      auxEnv,
                                      handlingInfo)
        code$implementation$toEigen <- 'yes'
    }
)

inSetToEigenEnv(
    No <- function(code, symTab, auxEnv, handlingInfo) {
        inserts <- recurse_setToEigen(code,
                                      symTab,
                                      auxEnv,
                                      handlingInfo)
        code$implementation$toEigen <- 'no'
    }
)

inSetToEigenEnv(
    Maybe <- function(code, symTab, auxEnv, handlingInfo) {
        inserts <- recurse_setToEigen(code,
                                      symTab,
                                      auxEnv,
                                      handlingInfo)
        code$implementation$toEigen <- 'maybe'
    }
)

inSetToEigenEnv(
    Assign <- function(code, symTab, auxEnv, handlingInfo) {
        inserts <- recurse_setToEigen(code,
                                      symTab,
                                      auxEnv,
                                      handlingInfo,
                                      useArgs = c(FALSE, TRUE))
        inserts <- c(inserts, AssignLHS(code, symTab, auxEnv, handlingInfo))
    }
)

inSetToEigenEnv(
    AssignLHS <- function(code, symTab, auxEnv, handlingInfo) {
        if(identical(code$type, 'unknown')) {
            code$implementation$toEigen <- 'no'
            return(list())
        }
        code$implementation$toEigen <- 'yes'    
        list()
    }
)

inSetToEigenEnv(
    recurse_setToEigen <-
        function(code, symTab, auxEnv, handlingInfo,
                 useArgs = rep(TRUE, length(code$args))) {
            ## won't be here unless code is a call.  It will not be a {
            inserts <- list()
            for(i in seq_along(code$args)) {
                if(useArgs[i]) {
                    if(inherits(code$args[[i]], 'exprClass')) {
                        inserts <- c(inserts,
                                     compile_setToEigen(code$args[[i]],
                                                        symTab,
                                                        auxEnv))
                    }
                }
            }
            if(length(inserts)==0) NULL else inserts
        }
)
