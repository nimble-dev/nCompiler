make_cpp_filebase <- function(cpp_code_name) {
  paste0(Rname2CppName(cpp_code_name), '_c_')
}

nf_substituteExceptFunctionsAndDollarSigns <- function(code, subList) {
    ## this is almost like doing substitute with code and subList, but it doesn't traverse the RHS of a '$' operator
    ## and it doesn't replace and function names
    if(is.character(code)) return(code)
    if(is.numeric(code)) return(code)
    if(is.logical(code)) return(code)
    if(is.null(code)) return(code)
    if(is.name(code)) {
        maybeAns <- subList[[as.character(code)]]
        return(
            if(is.null(maybeAns))
                code
            else
                maybeAns
        )
    }
    if(is.call(code)) {
        if(length(code) == 1)
            return(code)
        else {
            if(is.call(code[[1]]))
                indexRange <- 1:length(code)
            else {
                if(as.character(code[[1]])=='$')
                    indexRange <- 2
                else 
                    indexRange <- 2:length(code)
            }
        }
        for(i in indexRange)
            code[[i]] <-
                nf_substituteExceptFunctionsAndDollarSigns(code[[i]], subList)
        return(code)
    }
    if(is.list(code)) {
        ## Keyword processing stage of compilation may have stuck
        ## lists into the argument list of a call (for maps)
        code <- lapply(code,
                       nf_substituteExceptFunctionsAndDollarSigns,
                       subList)
        return(code)
    }
    stop(paste("Error doing replacement for code ", deparse(code)))
}

mangleArgumentNames <- function(argNames) {
    ans <- character()
    if(length(argNames) > 0)
        ans <- paste0("ARG",
                      seq_along(argNames),
                      "_",
                      Rname2CppName(argNames),"_")
}
