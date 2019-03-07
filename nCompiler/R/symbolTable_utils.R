symbolTable_getArgNames <- function(symTab) {
    unlist(lapply(symTab$symbols,
                  function(x)
                      if(isTRUE(x$isArg))
                          x$name
                      else
                          NULL ## unlist removes NULLs
                  )
           )
}

symbolTable_setImplementation <- function(symTab,
                                          implementation = NA) {
    if(length(implementation)==1)
        implementation <- rep(implementation,
                              length(symTab$symbols))
    for(i in seq_along(symTab$symbols)) {
        symTab$symbols[[i]]$implementation <- implementation[i]
    }
    invisible(NULL)
}

symbolTable2cppSymbolTable <- function(symTab,
                                       args = FALSE,
                                       include,
                                       parentSymbolTable = NULL
                                       ) {
    newSymTab <- symbolTableClass$new(parentST = parentSymbolTable)
    if(missing(include))
        include <- symTab$getSymbolNames()
    for(s in include) {
        sObj <- symTab$getSymbol(s)
        isArg <- sObj$isArg
        if(xor(isArg, args)) next
        if(is.null(sObj$type))
            stop(paste('Error in symbolTable2cppVars for ',
                       symTab,
                       '. type field is not set.'),
                 call. = FALSE)
        if(length(sObj$type == 'Ronly') == 0)
            stop(paste('Error in symbolTable2cppVars for ',
                       symTab,
                       ',  length(sObj$type == "Ronly") == 0'),
                 call. = FALSE)
        if(sObj$type == 'Ronly')
            next
        newSymOrList <- symTab$getSymbol(s)$genCppVar()
        if(is.list(newSymOrList)) {
            for(i in seq_along(newSymOrList)) {
                newSymTab$addSymbol(newSymOrList[[i]])
            }
        } else {
            newSymTab$addSymbol(newSymOrList)
        }
    }
    newSymTab
}

