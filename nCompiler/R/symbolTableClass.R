symbolTableClass <- 
    R6::R6Class(
        classname = 'symbolTableClass',
        portable = FALSE,
        private = list(
            deep_clone = function(name, value) {
                switch(name,
                       symbols = {
                           ans <- lapply(value,
                                         function(x) x$clone(deep = TRUE)
                                         )
                           ans
                       },
                       value)
            }),
        public = list(
            symbols  = list(),
            parentST = NULL,
            dimAndSizeList = NULL,
            dimAndSizeListMade = NULL,
            initialize = function(parentST = NULL,
                                  ...) {
                dots <- list(...)
                if('symbols' %in% names(dots)) {
                    if(is.list(dots$symbols)) {
                        for(s in dots$symbols) {
                            if(is.null(s$name))
                                stop(paste0(
                                    'Error: all symbols in list must have meaningful',
                                    ' name fields'), call. = FALSE)
                            if(identical(s$name, character()))
                                stop(paste0(
                                    'Error: all symbols in list must have meaningful',
                                    ' name fields'), call. = FALSE)
                            symbols[[s$name]] <<- s
                        }
                    } else
                        stop('Error: symbols provided must be a list')
                }
                parentST <<- parentST
                dimAndSizeListMade <<- FALSE
            },
            ## add a symbol RC object to this symbolTable;
            ## checks for valid symbolRC object, and duplicate symbol names
            addSymbol  = function(newSymbol,
                                  allowReplace = FALSE) {
                name <- newSymbol$name
                if(!allowReplace)
                    if(name %in% getSymbolNames())
                        warning(paste0('duplicate symbol name: ', name))
                symbols[[name]] <<- newSymbol
                if(dimAndSizeListMade) {
                    dimAndSizeList[[name]] <<- {
                        ans <- try(list(newSymbol$size, newSymbol$nDim))
                        if(inherits(ans, 'try-error'))
                            NULL
                        else
                            ans
                    }
                }
            },
            ## remove a symbol RC object from this symbolTable;
            ## gives warning if symbol isn't in table
            removeSymbol = function(name) {
                if(!(name %in% getSymbolNames()))
                    warning(paste0('removing non-existant symbol name: ', name))
                symbols[[name]] <<- NULL
            },
            ## symbol accessor functions
            getLength = function()
                return(length(symbols)),
            getSymbols = function()
                return(symbols),
            getSymbolNames = function()
                if(is.null(names(symbols)))
                    return(character(0))
                else
                    return(names(symbols)),
            setSymbolNames = function(names) {
                if(length(names) != length(symbols))
                    stop('In setSymbolNames, length(names) must equal length(symbols)',
                         call.=FALSE)
                for(i in seq_along(symbols)) {
                    symbols[[i]]$name <- names[i] 
                }
                names(symbols) <<- names
            },
            getSymbol = function(name,
                                       inherits = FALSE) {
                ans <- symbols[[name]]
                if(is.null(ans))
                    if(inherits)
                        if(!is.null(parentST))
                            ans <- parentST$getSymbol(name, TRUE)
                return(ans)
            },
            symbolExists = function(name,
                                    inherits = FALSE) {
                return(!is.null(getSymbol(name, inherits)))
            },
            initDimAndSizeList = function() {
                dimAndSizeList <<- lapply(symbols, function(x) {
                    ans <- try(list(x$size, x$nDim))
                    if(inherits(ans, 'try-error'))
                        NULL
                    else
                        ans
                })
                dimAndSizeListMade <<- TRUE
            },
            makeDimAndSizeList = function(names) {
                if(!dimAndSizeListMade) initDimAndSizeList()
                dimAndSizeList[names]
            },
            getSymbolType = function(name)
                return(symbols[[name]]$type),
            getSymbolField = function(name, field)
                return(symbols[[name]][[field]]),
            setSymbolField = function(name, field, value)
                symbols[[name]][[field]] <<- value,
            
            ## parentST accessor functions
            getParentST = function()
                return(parentST),
            setParentST = function(ST)
                parentST <<- ST,
            print = function() {
                writeLines('symbol table:')
                for(i in seq_along(symbols))
                    symbols[[i]]$print()
                if(!is.null(parentST)) {
                    writeLines('parent symbol table:')
                    parentST$print()
                }
            }
        )
    )



