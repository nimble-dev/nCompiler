##############################################################
## Section for outputting C++ code from an exprClass object ##
##############################################################

nimCppKeywordsThatFillSemicolon <- c(
    '{',
    'for',
    ifOrWhile,
    'nimSwitch',
    'cppLiteral',
    'cppComment')

genCppEnv <- new.env()
genCppEnv$.debug <- FALSE

inGenCppEnv <- function(expr) {
    expr <- substitute(expr)
    eval(expr, envir = genCppEnv)
}

compile_generateCpp <- function(code,
                                symTab = NULL,
                                indent = '',
                                showBracket = TRUE,
                                asArg = FALSE) {
    nErrorEnv$stateInfo <- paste0("handling generateCpp for ",
                                  code$name,
                                  ".")
    logging <- get_nOption('compilerOptions')[['logging']]
    if (logging) appendToLog(paste('###', nErrorEnv$stateInfo, '###'))

    if(isTRUE(code$isLiteral)) {
        value <- code$name
        if(is.numeric(value))
          return(
            if(is.nan(value)) "(nCompiler_NaN())"
            else if (identical(code$type$type, 'double') &&
                     identical(value %% 1, 0))
              ## If value is a whole number we need to add . to the end
              ## so C++ knows that it's actually a double.
              paste0(value, ".")
            else value
          )
        if(is.character(value)) return(paste0('\"',
                                              gsub("\\n","\\\\n", value),
                                              '\"'))
        if(is.null(value)) return('R_NilValue')
        if(is.logical(value) ) return(if(value)
                                          'true'
                                      else
                                          'false')
    }
        if(is.list(code) )
        stop(paste0("Error generating C++ code, there is a list where there ",
                    "shouldn't be one.  It is probably inside map information.",
                    call. = FALSE))

    if(length(code$isName) == 0)
        stop("Error generating C++ code, length(code$isName) == 0.", call. = FALSE)
    if(isTRUE(code$isName))
        return(genCppEnv$exprName2Cpp(code, symTab, asArg))
    if(code$name == '{') {
        iOffset <- as.integer(showBracket)
        ans <- vector('list', length(code$args) + 2*iOffset)
        if(showBracket) ans[[1]] <- paste0(indent, '{')
        newInd <- if(showBracket) paste0(indent, ' ') else indent
        for(i in seq_along(code$args)) {
            oneEntry <- compile_generateCpp(code$args[[i]],
                                            symTab,
                                            newInd,
                                            FALSE)
            if(code$args[[i]]$isCall)
                if(!(code$args[[i]]$name %in% nimCppKeywordsThatFillSemicolon))
                    oneEntry <- pasteSemicolon(oneEntry)
            ans[[i + iOffset]] <- if(showBracket)
                                      addIndentToList(oneEntry, newInd)
                                  else
                                      oneEntry
        }
        if(showBracket)
            ans[[length(code$args) + 2]] <- paste0(indent, '}')
        return(ans)
    }
    opInfo <- operatorDefEnv[[code$name]]
    if(!is.null(opInfo)) {
        handlingInfo <- opInfo[["cppOutput"]]
        if(!is.null(handlingInfo)) {
            handler <- handlingInfo$handler
            if(!is.null(handler)) {
                if (logging)
                  appendToLog(paste('Calling handler', handler, 'for', code$name))
                res <- eval(call(handler,
                                 code,
                                 symTab),
                            envir = genCppEnv)
                if (logging) {
                  appendToLog(paste('Finished handling', handler, 'for',
                                    code$name, 'with result:'))
                  appendToLog(res)
                }
                return(res)
            }
        }
    }
    ## default: not yet updated
    return(eval(call("AsIs", code, symTab),
                envir = genCppEnv))
}

inGenCppEnv(
    exprName2Cpp <- function(code, symTab, asArg = FALSE) {
        if(!is.null(symTab)) {
            sym <- symTab$getSymbol(code$name, inherits = TRUE)
            if(!is.null(sym))
                return(sym$generateUse(asArg = asArg))
            
        }
        return(code$name)
    }
)

inGenCppEnv(
    AsIs <- function(code, symTab) {
        paste0(exprName2Cpp(code, symTab),
               '(', paste0(unlist(lapply(code$args,
                                         compile_generateCpp,
                                         symTab,
                                         asArg = TRUE) ),
                           collapse = ', '),
               ')' )
    }
)

inGenCppEnv(
  chainedCall <- function(code, symTab) {
    firstCall <- compile_generateCpp(code$args[[1]], symTab)
    paste0(firstCall, 
           '(',
           paste0(unlist(lapply(code$args[-1],
                                compile_generateCpp, 
                                symTab, 
                                asArg = TRUE) ),
                  collapse = ', '), ')' 
    )
  }
)

inGenCppEnv(
    MidOperator <- function(code, symTab) {
        if(length(code$args) != 2) stop('Error: expecting 2 arguments for operator ',code$name)
        if(is.null(code$caller)) useParens <- FALSE
        else {
            thisRank <- operatorRank[[code$name]]
            callingRank <- if(!is.null(code$caller))
                               operatorRank[[code$caller$name]]
                           else
                               NULL
            useParens <- FALSE ## default to FALSE - possibly dangerous if we've missed a case
            if(!is.null(callingRank)) {
                if(!is.null(thisRank)) {
                    if(callingRank <= thisRank)
                        useParens <- TRUE
                }
            }
        }

        firstPart <- compile_generateCpp(code$args[[1]], symTab)
        secondPart <- compile_generateCpp(code$args[[2]], symTab)

        if(!isTRUE(get_nOption("automaticDerivatives"))) {
          ## The code generated for automatic derivatives creates problems here.
          ## The isEigScalar may not be general.
          ## In any case, we'll want a more general way to manage flex_()
          if (code$name %in% assignmentOperators && isEigScalar(code$args[[1]]))
            firstPart <- paste0('flex_(', firstPart, ')')
        }

        opString <- getOperatorDef(code$name, 'cppOutput', 'cppString')
        if (is.null(opString)) opString <- paste0(" ", code$name, " ")

        output <- paste0(firstPart, opString, secondPart)

        if(useParens)
          ouput <- paste0( '(', output, ')')

        output

    }
)

inGenCppEnv(
    BinaryOrUnary <- function(code, symTab) {
        if(length(code$args) == 2)
            return(MidOperator(code, symTab))
        AsIs(code, symTab)
    }
)

inGenCppEnv(
  IfOrWhile <- function(code, symTab) {
    part1 <- paste0(code$name,'(', compile_generateCpp(code$args[[1]], symTab), ')')
    part2 <- compile_generateCpp(code$args[[2]], symTab)
    if (is.list(part2)) {
        part2[[1]] <- paste(part1, part2[[1]])
    } else {
        part2 <- list(paste(part1, part2))
    }
    if (length(code$args)==2) return(part2)

    part3 <- compile_generateCpp(code$args[[3]], symTab)
    if (is.list(part3)) {
        part2[[length(part2)]] <- paste(part2[[length(part2)]], 'else', part3[[1]])
        part3 <- c(part2, part3[-1])
        return(part3)
    } else {
        part2[[length(part2)]] <- paste(part2[[length(part2)]], 'else', part3)
        return(part2)
    }
    stop('Error in IfOrWhile')
  }
)

inGenCppEnv(
  For <- function(code, symTab) {
    if(code$args[[2]]$name != ':') stop('Error: for now for loop ranges must be defined with :')
    begin <- compile_generateCpp(code$args[[2]]$args[[1]], symTab)
    end <- compile_generateCpp(code$args[[2]]$args[[2]], symTab)
    iterVar <- compile_generateCpp(code$args[[1]], symTab)
    part1 <- paste0('for(',
                    iterVar ,
                    '=', 
                    begin,'; ',
                    iterVar, 
                    '<= static_cast<int>(',
                    end,
                    '); ++',
                    iterVar,
                    ')')
    part2 <- compile_generateCpp(code$args[[3]], symTab)
    if(is.list(part2)) {
      part2[[1]] <- paste(part1, part2[[1]])
      return(part2)
    } else {
      return(paste(part1, part2))
    }
  }
)

inGenCppEnv(
    Return <- function(code, symTab) {
        if(length(code$args) == 0) {
            return('return')
        }
        AsIs(code, symTab)
    }
)

cppOutputMemberData <- function(code, symTab) {
  paste0( nimGenerateCpp(code$args[[1]], symTab), '.', code$args[[2]]$name)
}

inGenCppEnv(
  ## Member(A, x) -> A.x
  Member <- function(code, symTab) {
    paste0( '(',
            compile_generateCpp(code$args[[1]], symTab),
            ').', code$args[[2]]$name)
  }
)

inGenCppEnv(
  ## This differs from old system
  ## Method(A, foo, x) -> A.foo(x)
  Method <- function(code, symTab) {
    paste0( '(', 
            compile_generateCpp(code$args[[1]], symTab),
            ').', paste0(code$args[[2]]$name,
                         '(', 
                         paste0(unlist(lapply(code$args[-c(1, 2)],
                                              compile_generateCpp, 
                                              symTab) ), 
                                collapse = ', '),
                         ')' )
    )
  }
)

inGenCppEnv(
  MinusOne <- function(x) {
    if(is.numeric(x)) return(x-1)
    paste0('(',x,') - 1')
  }
)

inGenCppEnv(
  Bracket <- function(code, symTab) {
    brackets <- if(length(code$args) <= 2) c('[',']') else c('(',')')
    paste0( compile_generateCpp(code$args[[1]], symTab), 
            brackets[1], 
            paste0(unlist(lapply(code$args[-1], 
                                 function(x) MinusOne(compile_generateCpp(x, symTab) ) ) ),
                   collapse = ', '), 
            brackets[2] 
    )
}
)

inGenCppEnv(
  Literal <- function(code, symTab) {
    code$args[[1]]$name
  }
)

inGenCppEnv(
  ## This differs from old system
  ## EigenCast(A, type) -> A.cast<type>() if A$type$nDim > 0
  ## EigenCast(A, type) -> static_cast<type>(A) if A is scalar
  EigenCast <- function(code, symTab) {
    if (code$type$nDim > 0)
      paste0('(', compile_generateCpp(code$args[[1]], symTab),
             ').cast<', compile_generateCpp(code$args[[2]]), '>()')
    else
      paste0('static_cast<', compile_generateCpp(code$args[[2]]), '>(',
             compile_generateCpp(code$args[[1]], symTab), ')')
  }
)
