
## each entry in the typeDeclarationList
## gives a function to convert the arguments of a
## type declaration into a symbol object.

## Cases to handle
##
## a = [5 | TRUE | 5L ]
## a = integerScalar(value = 4)
## a = foo()
## a = integerVector(10, init = FALSE)
## a = integerVector(10, value = 5)
## a = integerVector(value = foo())
## a = integerMatrix(nrow = 3, ncol = 4, value = some_vector)
## a = integerArray(dims = c(3, 5, 4), init = FALSE)
## a = integer(10, init = FALSE)
## a = matrix(type = "integer", nrow = 10, ncol, init = FALSE)

#' @export
nType <- function(scalarType, nDim, isRef = FALSE, ...) {
  symbolBasic$new(type = scalarType,
                  nDim = nDim,
                  isRef = isRef,
                  ...)
}

nSparseType <- function(scalarType, nDim, isRef = FALSE, ...) {
  symbolSparse$new(type = scalarType,
                   nDim = nDim,
                   isRef = isRef,
                   ...)
}

typeDeclarationList <- list(
  ref = function(internalType) {
    ans <- argType2symbol(substitute(internalType),
                          evalEnv = parent.frame(2))
    ans$isRef <- TRUE
    nErrorEnv$.isRef_has_been_set <- TRUE
    ans
  },
  ## integer types:
  integerScalar = function(value) {
    nType("integer", 0)
  },
  integerVector = function(length = NA,
                           ...) {
    nType("integer", 1, ...)
  },
  integerMatrix = function(value,
                           ...) {
    nType("integer", 2, ...)
  },
  integerArray = function(value,
                          nDim = 1,
                          ...) {
    nType("integer", nDim, ...)
  },
  ## logical types
  logicalScalar = function(value) {
    nType("logical", 0)
  },
  logicalVector = function(length = NA,
                           ...) {
    nType("logical", 1, ...)
  },
  logicalMatrix = function(value,
                           ...) {
    nType("logical", 2)
  },
  logicalArray = function(value,
                          nDim = 1,
                          ...) {
    nType("logical", nDim, ...)
  },
  ## numeric types
  numericScalar = function(value) {
    nType("double", 0)
  },
  numericVector = function(length = NA,
                           ...) {
    nType("double", 1, size = length, ...)
  },
  numericMatrix = function(value,
                           ...) {
    nType("double", 2, ...)
  },
  numericArray = function(value,
                          nDim = 1,
                          ...) {
    nType("double", nDim, ...)
  },
  ## versions with type as a declared argument
  nScalar = function(...,
                     type = "double") {
    nType(type, 0)
  },
  nVector = function(value,
                     length,
                     ...,
                     type = "double") {
    nType(type, 1)
  },
  nMatrix = function(value,
                     ...,
                     type = "double") {
    nType(type, 2)
  },
  nArray = function(value,
                    dim,
                    ...,
                    type = "double") {
    nType(type, length(dim))
  },
  ## vector versions with type embedded in keyword
  nInteger = function(length = NA,
                      ...) {
    nType("integer", 1)
  },
  nLogical = function(length = NA,
                      ...) {
    nType("logical", 1)
  },
  nNumeric = function(length = NA,
                      ...) {
    nType("double", 1)
  },
  ## versions from original.nCompiler
  double = function(nDim = 0,
                    ...) {
    nType("double", nDim)
  },
  integer = function(nDim = 0,
                     ...) {
    nType("integer", nDim)
  },
  logical = function(nDim = 0,
                     ...) {
    nType("logical", nDim)
  },
  void = function(...) {
    nType("void", 0)
  },
  string = function(...) {
    nType("string", 0)
  },

  ## Rcpp types
  RcppList = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::List", ...)
  },
  RcppNumericVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::NumericVector", ...)
  },
  RcppNumericMatrix = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::NumericMatrix", ...)
  },
  RcppIntegerVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::IntegerVector", ...)
  },
  RcppIntegerMatrix = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::IntegerMatrix", ...)
  },
  RcppCharacterVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::CharacterVector", ...)
  },
  RcppCharacterMatrix = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::CharacterMatrix", ...)
  },
  RcppComplexVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::ComplexVector", ...)
  },
  RcppComplexMatrix = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::ComplexMatrix", ...)
  },
  RcppLogicalVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::LogicalVector", ...)
  },
  RcppLogicalMatrix = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::LogicalMatrix", ...)
  },
  RcppDateVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::DateVector", ...)
  },
  RcppDatetimeVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::DatetimeVector", ...)
  },
  RcppRawVector = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::RawVector", ...)
  },
  RcppNamed = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::Named", ...)
  },
  RcppDataFrame = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::DataFrame")
  },
  RcppLogicalMatrix = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::LogicalMatrix", ...)
  },
  RcppS4 = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::S4", ...)
  },
  RcppFunction = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::Function", ...)
  },
  
  ## RcppEigen RcppTypes
  RcppEigenMatrixXd = function(...) {
    symbolRcppType$new(RcppType = "Eigen::MatrixXd", ...)
  },
  RcppEigenMatrixXi = function(...) {
    symbolRcppType$new(RcppType = "Eigen::MatrixXi", ...)
  },
  RcppEigenMatrixXcd = function(...) {
    symbolRcppType$new(RcppType = "Eigen::MatrixXcd", ...)
  },
  RcppEigenVectorXd = function(...) {
    symbolRcppType$new(RcppType = "Eigen::VectorXd", ...)
  },
  RcppEigenVectorXi = function(...) {
    symbolRcppType$new(RcppType = "Eigen::VectorXi", ...)
  },
  RcppEigenVectorXcd = function(...) {
    symbolRcppType$new(RcppType = "Eigen::VectorXcd", ...)
  },
  
  ## Sparse types
  nSparseMatrix = function(value,
                           ...,
                           type = "double") {
    nSparseType(scalarType = type, nDim = 2)
  },
  nSparseVector = function(value,
                           ...,
                           type = "double") {
    nSparseType(scalarType = type, nDim = 1)
  },
  
  ## determine type from an evaluated object
  typeDeclarationFromObject = function(x) {
    if(inherits(x, 'symbolBasic'))
      return(x)
    scalarType <- storage.mode(x)
    if(!(scalarType %in% c("integer",
                           "double",
                           "logical"))) {
      stop(paste0("storage.mode of an object used to declare a.nCompiler ",
                  "argument must be integer, double or logical."),
           call. = FALSE)
    }
    nDim <- length(nDim(x))
    if(!(nDim >= 0 &
           nDim <= 6))
      stop(paste0("Invalid number of dimensions used to declare a.nCompiler ",
                  "argument.  Dimensions from 0-6 are allowed."),
           call. = FALSE)  
    nType(scalarType, nDim)
  }
  ## universal handler for creating symbolBasic objects
)

argType2symbol <- function(argType,
                           name = character(),
                           origName = "",
                           isArg = FALSE,
                           explicitType = NULL,
                           isRef = NULL,
                           evalEnv = parent.frame()) {
  nErrorEnv$stateInfo <- paste0("handling argument ", name, ".")
  nErrorEnv$.isRef_has_been_set <- FALSE
  typeToUse <- if(!is.null(explicitType))
    explicitType
  else
    argType
  inputAsCharacter <- FALSE
  if(is.character(typeToUse)) {
    typeToUse <- parse(text = typeToUse, keep.source = FALSE)[[1]]
    inputAsCharacter <- TRUE
  }
  ## allow 'scalarInteger' to become scalarInteger()
  if(is.name(typeToUse))
    typeToUse <- as.call(list(typeToUse))
  
  ## argType could be a blank
  if(is.name(argType))
    if(as.character(argType)=="")
      argType <- NULL
  
  ans <- try({
    ## TO-DO: Case 1: It is a nType object
    ##    To be implemented
    ##
    ## Case 2: It is a valid declaration
    funName <- deparse(typeToUse[[1]])
    handler <- typeDeclarationList[[funName]]
    if(!is.null(handler)) {
      symbol <- do.call(handler, as.list(typeToUse[-1]))
      symbol$name <- name
      symbol$isArg <- isArg
      if(!is.null(isRef)) {
        if(isTRUE(nErrorEnv$.isRef_has_been_set)) {
          nErrorEnv$.errorDetails <-
            paste0("The reference status is being set in multiple ways.",
                   " That is not allowed.")
          stop(call.=FALSE)
        }
        symbol$isRef <- isRef
      }
      if(!is.null(explicitType)) {
        if(!is.null(argType)) {
          checkObject <- eval(argType, envir = evalEnv)
          checkSymbol <- typeDeclarationList[["typeDeclarationFromObject"]](checkObject)
          need_warning <- !identical(symbol$type, checkSymbol$type)
          ## If dimensions don't match, trigger an error...
          need_error <- !identical(as.integer(symbol$nDim),
                                   as.integer(checkSymbol$nDim))
          ## ... but might be ok if the checked type is nDim = 1 with length 1
          ## and provided is nDim = 0.
          if(need_error) {
            if(symbol$nDim == 0 & checkSymbol$nDim == 1)
              if(length(checkObject) == 1)
                need_error <- FALSE
          }
          if(need_warning)
            warning(paste0("Type ", symbol$type, " declared, ",
                           "but default has type ", checkSymbol$type),
                    call. = FALSE)
          if(need_error) {
            nErrorEnv$.errorDetails <-
              paste0("nDim = ", symbol$nDim, " declared, ",
                     "but default has nDim = ", checkSymbol$nDim)
            stop(call. = FALSE)
          }
        }
      }
    } else {
      ## Case 3: It is a nClass type or possibly other "to-be-determined" type.
      ## We defer type lookup until compiler stage labelAbstractTypes
      if(inputAsCharacter) {
        symbol <- symbolTBD$new(name = name, 
                                type = funName, 
                                isArg = isArg)
      } else {
        ## Case 4: Type can be determined by evaluating the default
        demoObject <- eval(argType, envir = evalEnv)
        symbol <-
          typeDeclarationList[["typeDeclarationFromObject"]](demoObject)
        symbol$name <- name
        symbol$isArg <- isArg
        if(isTRUE(isRef)) {
          nErrorEnv$.errorDetails <-
            paste0("A reference variable must have its type set by explicit declaration,",
                   " because it cannot have a default value.")
          stop(call.=FALSE)
        }
      }
    }
    symbol
  },
  silent = TRUE
  )
  if(inherits(ans, 'try-error')) {
    errorDetails <- nErrorEnv$.errorDetails
    nErrorEnv$.errorDetails <- NULL
    stop(paste0("Invalid type declaration",
                if(!is.null(origName))
                  paste0(" for ", origName)
                else
                  character(),
                ". ", paste(errorDetails, collapse = "\n")),
         call.=FALSE)
  }
  if(isTRUE(symbol$isRef)) {
    
  }
  nErrorEnv$.isRef_has_been_set <- FALSE
  nErrorEnv$stateInfo <- character()
  ans
}

argTypeList2symbolTable <- function(argTypeList,
                                    origNames = NULL,
                                    isArg = rep(FALSE, length(argTypeList)),
                                    isRef = list(),
                                    explicitTypeList = list(),
                                    evalEnv = parent.frame()
                                    ) {
  ## argTypeList is the argument-type list from run-time args to a.nCompiler function
  ## This function creates a symbolTable from the argument-type list.

  ## Begin error-trapping on arguments
  ## 1. Check that argTypeList is named list
  if(!is.list(argTypeList)) {
    stop(paste0("In argTypeList2symbolTable, ",
                "argTypeList must be a list."),
         call. = FALSE)
  }
  if(length(argTypeList) == 0) {
    return(symbolTableClass$new())
  }
  if(is.null(names(argTypeList)))
    stop(paste0("In argTypeList2symbolTable, ",
                "argTypeList must have named elements."),
         call. = FALSE)
  ## check that isArg is valid
  if(!is.list(isArg)) {
    ok <- FALSE
    if(is.logical(isArg)) {
      ok <- TRUE
      isArg <- as.list(isArg)
    }
    if(!ok) {
      stop(paste0("In argTypeList2symbolTable, ",
                  "isArg must be a list or logical vector."),
           call. = FALSE)
    }
  }
  if(length(isArg) != length(argTypeList)) {
    if(length(isArg) > 0)
      stop(paste0("In argTypeList2symbolTable, ",
                  "isArg must be the same length as argTypeList."),
           call. = FALSE)
  }
  if(is.null(names(isArg)))
    names(isArg) <- names(argTypeList)
  
  ## Check that isRef is valid
  if(!is.list(isRef)) {
    ok <- FALSE
    if(is.logical(isRef)) {
      ok <- TRUE
      isRef <- as.list(isRef)
    }
    if(!ok) {
      stop(paste0("In argTypeList2symbolTable, ",
                  "isRef must be a list or logical vector."),
           call. = FALSE)
    }
  }
  if(is.null(names(isRef))) {
    ok <- FALSE
    if(length(isRef) == length(argTypeList)) {
      ok <- TRUE
      names(isRef) <- names(argTypeList)
    }
    if(!ok) {
      if(length(isRef) > 0)
        stop(paste0("In argTypeList2symbolTable, ",
                    "isRef must be named or be the same length as argTypeList."),
             call. = FALSE)
    }
  }
  if(!is.list(explicitTypeList)) {
    stop(paste0("In argTypeList2symbolTable, ",
                "explicitTypeList must be a list."),
         call. = FALSE)
  }
  if(is.null(names(explicitTypeList))) {
    ok <- FALSE
    if(length(explicitTypeList) == length(argTypeList)) {
      ok <- TRUE
      names(explicitTypeList) <- names(argTypeList)
    }
    if(!ok) {
      if(length(explicitTypeList) > 0)
        stop(paste0("In argTypeList2symbolTable, ",
                    "explicitTypeList must be named or be the same length as argTypeList."),
             call. = FALSE)
    }
  }
  ## End error-trapping on arguments
  symTab <- symbolTableClass$new()
  if(is.null(origNames))
    origNames <- names(argTypeList)
  for(i in seq_along(argTypeList)) {
    thisName <- names(argTypeList)[i]
    symTab$addSymbol(
      argType2symbol(argTypeList[[i]],
                     thisName,
                     origNames[i],
                     isArg = isArg[[thisName]],
                     isRef = isRef[[thisName]],
                     explicitType = explicitTypeList[[thisName]],
                     evalEnv = evalEnv)
    )
  }
  symTab
}

resolveOneTBDsymbol <- function(symbol, env = parent.frame()) {
  if(inherits(symbol, "symbolTBD")) {
    candidate <- nGet(symbol$type,
                      where = env)
    if(isNCgenerator(candidate)) {
      newSym <- symbolNC$new(name = symbol$name,
                             type = symbol$type,
                             isArg = symbol$isArg,
                             NCgenerator = candidate)
      return(newSym)
    }
  }
  symbol #return unmodified symbol if nothing to do
}

resolveTBDsymbols <- function(symTab, 
                              env = parent.frame()) {
  for(i in seq_along(symTab$symbols)) {
    symTab$symbols[[i]] <- resolveOneTBDsymbol(symTab$symbols[[i]], env)
  }
  invisible(NULL)
}
