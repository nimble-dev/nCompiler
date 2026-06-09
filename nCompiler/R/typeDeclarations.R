
## How creating and passing around types works
## The raw expression information of types is recorded 
## in rlang quosures. The idiom for programming with 
## a type as an input is:
## function(type) {
##   ttype <- nCaptureType(type)
##   next_res <- foo({{ttype}})
## }
## This also means that the above function can be
## called with with arguments like "type = numericVector()", 
##. type = 'numericVector', or type = {{ type_var }},
## where type_var was either the result of nCaptureType
## a call like nType(numericVector()) or nType("numericVector")

nTypeSpec <- function(type = NULL) {
  if(rlang::is_quosure(type)) {
    texpr <- rlang::quo_get_expr(type)
  } else {
    texpr <- type
  }
  if(is.null(texpr)) return(NULL)
  typeToUse <- texpr
  inputAsCharacter <- FALSE
  if(is.character(typeToUse)) {
    typeToUse <- str2lang(typeToUse)
    inputAsCharacter <- TRUE # This might become unnecessary but in past experience uses have cropped up for it.
  }
  ## allow e.g. 'scalarInteger' to become scalarInteger()
  if(is.name(typeToUse)) {
    funName <- deparse(typeToUse)
    args <- list()
  } else {
    funName <- deparse(typeToUse[[1]])
    args <- typeToUse[-1] |> as.list()
  }
  list(funName = funName, args = args, inputAsCharacter = inputAsCharacter) |> 
    structure(class = "nTypeSpec")
}

#' @export
nCaptureType <- function(arg) {
  # Note that for backward compatibility with nimble and convenience,
  # arg could be a quoted R expr, like quote(double(1))
  # In that case, the env of the quosure created here will be R_EmptyEnv (emptyenv())
  # We do not replace it here because at evaluation points we sometimes have
  # a "where" or "env" from scoping of an nClass or Function and we will use that.
  arg <- substitute(arg)
  substitute(rlang::enquo(ARG), list(ARG = arg)) |> eval(parent.frame())
}

#' @export
nType <- function(type, expr=NULL, env=parent.frame()) {
  if(is.null(expr)) {
    res <- nCaptureType(type)
    return(res)
  }
  res <- substitute(rlang::quo(EXPR), list(EXPR = expr)) |> eval(envir = env)
  res
}

#' @export
nTypeList <- function(..., .list = NULL, .where = parent.frame()) {
  if(!is.null(.list)) {
    res <- vector(mode = "list", length = length(.list))
    #pf <- parent.frame()
    for(i in seq_along(.list)) {
      new_quo <- substitute(rlang::quo(EXPR), list(EXPR = .list[[i]])) |> eval(envir = .where)
      # First idea was to populate this but it is better to leave env
      # as emptyenv() so later we know there was no properly captured
      # original env and we can instead use a scoped env from an nFunction or nClass
      #new_quo <- rlang::quo_set_env(new_quo, pf)
      res[[i]] <- new_quo
    }
    names(res) <- names(.list)
    return(res)
  }
  res <- rlang::enquos(..., .homonyms = "error")
  res
}

# nCaptureType <- function(arg, .substitute = TRUE) {
#   s_arg <- substitute(arg)
#   if(.substitute)
#     s_type <- substitute(substitute(ARG), list(ARG=s_arg)) |> eval(parent.frame())
#   else
#     s_type <- s_arg |> eval(parent.frame())
#   if(is.call(s_type)) {
#     if(length(s_type[[1]]) == 1) {
#       if(identical(as.character(s_type[[1]]), "T")) {
#         # In many cases from simple function calls, promise_env
#         # will be the same as parent.frame(2). However,
#         # promises could have been passed un-forced through
#         # additional call frames, and notably that will be the
#         # case in the initialize method of an R6 class generator
#         # Hence we obtain the environment of the promise,
#         # which requires a call to C++.
#         #
#         # Checking on .GlobalEnv catches on silly case of
#         # mis-use (not calling from a function) that we
#         # make "just work". Other cases of mis-use could
#         # give untrapped errors
#         if(identical(parent.frame(), .GlobalEnv))
#           promise_env <- .GlobalEnv
#         else
#           promise_env <- nCompiler:::get_promise_env(s_arg, parent.frame())
#         res <- s_type[[2]] |> eval(promise_env)
#         if(!inherits(res, "nTypeSpec"))
#           res <- nTypeSpec(res)
#         return(res)
#       }
#     }
#   }
#   res <- nTypeSpec(s_type)
#   res
# }

# #' @export
# nType <- function(type) {
#   nCaptureType(type)
# }

# #' @export
# nTypeList <- function(..., .list = NULL) {
#   if(!is.null(.list)) {
#     res <- .list |> lapply(function(x) nType(T(x)))
#     return(res)
#   }
#   expr <- eval(substitute(alist(...)))
#   res <- vector(mode = "list", length = length(expr))
#   for(i in seq_along(expr)) {
#     res[[i]] <- nCaptureType(expr[[i]], .substitute = FALSE)
#   }
#   names(res) <- names(expr)
#   res
# }

## The next sections of code handle types to symbols
##

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
nTypeBasic <- function(scalarType, nDim, isRef = FALSE, isBlockRef = FALSE,
                  interface = TRUE, ...) {
  symbolBasic$new(type = scalarType,
                  nDim = nDim,
                  isRef = isRef,
                  isBlockRef = isBlockRef,
                  interface = interface,
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
    tIntT <- nCaptureType(internalType)
    ans <- type2symbol({{tIntT}},
                          where = parent.frame(1))
    ans$isRef <- TRUE
    nErrorEnv$.isRef_has_been_set <- TRUE
    ans
  },
  blockRef = function(internalType) {
    ans <- type2symbol({{internalType}},
                          where = parent.frame(1))
    ans$isBlockRef <- TRUE
    nErrorEnv$.isBlockRef_has_been_set <- TRUE
    ans
  },
  ## integer types:
  integerScalar = function(value, ...) {
    nTypeBasic("integer", 0, ...)
  },
  integerVector = function(length = NA,
                           ...) {
    nTypeBasic("integer", 1, ...)
  },
  integerMatrix = function(value,
                           ...) {
    nTypeBasic("integer", 2, ...)
  },
  integerArray = function(value,
                          nDim = 1,
                          ...) {
    nTypeBasic("integer", nDim, ...)
  },
  ## logical types
  logicalScalar = function(value) {
    nTypeBasic("logical", 0)
  },
  logicalVector = function(length = NA,
                           ...) {
    nTypeBasic("logical", 1, ...)
  },
  logicalMatrix = function(value,
                           ...) {
    nTypeBasic("logical", 2)
  },
  logicalArray = function(value,
                          nDim = 1,
                          ...) {
    nTypeBasic("logical", nDim, ...)
  },
  ## numeric types
  numericScalar = function(value, ...) {
    nTypeBasic("double", 0, ...)
  },
  numericVector = function(length = NA,
                           ...) {
    nTypeBasic("double", 1, size = length, ...)
  },
  numericMatrix = function(value,
                           ...) {
    nTypeBasic("double", 2, ...)
  },
  numericArray = function(value,
                          nDim = 1,
                          ...) {
    nTypeBasic("double", nDim, ...)
  },
  ## AD types
  ADScalar = function(value, ...) {
    nTypeBasic("AD", 0, ...)
  },
  ADVector = function(length = NA,
                      ...) {
    nTypeBasic("AD", 1, size = length, ...)
  },
  ADMatrix = function(value,
                      ...) {
    nTypeBasic("AD", 2, ...)
  },
  ADArray = function(value,
                     nDim = 1,
                     ...) {
    nTypeBasic("AD", nDim, ...)
  },
  ## versions with type as a declared argument
  nScalar = function(...,
                     type = "double") {
    nTypeBasic(type, 0, ...)
  },
  nVector = function(value,
                     length,
                     ...,
                     type = "double") {
    nTypeBasic(type, 1, ...)
  },
  nMatrix = function(value,
                     ...,
                     type = "double") {
    nTypeBasic(type, 2, ...)
  },
  nArray = function(value,
                    dim,
                    ...,
                    type = "double") {
    nTypeBasic(type, length(dim), ...)
  },
  ## vector versions with type embedded in keyword
  nInteger = function(length = NA,
                      ...) {
    nTypeBasic("integer", 1, ...)
  },
  nLogical = function(length = NA,
                      ...) {
    nTypeBasic("logical", 1, ...)
  },
  nNumeric = function(length = NA,
                      ...) {
    nTypeBasic("double", 1, ...)
  },
  ## versions from original nimble
  double = function(nDim = 0,
                    ...) {
    nTypeBasic("double", nDim, ...)
  },
  integer = function(nDim = 0,
                     ...) {
    nTypeBasic("integer", nDim, ...)
  },
  logical = function(nDim = 0,
                     ...) {
    nTypeBasic("logical", nDim, ...)
  },
  void = function(...) {
    symbolVoid$new(...)
  },
  string = function(...) {
    symbolBasicString$new(nDim = 0, ...)
  },
  ##
  SEXP = function(...) {
    symbolCppVar$new(baseType = "SEXP", ...)
  },
  ## Rcpp types
  RcppEnvironment = function(...) {
    symbolRcppType$new(RcppType = "Rcpp::Environment", ...)
  },
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
  ## This is not a primary type. It is used within Rcpp vectors etc.
  ## RcppNamed = function(...) {
  ##   symbolRcppType$new(RcppType = "Rcpp::Named", ...)
  ## },
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
    nSparseType(scalarType = type, nDim = 2, ...)
  },
  nSparseVector = function(value,
                           ...,
                           type = "double") {
    nSparseType(scalarType = type, nDim = 1, ...)
  },
  nCppVec = function(type) {
    ttype <- nCaptureType(type)
    elementSym <- type2symbol({{ttype}}, where = parent.frame())
    symbolNcppVec$new(elementSym = elementSym)
  },
  ## determine type from an evaluated object
  typeDeclarationFromObject = function(x) {
    if(inherits(x, 'symbolBasic'))
      return(x)
    scalarType <- storage.mode(x)
    if(!(scalarType %in% c("integer",
                           "double",
                           "logical"))) {
      stop(paste0("storage.mode of an object used to declare an Compiler ",
                  "argument must be integer, double, or logical."),
           call. = FALSE)
    }
    nDim <- length(nDim(x))
    if(!(nDim >= 0 &
           nDim <= 6))
      stop(paste0("Invalid number of dimensions used to declare a nCompiler ",
                  "argument.  Dimensions from 0-6 are allowed."),
           call. = FALSE)
    nTypeBasic(scalarType, nDim)
  },
  ## O(obj) allows a syntax for explicitly saying there is an object 
  ## to look at for the type.
  O = function(x) {
    typeDeclarationList$typeDeclarationFromObject(x)
  },
  CppVar = function(...) { # symbolBaseArgs will be passed to symbolBase$initialize
    symbolCppVar$new(...)
  },
  nCpp = function(value, ...) {
    symbolCppVar$new(baseType = value, ...)
  },
  T = function(symbol) { ## This is semi-defunct but could be resurrected.
    # The use of T(mytype) indicates that mytype evaluates to an 
    # existing type object in the evalEnv.
    # So this is a splice point between base R expression handling
    # and rlang.
    symbol <- substitute(symbol)
    tsymbol <- eval(symbol, envir = parent.frame())
    # Add error trapping for whether tsymbol is a quosure
    # whose expr contains T(type_obj). This nested use of T() syntax
    # will not work if the quosure environments matter because
    # the inner T will lose its argument's environment because
    # we use T as a syntax tree marker only (so its arg isn't captured)
    symbol <- type2symbol({{tsymbol}}, where = parent.frame()) # this is called *from* the evalEnv via do.call with envir
    #    symbol$clone(deep=TRUE)
  }
  ## universal handler for creating symbolBasic objects
)

#' @export
type2cpp <- function(...) {
  type2symbol(...)$genCppVar()$generate()
}

quo_strip_quote <- \(qq) {
#  qq <- q
  if(identical(rlang::quo_get_expr(qq), rlang::missing_arg())) return(qq)
  e <- rlang::quo_get_expr(qq)  
  e <- if(is.call(e) && deparse1(e[[1]])=="quote") e[[2]] else e
  rlang::quo_set_expr(qq, e)
}

type2symbol <- function(type,
                        name = character(),
                        origName = "",
                        isArg = FALSE,
                        explicitType = NULL,
                        isRef = NULL,
                        isBlockRef = NULL,
                        where = parent.frame()) {
  nErrorEnv$stateInfo <- paste0("handling argument ", name, ".")
  nErrorEnv$.isRef_has_been_set <- FALSE
  nErrorEnv$.isBlockRef_has_been_set <- FALSE
  ttype <- nCaptureType(type)
  texplicitType <- nCaptureType(explicitType)
  ttype <- quo_strip_quote(ttype)
  texplicitType <- quo_strip_quote(texplicitType)
  
  if(!is.null(explicitType)) {
#    typeToUse <- explicitType
    ttypeToUse <- texplicitType
  } else {
#    typeToUse <- type
    ttypeToUse <- ttype
  }

  # The idea here was to check if a symbol itself is provided,
  # but now using rlang quosures, we don't want to assume we
  # can evaluate the type argument. 
  # 
  ## First check if what was provided is actually a symbol, and if so return a copy.
  ## This could be restricted to inherits(typeToUse, "symbolBase")
  ## but "R6" allows an even wider range of flexibility.
  # if(inherits(typeToUse, "R6")) {
  #   ans <- typeToUse$clone(deep=TRUE)
  #   ans$name <- name
  #   return(ans)
  # }

  if(ttypeToUse |> rlang::get_expr() |> inherits("symbolBase")) {
    ans <- ttypeToUse |> rlang::get_expr()
    ans$name <- name
    return(ans)
  }

  typeSpec <- nTypeSpec(ttypeToUse)
  inputAsLiteral <- identical(rlang::quo_get_env(ttypeToUse), emptyenv())
  inputAsCharacter <- typeSpec$inputAsCharacter
  ## allow e.g. 'scalarInteger' to become scalarInteger()
  #if(is.name(typeToUse))
  #  typeToUse <- as.call(list(typeToUse))

  ans <- try({
    ## TO-DO: Case 1: It is a nTypeBasic object
    ##    To be implemented
    ##
    ## Case 2: It is a valid declaration
    funName <- typeSpec$funName # deparse(typeToUse[[1]])
    handler <- typeDeclarationList[[funName]]
    if(!is.null(handler)) {
      symbol <- do.call(handler, typeSpec$args, envir = where) #as.list(typeToUse[-1]))
      symbol$name <- name
      symbol$isArg <- isArg
      if(!is.null(isRef)) {
        if(isTRUE(nErrorEnv$.isRef_has_been_set)) {
          nErrorEnv$.errorDetails <-
            paste0("The reference status for ", origName, " is being set in multiple ways.",
                   " That is not allowed.")
          stop(call.=FALSE)
        }
        symbol$isRef <- isRef
      }
      if(inherits(symbol, "symbolBasic")) {
        if(!inherits(symbol, "symbolSparse")) {
          if(!is.null(isBlockRef)) {
            if(isTRUE(nErrorEnv$.isBlockRef_has_been_set)) {
              nErrorEnv$.errorDetails <-
                paste0("The block reference status for ", origName, "is being set in multiple ways.",
                       " That is not allowed.")
              stop(call.=FALSE)
            }
            symbol$isBlockRef <- isBlockRef
          }
          if(symbol$isRef & symbol$isBlockRef) {
            nErrorEnv$.errorDetails <-
              paste0("Argument ", origName, " is marked as both a reference and a block reference.",
                     " That is not allowed.")
            stop(call.=FALSE)
          }
        }
      }
      # Next we will check if both a type and explicitType were provided
      # we evaluate the type and see if it (evaluated) is consistent with the explicitType.
      # It is not clear if this is really a good idea in all cases.
      #
      # Because input could be a quosure already, we need for null based on 
      # the expression of the quosure, which could be missing_arg or NULL.
      if(identical(rlang::quo_get_expr(texplicitType), rlang::missing_arg()) ||
          is.null(rlang::quo_get_expr(texplicitType)))
        explicitType <- NULL
      if(!is.null(explicitType)) {
        ## type could be a blank
        ## ttexpr <- rlang::quo_get_expr(ttype) # It does not even work to hold this in a variable
        ## because if it is the missing symbol, it explodes with error when used!
        if(identical(rlang::quo_get_expr(ttype), rlang::missing_arg()) ||
            is.null(rlang::quo_get_expr(ttype)))
          type <- NULL

        if(!is.null(type)) {
          checkObject <- if(!identical(rlang::quo_get_env(ttype), emptyenv()))
            rlang::eval_tidy(ttype)
          else
            eval(rlang::quo_get_expr(ttype), envir=where)
            # eval(type, envir = evalEnv) # ttype is a quosure of a default value, since the type spec is in explicitType
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
      ## We allow that character input could be a type specification, but other
      ## literal input should not be.
      ## Again it is not clear if this is really fully general.
      if(inputAsCharacter || !inputAsLiteral) {
        symbol <- symbolTBD$new(name = name,
                                # this type string normalized to appear as a deparsed call.
                                type = typeSpec$funName,
                                typeSpec = typeSpec,
                                quo = ttypeToUse,
                                isArg = isArg,
                                where = where)
      } else {
        ## Case 4: Type can be determined by evaluating the default
        ## This will really only work for scalar non-character literals,
        ## i.e. a number or logical, so it is not very useful.
        demoObject <- if(!identical(rlang::quo_get_env(ttype), emptyenv()))
          rlang::eval_tidy(ttype)
        else
          eval(rlang::quo_get_expr(ttype), envir=where) # eval(type, envir = evalEnv)
        symbol <-
          typeDeclarationList[["typeDeclarationFromObject"]](demoObject)
        symbol$name <- name
        symbol$isArg <- isArg
        if(isTRUE(isRef) | isTRUE(isBlockRef)) {
          nErrorEnv$.errorDetails <-
            paste0("A reference or block reference variable must have its type set by explicit declaration,",
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
  nErrorEnv$.isBlockRef_has_been_set <- FALSE
  nErrorEnv$stateInfo <- character()
  ans
}

typeList2symbolTable <- function(typeList,
                                origNames = NULL,
                                isArg = rep(FALSE, length(typeList)),
                                isRef = list(),
                                isBlockRef = list(),
                                explicitTypeList = list(),
                                where = parent.frame()
                                ) {
  ## typeList is the argument-type list from run-time args to compile_simpleTransformations.RnCompiler function
  ## This function creates a symbolTable from the argument-type list.

  ## Begin error-trapping on arguments
  ## 1. Check that typeList is named list
  if(!is.list(typeList)) {
    typeList <- try(as.list(typeList), silent = TRUE)
    if(inherits(typeList, 'try-error')) {
      stop(paste0("In typeList2symbolTable, ",
                  "typeList must be a list or coerceable to a list."),
           call. = FALSE)
    }
  }
  if(length(typeList) == 0) {
    return(symbolTableClass$new())
  }
  if(is.null(names(typeList)))
    stop(paste0("In typeList2symbolTable, ",
                "typeList must have named elements."),
         call. = FALSE)
  ## check that isArg is valid
  if(!is.list(isArg)) {
    ok <- FALSE
    if(is.logical(isArg)) {
      ok <- TRUE
      isArg <- as.list(isArg)
    }
    if(!ok) {
      stop(paste0("In typeList2symbolTable, ",
                  "isArg must be a list or logical vector."),
           call. = FALSE)
    }
  }
  if(length(isArg) != length(typeList)) {
    if(length(isArg) > 0)
      stop(paste0("In typeList2symbolTable, ",
                  "isArg must be the same length as typeList."),
           call. = FALSE)
  }
  if(is.null(names(isArg)))
    names(isArg) <- names(typeList)

  ## Check that isRef is valid
  if(!is.list(isRef)) {
    ok <- FALSE
    if(is.logical(isRef)) {
      ok <- TRUE
      isRef <- as.list(isRef)
    }
    if(!ok) {
      stop(paste0("In typeList2symbolTable, ",
                  "isRef must be a list or logical vector."),
           call. = FALSE)
    }
  }
  ## Check that isBlockRef is valid
  if(!is.list(isBlockRef)) {
    ok <- FALSE
    if(is.logical(isBlockRef)) {
      ok <- TRUE
      isBlockRef <- as.list(isBlockRef)
    }
    if(!ok) {
      stop(paste0("In typeList2symbolTable, ",
                  "isBlockRef must be a list or logical vector."),
           call. = FALSE)
    }
  }
  if(is.null(names(isRef))) {
    ok <- FALSE
    if(length(isRef) == length(typeList)) {
      ok <- TRUE
      names(isRef) <- names(typeList)
    }
    if(!ok) {
      if(length(isRef) > 0)
        stop(paste0("In typeList2symbolTable, ",
                    "isRef must be named or be the same length as typeList."),
             call. = FALSE)
    }
  }
  if(is.null(names(isBlockRef))) {
    ok <- FALSE
    if(length(isBlockRef) == length(typeList)) {
      ok <- TRUE
      names(isBlockRef) <- names(typeList)
    }
    if(!ok) {
      if(length(isBlockRef) > 0)
        stop(paste0("In typeList2symbolTable, ",
                    "isBlockRef must be named or be the same length as typeList."),
             call. = FALSE)
    }
  }
  if(!is.list(explicitTypeList)) {
    stop(paste0("In typeList2symbolTable, ",
                "explicitTypeList must be a list."),
         call. = FALSE)
  }
  if(is.null(names(explicitTypeList))) {
    ok <- FALSE
    if(length(explicitTypeList) == length(typeList)) {
      ok <- TRUE
      names(explicitTypeList) <- names(typeList)
    }
    if(!ok) {
      if(length(explicitTypeList) > 0)
        stop(paste0("In typeList2symbolTable, ",
                    "explicitTypeList must be named or be the same length as typeList."),
             call. = FALSE)
    }
  }
  ## End error-trapping on arguments

  ttypeList <- nTypeList(.list = typeList, .where = where)
  texplicitTypeList <- nTypeList(.list = explicitTypeList, .where = where)

  symTab <- symbolTableClass$new()
  if(is.null(origNames))
    origNames <- names(ttypeList)
  for(i in seq_along(ttypeList)) {
    thisName <- names(ttypeList)[i]
    this_ttype <- ttypeList[[i]]
    this_texplicitType <- texplicitTypeList[[thisName]]
    symTab$addSymbol(
      type2symbol({{this_ttype}},
                  thisName,
                  origNames[i],
                  isArg = isArg[[thisName]],
                  isRef = isRef[[thisName]],
                  isBlockRef = isBlockRef[[thisName]],
                  explicitType = {{this_texplicitType}},
                  where = where)
)
  }
  symTab
}

# take a type (quosure or ready to become one)
# and look for NCgenerator or nClassBuilder
check_unknown_types <- function(type, where = parent.frame(),
                                project_env = new.env(), returnID = FALSE,
                                typeSpec = NULL) {

  ttype <- nCaptureType(type)
  if(is.null(typeSpec)) {
    typeSpec <- nTypeSpec(ttype)
  }
  funName <- typeSpec$funName
  if(!identical(rlang::quo_get_env(ttype), emptyenv())) {
    where <- rlang::quo_get_env(ttype)
  }
  candidate <- nGet(funName,
                  where = where,
                  project_env = project_env) # project_env should not be relevant but can be checked in case of trickiness
  if(!isNCgenerator(candidate)) {
    candidate2 <- check_built_types(candidate = candidate,
            typeSpec = typeSpec, where = where,
            project_env = project_env,
            returnID = returnID)
    if(returnID) return(candidate2) # candidate2 *can* be NULL.
    candidate <- candidate2 %||% candidate
  }
  if(isNCgenerator(candidate)) {
    if(returnID) {
      return(NCinternals(candidate)$classID)
    }
    # return(candidate)
  }
  candidate
}

check_built_types <- function(Rexpr = NULL, candidate = NULL,
                              typeSpec = NULL,
                              where = parent.frame(),
                              project_env = new.env(), returnID = FALSE) {
  if(!is.null(Rexpr)) {
    if(!is.null(typeSpec)) {
      stop("In check_built_types, either Rexpr or typeSpec should be NULL.")
    }
    ttype <- nType(expr = Rexpr, env = where)
    typeSpec <- nTypeSpec(ttype)
    if(!is.null(candidate))
      candidate <- nGet(typeSpec$funName, where = where) #project_env not useful here
  }

  if(inherits(candidate, "nClassBuilder")) {
    args <- typeSpec$args
    args2 <- c(args, .ID=TRUE)
    ID <- do.call(candidate, args2, envir = where) # get the classID for this type
    if(returnID) return(ID)
    NCgen <- project_env$built_types[[ID]]
    if(is.null(NCgen)) {
      NCgen <- do.call(candidate, args, envir = where) # get the NCgenerator for this type
      project_env$built_types[[ID]] <- NCgen
    }
    ##cpp_classname <- NCinternals(NCgen)$cpp_classname
    ##list(NCgen) |> setNames(cpp_classname)
    NCgen
  } else 
    NULL
}

type2uniqueID <- function(type, where = parent.frame()) {
  ttype <- nCaptureType(type)
  symbol <- type2symbol({{ttype}}, where = where)
  symbol$uniqueID()
}

type2cpp_typename <- function(type, where = parent.frame()) {
  ttype <- nCaptureType(type)
  symbol <- type2symbol({{ttype}}, where = where)
  symbol$cpp_typename()
}

resolveOneTBDsymbol <- function(symbol, env = parent.frame(),
                                project_env = new.env()) {
  symbol <- symbol$resolveSym(project_env = project_env)
  symbol #return unmodified symbol if nothing to do
}

resolveTBDsymbols <- function(symTab,
                              env = parent.frame(),
                              project_env = new.env()) {
  for(i in seq_along(symTab$symbols)) {
    symTab$symbols[[i]] <- resolveOneTBDsymbol(symTab$symbols[[i]], env, project_env)
  }
  invisible(NULL)
}
