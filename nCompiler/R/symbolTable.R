symbolBase <- R6::R6Class(
  classname = 'symbolBase',
  portable = TRUE,
  public = list(
    name = NULL,
    type = character(),
    isRef = FALSE,
    isArg = FALSE,
    interface = TRUE,
    implementation = NULL,
    initialize = function(name = NULL,
                          type = character(),
                          isArg = FALSE,
                          isRef = FALSE,
                          interface = TRUE,
                          implementation = NULL) {
      self$name <- name
      self$type <- type
      self$isArg <- isArg
      self$isRef <- isRef
      self$interface <- interface
      self$implementation <- implementation
    },
    shortPrint = function() {
      self$type
    },
    resolveSym = function(...) self,
    uniqueID = function(...) {
      stop("uniqueID() is not yet implemented for a symbol of class ", class(self)[1], ".")
    },
    cpp_typename = function(...) {
      self$genCppVar()$generate("") |> trimws()
    },
    generateUse = function(...) self$name
  )
)

symbolVoid <- R6::R6Class(
  classname = 'symbolVoid',
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    initialize = function(...) {
      super$initialize(type = 'void', ...)
    },
    shortPrint = function()"void",
    uniqueID = function() "void",
    print = function() "void",
    genCppVar = function() {
      cppVoid()
    }
  )
)

## nDim and size are redundant for convenience with one exception:
## nDim = 0 must have size = 1 and means it is a true scalar -- NOT sure this is correct anymore...
## nDim = 1 with size = 1 means it is a 1D vector that happens to be length 1
## size may end up not being used, but at least in principle it is to define a C++ object of FIXED size.
## knownSize is added for some backward compatibility needs with nimble
##   In cases such as x[3:3, 2:4] or x[c(3), 2:4], nimble's system for 
##   symbolic sizeExprs determines that 3:3 or c(3) have sizeExpr "1"
##   and then the bracket processing will drop the index (if drop=TRUE)
symbolScalarOrTensor <-
  R6::R6Class(
    classname = 'symbolScalarOrTensor',
    inherit = symbolBase,
    portable = TRUE,
    public = list(
      nDim  = NULL,
      size = NULL,
      knownSize = NULL,
      isBlockRef = FALSE,
      isConst = FALSE,
      initialize = function(...,
                            nDim = 0,
                            size = if(nDim == 0) 1 else NA,
                            isBlockRef = FALSE,
                            isConst = FALSE) {
        super$initialize(...)
        self$nDim <- nDim
        self$size <- size
        self$isBlockRef <- isBlockRef
        self$isConst <- isConst
        self
      },
      shortPrint = function(prefix = '') {
        paste0(prefix, self$nDim)
      },
      uniqueID = function(...) {
        self$shortPrint()
      },
      print = function() {
        if(is.null(self$size)) {
          writeLines(paste0(self$name, ': ', self$type,
                            ' sizes = (uninitialized),',
                            ' nDim = ', self$nDim)
                     )
        } else {
          writeLines(paste0(self$name, ': ', self$type,
                            ' sizes = (', paste(self$size, collapse = ', '), '),',
                            ' nDim = ', self$nDim)
                     )
        }
      },
      genCppVar = function(cType = NULL) {
        if(is.null(cType)) stop("should not be calling symbolScalarOrTensor$genCppVar without a cType")
        isArg <- self$isArg
        if(self$nDim == 0) {
          if(identical(self$name, "pi"))
            return(cppVarFullClass$new(baseType = cType,
                                  name = self$name,
                                  ptr = 0,
                                  ref = FALSE,
                                  constructor = "(M_PI)"))
          if(isTRUE(self$isConst))
            return(cppVarFullClass$new(baseType = cType,
                            name = self$name,
                            ptr = FALSE,
                            ref = self$isRef,
                            const = self$isConst))
          return(cppVarClass$new(baseType = cType,
                            name = self$name,
                            ptr = 0,
                            ref = self$isRef))
        }
        if(self$isBlockRef) {
          ans <- cppStridedTensorMapRef(name = self$name,
                                        nDim = self$nDim,
                                        scalarType = cType)
        } else if(self$isRef) {
          ans <- cppEigenTensorRef(name = self$name,
                                   nDim = self$nDim,
                                   scalarType = cType)
        } else {
          ans <- cppEigenTensor(name = self$name,
                                nDim = self$nDim,
                                scalarType = cType)
        }
        if(self$isConst) {
          ans$const <- TRUE
        }
        ans
      }
    )
  )

symbolBasic <- R6::R6Class(
  classname = "symbolBasic",
  inherit = symbolScalarOrTensor,
  portable = TRUE,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    shortPrint = function() {
      prefix <- switch(self$type,
                    double = 'D',
                    integer = 'I',
                    logical = 'L',
                    AD = 'AD',
                    'Other')
      super$shortPrint(prefix = prefix)
    },
    genCppVar = function() {
      cType <- NULL
      type <- self$type
      if(type == 'integer') cType <- 'int'
      else if(type == 'double') cType <- 'double'
      else if(type == 'logical') cType <- 'bool'
      else if(type == 'AD') cType <- 'CppAD::AD<double>'
      else warning(paste("in genCppVar method for",
                    self$name,
                    "in symbolBasic class,",
                    "type", type,"unrecognized\n"),
              FALSE)
      super$genCppVar(cType = cType)
    }

  )
)

symbolBasicString <- R6::R6Class(
  classname = "symbolBasicString",
  inherit = symbolScalarOrTensor,
  portable = TRUE,
  public = list(
    initialize = function(...) {
      super$initialize(..., type = "string")
    },
    shortPrint = function() {
      super$shortPrint(prefix = 'S')
    },
    genCppVar = function() {
      super$genCppVar(cType = 'std::string')
    }
  )
)

symbolBlank <- R6::R6Class(
  classname = "symbolBlank",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    initialize = function(){},
    print = function() {
      writeLines(paste0('symbolBlank'))
    },
    genCppVar = function() {
      cppBlank()
    }
  )
)

symbolNF <- R6::R6Class(
  classname = "symbolNF",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    returnSym = NULL, ## Not always needed.
    initialize = function(name,
                          returnSym = NULL) {
      self$name <- name
      self$returnSym <- returnSym
    },
    print = function() {
      writeLines(paste0(self$name, ': nFunction'))
    },
    uniqueID = function(...) {
      self$name # we don't seem to have a type?
    },
    genCppVar = function() {
      stop("Attempting to create a cppVar for a symbolNF")
    }
  )
)

# Symbol for names that will be looked up via R scoping
# during compiler stage labelAbstractTypes.
# This includes nClass types.
symbolTBD <- R6::R6Class(
  classname = "symbolTBD",
  inherit = symbolBase,
  portable = TRUE,
  # The where (formerly evalEnv) should be known
  # when the symbol is created. The project_env
  # may not be known until used, so it is a compile-time
  # (not declaration-time)
  # argument of some methods.
  public = list(
    initialize = function(..., typeSpec = NULL, 
                          quo = NULL, 
                          where = NULL) {
      super$initialize(...)
      self$typeSpec <- typeSpec
      self$quo <- quo
      self$where <- where
    },
    # type will be the deparsed normalized input (i.e. converted to a call)
    typeSpec = NULL, # result of nTypeSpec
    quo = NULL, # original captured quosure (with any quote() layer removed)
    where = NULL, # environment from nFunction or nClass scoping
    print = function() {
      writeLines(paste0(self$name,
                        ": symbolTBD of type '",
                        self$type, "'"))
    },
    check_unknown_types = function(returnID = FALSE,
                                  project_env = NULL) {
      ttype <- self$quo
      candidate <- check_unknown_types(type = {{ttype}},
                                project_env = project_env %||% self$project_env,
                                where = self$where,
                                returnID = returnID,
                                typeSpec = self$typeSpec)
    },
    resolveSym = function(project_env = NULL, ...) {
      # This is needed when called from genCppVar as an inner variable such as for nCppVec
      # Possible redundance with these env's, which if provided
      # can over-rule the member env's
      candidate <- self$check_unknown_types(returnID = FALSE,
                                            project_env = project_env)
      if(isNCgenerator(candidate)) {
        newSym <- symbolNC$new(name = self$name,
                             type = NCinternals(candidate)$cpp_classname, # will this work for the type field??
                             isArg = self$isArg,
                             NCgenerator = candidate)
        return(newSym)
      } else {
        stop("In resolveSym method for symbolTBD (", self$name, ", ", self$type, "), could not resolve an nClass generator.")
      }
    },
    cpp_typename = function(project_env = NULL, ...) {
      sym <- self$resolveSym(project_env, ...)
      return(sym$cpp_typename(project_env, ...))
      # candidate <- self$check_unknown_types(returnID = FALSE,
      #                                       project_env = project_env)
      # if(isNCgenerator(candidate)) {

      #   return(NCinternals(candidate)$cpp_classname |> trimws())
      # } else {
      #   stop("In cpp_classname method for symbolTBD (", self$name, ", ", self$type, "), could not resolve an nClass generator.")
      # }
    },
    uniqueID = function(project_env = NULL, ...) {
      # If the type was provided like "nCppVec(some_custom_type())",
      # the string literal aspect will make the quosure have
      # env = emptyenv() and if an env is needed for the
      # inner type it is essentially lost. For now to cover
      # basic cases we use evalEnv or (if NULL) default to .GlobalEnv,
      # which will cover
      # cases where the inner type is a base type.
      # This will fail if a user provides "nList(T(mytype))"
      # as a character string.
      # where mytype only exists in the environment where 
      # mytype was created by nType(). That environment
      # will not be retained in the quosure and is also not
      # the environment of the nFunction or nClass call. 
      # A user
      # can provide the type as an expression, nList(T(mytype)),
      # not as character string, to get type scoping.
      # In future we could either more cleverly grab an 
      # actual environment when needed (but this will be unstable
      # for the reasons that quosures grab the PROMISE environment
      # which is non-trivial in multiple call layers). or we
      # could deprecate the idea of using strings for types,
      # or we could document that their use is limited if in 
      # inner type needs to be found by scoping.
      ID <- self$check_unknown_types(returnID = TRUE,
                                    project_env = project_env %||% self$project_env)
      ID
    },
    genCppVar = function() {
      stop("Trying to generate a C++ type from a TBD type ('",
           self$name,
           "' of type '",
           self$type, "').")
    }
  )
)

## Possible TO-DO: do not store the NCgenerator in the symbol.
## Instead, find it by scoping every time it is needed.
## type is the uniqueID of the NCgenerator.
symbolNC <- R6::R6Class(
  classname = "symbolNC",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    NCgenerator = NULL,
    initialize = function(name,
                          type,
                          NCgenerator,
                          isArg,
                          implementation = NULL) {
      self$name <- name
      self$type <- type
      self$NCgenerator <- NCgenerator
      self$isArg <- isArg
##      self$isRef <- TRUE
      self$implementation <- implementation
    },
    print = function() {
      writeLines(paste0(self$name, ': symbolNC of type ', self$type))
    },
    uniqueID = function(...) {
      NCinternals(self$NCgenerator)$classID
    },
    genCppVar = function() {
      cppSharedPtrToNC(name = self$name,
                       NCtype = NCinternals(self$NCgenerator)$cpp_classname)
    }
  )
)

## Symbol for `self` inside nClass method bodies.
## generateUse() emits nC_shared_from_this() so that `self` used as a value
## (passed as argument or returned) produces a std::shared_ptr<ClassName>.
## For method calls (self$method(x)), the DollarSign handler in
## compile_labelAbstractTypes uses the inherited NCgenerator for lookup,
## and PtrMember in compile_generateCpp emits (nC_shared_from_this())->method(x).
symbolSelf <- R6::R6Class(
  classname = "symbolSelf",
  inherit = symbolNC,
  portable = TRUE,
  public = list(
    initialize = function(name, type, NCgenerator, isArg = FALSE) {
      super$initialize(name = name,
                       type = type,
                       NCgenerator = NCgenerator,
                       isArg = isArg)
    },
    # Note that the genCppOutput handlers for 'Method' and 'Member'
    # intercept this. If they see a name "self" with type that inherits from "symbolSelf",
    # they will emit "this" instead of generating a cppVar for it.
    # The cppVar for self is only used if self is used as a value, such as in an argument or return value.
    genCppVar = function() cppVarSelfClass$new()
  )
)

## type is the unique ID of the NCgenerator.
## same value as for a symbolNC for an object of the class.
symbolNCgenerator <- R6::R6Class(
  classname = "symbolNCgenerator",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    NCgenerator = NULL,
    initialize = function(name,
                          type,
                          NCgenerator,
                          implementation = NULL) {
      self$name <- name
      self$type <- type
      self$NCgenerator <- NCgenerator
      self$implementation <- implementation
    },
    print = function() {
      writeLines(paste0(self$name, ': symbolNCgenerator of type ', self$type))
    },
    uniqueID = function(...) {
      NCinternals(self$NCgenerator)$classID
    },
    genCppVar = function() {
      cppNCgenerator(name = self$name,
                     baseType = NCinternals(self$NCgenerator)$cpp_classname)
    })
)

symbolNcppVec <- R6::R6Class(
  classname = "symbolNcppVec",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    elementSym = NULL,
    initialize = function(name=NULL, elementSym = NULL,
                          isArg = FALSE) {
      self$name <- name
      self$elementSym <- elementSym
      self$isArg <- isArg
      self$type <- "nCppVec"
      self
    },
    shortPrint = function() {
      'nCppVec'
    },
    resolveSym = function(project_env = NULL, ...) {
      elemSym <- self$elementSym
      if(inherits(elemSym, "symbolTBD")) {
        elemSym <- elemSym$resolveSym(project_env)
        #resolveOneTBDsymbol(elementSym, env, project_env)
        newSym <- self$clone(deep=TRUE)
        newSym$elementSym <- elemSym
        return(newSym)
      }
      self
    },
    cpp_typename = function(project_env = NULL, ...) {
      # This is the same as genCppVar except
      # that it has the benefit of the project_env
      elemSym <- self$elementSym
      if(inherits(elemSym, "symbolTBD")) {
        elemSym <- elemSym$resolveSym(project_env, ...)
      }
      cppVar <- cppNcppVec(name = self$name,
                        elementVar = elemSym$genCppVar())
      return(cppVar$generate("") |> trimws())
    },
    uniqueID = function() {
      elementID <- self$elementSym$uniqueID()
      paste0("nCppVec_", elementID)
    },
    print = function() {
      writeLines(
        paste0(self$name, ': ', self$type, '(',
               if(!is.null(self$elementSym)) self$elementSym$shortPrint() else "unknown!"
              ,')')
      )
    },
    genCppVar = function() {
      elemSym <- self$elementSym
      if(inherits(elemSym, "symbolTBD")) {
        # temporarily resolve the symbolTBD
        # This could be repetitive. If elemSym
        # needs resolution at compile-time,
        # such as from an nClassBuilder call,
        # then this will fail. However,
        # elementSym *should* be resolved by the
        # time this is used for actual code generation.
        elemSym <- elemSym$resolveSym()
      }
      return(cppNcppVec(name = self$name,
                      elementVar = elemSym$genCppVar()))
    }
  )
)

symbolETaccBase <- R6::R6Class(
  classname = "symbolETaccBase",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    initialize = function(...) {
      super$initialize(...)
      self$type <- "ETaccessorBase"
    },
    print = function() {
      writeLines(paste0(self$name, ': symbolETaccBase (ETaccessorBase) '))
    },
    uniqueID = function(...) {
      paste0("ETaccessorBase")
    },
    genCppVar = function() {
      cppETaccBase(name = self$name, ref = self$isArg)
    }
  )
)

symbolRcppType<- R6::R6Class(
  classname = "symbolRcppType",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    size = NULL,
    initialize = function(..., RcppType = NA, size = NA) {
      super$initialize(...)
      self$type <- RcppType
      self$size <- size
      self
    },
    shortPrint = function() {
      self$type
    },
    uniqueID = function() {
      stop("uniqueID() is not yet implemented for symbolRcppType.")
    },
    print = function() {
      writeLines(
        paste0(self$name, ': ', self$type)
      )
    },
    genCppVar = function() {
      return(cppRcppType(name = self$name, baseType = self$type))
    }
  )
)


symbolRcppNumericVector <- R6::R6Class(
  classname = "symbolRcppNumericVector",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    size = NULL,
    initialize = function(..., size = NA) {
      super$initialize(...)
      self$type = 'RcppNumericVector'
      self$size <- size
      self
    },
    shortPrint = function() {
      'RcppNumVec'
    },
    print = function() {
      if(is.null(self$size)) {
        writeLines(
          paste0(self$name, ': ', self$type, ' size = (uninitialized),')
        )
      } else {
        writeLines(
          paste0(self$name, ': ', self$type, ' size = ', self$size)
        )
      }
    },
    genCppVar = function() {
      return(cppRcppNumericVector(name = self$name))
    }
  )
)

symbolRcppNumericMatrix <- R6::R6Class(
  classname = "symbolRcppNumericMatrix",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    size = NULL,
    nDim = NULL,
    initialize = function(..., size = NA) {
      super$initialize(...)
      self$type = 'RcppNumericMatrix'
      self$size <- size
      self$nDim <- 2
      self
    },
    shortPrint = function() {
      'RcppNumMtx'
    },
    print = function() {
      if(is.null(self$ncol)) {
        writeLines(
          paste0(self$name, ': ', self$type, ' size = (uninitialized),')
        )
      } else {
        writeLines(
          paste0(self$name, ': ', self$type, ' size = ', size)
        )
      }
    },
    genCppVar = function() {
      return(cppRcppNumericMatrix(name = self$name))
    }
  )
)

symbolRcppDataFrame <- R6::R6Class(
  classname = "symbolRcppDataFrame",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    nrow = NULL,
    ncol = NULL,
    initialize = function(..., nrow = NA, ncol = NA) {
      super$initialize(...)
      self$type = 'symbolRcppDataFrame'
      self$nrow <- nrow
      self$ncol <- ncol
      self
    },
    shortPrint = function() {
      'RcppDataFrame'
    },
    print = function() {
      if(is.null(self$ncol)) {
        writeLines(
          paste0(self$name, ': ', self$type, ' (uninitialized),')
        )
      } else {
        writeLines(
          paste0(self$name, ': ', self$type, ' nrow = ', nrow, ', ncol = ', ncol)
        )
      }
    },
    genCppVar = function() {
      return(cppRcppDataFrame(name = self$name))
    }
  )
)

symbolRcppEigenMatrixXd <- R6::R6Class(
  classname = "symbolRcppEigenMatrixXd",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    size = NULL,
    nDim = NULL,
    initialize = function(..., size = NA) {
      super$initialize(...)
      self$type = 'symbolRcppEigenMatrixXd'
      self$size <- size
      self
    },
    shortPrint = function() {
      'RcppEigenMtxXd'
    },
    print = function() {
      if(is.null(self$ncol)) {
        writeLines(
          paste0(self$name, ': ', self$type, ' size = (uninitialized),')
        )
      } else {
        writeLines(
          paste0(self$name, ': ', self$type, ' size = ', size)
        )
      }
    },
    genCppVar = function() {
      return(cppRcppEigenMatrixXd(name = self$name))
    }
  )
)

symbolSparse <- R6::R6Class(
  classname = "symbolSparse",
  inherit = symbolBasic,
  portable = TRUE,
  public = list(
    initialize = function(...,
                          nDim = 0,
                          size = if(nDim == 0) 1 else NA) {
      super$initialize(...)
      self$nDim <- nDim
      self$size <- size
      self
    },
    shortPrint = function() {
      paste0('sp',
             switch(self$type,
                    double = 'D',
                    integer = 'I',
                    logical = 'L',
                    AD = 'AD',
                    'Other'),
             self$nDim)
    },
    print = function() {
      if(is.null(self$size)) {
        writeLines(paste0(self$name, ': sparse ', self$type,
                          ' sizes = (uninitialized),',
                          ' nDim = ', self$nDim)
                   )
      } else {
        writeLines(paste0(self$name, ': sparse ', self$type,
                          ' sizes = (', paste(self$size, collapse = ', '), '),',
                          ' nDim = ', self$nDim)
                   )
      }
    },
    genCppVar = function() {
      isArg <- self$isArg
      type <- self$type
      if(type == 'integer') cType <- 'int'
      else if(type == 'double') cType <- 'double'
      else if(type == 'logical') cType <- 'bool'
      else if(type == 'AD') cType <- 'CppAD::AD<double>'
      else warning(paste("in genCppVar method for",
                         self$name,
                         "in symbolSparse class,",
                         "type", type,"unrecognized\n"),
                   FALSE)
      if(self$nDim == 0) {
        return(if(!(identical(self$name, "pi")))
          cppVarClass$new(baseType = cType,
                          name = self$name,
                          ptr = 0,
                          ref = FALSE)
          else
            cppVarFullClass$new(baseType = cType,
                                name = self$name,
                                ptr = 0,
                                ref = FALSE,
                                constructor = "(M_PI)")
          )
      }
      if(self$isRef) {
        return(cppEigenSparseRef(name = self$name,
                                 nDim = self$nDim,
                                 scalarType = cType))
      } else {
        return(cppEigenSparse(name = self$name,
                              nDim = self$nDim,
                              scalarType = cType))
      }
    }
  )
)



## This was an exercise in conversion from the old system.
## I'm not sure this is or will be needed.
## If it is needed, it does not have AD included in the options below.
symbolEigenMap <- R6::R6Class(
  classname = 'symbolEigenMap',
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    eigMatrix = NULL, 	#'logical',
    strides = NULL, 		# 'numeric'
    initialize = function(..., eigMatrix = NULL, strides = NULL) {
      self$eigMatrix <- eigMatrix
      self$strides <- strides
      super$initialize(...)
      self
    },
    print = function() {
      writeLines(paste0(name,
                        ': Eigen ',
                        if(eigMatrix) 'Matrix' else 'Array',
                        ' Map of ',
                        type,
                        if(length(strides) > 0)
                          paste0(' with strides ', paste(strides, collapse = ', '))
                        else
                          character())
                 )
    },
    genCppVar = function(functionArg = FALSE) {
      if(functionArg)
        stop('Error: cannot take Eigen Map as a function argument (without more work).')
      if(length(strides)==2 & eigMatrix) {
        if(all(is.na(strides))) {
          baseType <- paste0('EigenMapStr',
                             if(type == 'double')
                               'd'
                             else {
                               if(type == 'integer')
                                 'i'
                               else
                                 'b' }
                             )
          return(cppVarFull$new(name = name,
                                baseType = baseType,
                                constructor = '(0,0,0, EigStrDyn(0, 0))',
                                ptr = 0,
                                static = FALSE))
        }
      }
      cppEigenMap$new(name = name,
                      type = type,
                      strides = strides,
                      eigMatrix = eigMatrix)
    }
  )
)

symbolCppVar <- R6::R6Class(
  classname = 'symbolCppVar',
  inherit = symbolBase,
  public = list(
    internalCppVar = NULL,
    initialize = function(..., symbolBaseArgs=list()) {
      self$internalCppVar <- cppVarFullClass$new(...)
      if(is.null(symbolBaseArgs$interface))
        symbolBaseArgs$interface <- FALSE # DEFAULT TO: NO INTERFACE (if it is being provided in a custom way, it probably can't be interfaced)
      do.call(super$initialize, symbolBaseArgs)
      self$type <- self$internalCppVar$generate(printName = "")
    },
    print = function() {
      writeLines(paste0(self$name, ': (C++) ', self$internalCppVar$generate()))
    },
    genCppVar = function() {
      self$internalCppVar$name <- self$name
      self$internalCppVar # Should it be cloned?
    }
  )
)
