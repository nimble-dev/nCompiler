symbolBase <- R6::R6Class(
  classname = 'symbolBase',
  portable = TRUE,
  public = list(
    name = NULL,
    type = character(),
    isRef = FALSE,
    isArg = FALSE,
    implementation = NULL,
    initialize = function(name = NULL,
                          type = character(),
                          isArg = FALSE,
                          isRef = FALSE,
                          implementation = NULL) {
      self$name <- name
      self$type <- type
      self$isArg <- isArg
      self$isRef <- isRef
      self$implementation <- implementation
    },
    shortPrint = function() {
      self$type
    },
    generateUse = function(...) self$name
  )
)

## nDim and size are redundant for convenience with one exception:
## nDim = 0 must have size = 1 and means it is a true scalar -- NOT sure this is correct anymore...
## nDim = 1 with size = 1 means it is a 1D vector that happens to be length 1
symbolBasic <-
  R6::R6Class(
    classname = 'symbolBasic',
    inherit = symbolBase,
    portable = TRUE,
    public = list(
      nDim  = NULL,
      size = NULL,
      initialize = function(...,
                            nDim = 0,
                            size = if(nDim == 0) 1 else NA) {
        super$initialize(...)
        self$nDim <- nDim
        self$size <- size
        self
      },
      shortPrint = function() {
        paste0(switch(self$type,
                      double = 'D',
                      integer = 'I',
                      logical = 'L',
                      'Other'),
               self$nDim)
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
      genCppVar = function() {
        isArg <- self$isArg
        type <- self$type
        if(type == 'void') return(cppVoid())
        else if(type == 'integer') cType <- 'int'
        else if(type == 'double') cType <- 'double'
        else if(type == 'logical') cType <- 'bool'
        else if(type == 'string') cType <- 'std::string'
        else warning(paste("in genCppVar method for",
                           self$name,
                           "in symbolBasic class,",
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
          return(cppEigenTensorRef(name = self$name,
                                   nDim = self$nDim,
                                   scalarType = cType))
        } else {
          return(cppEigenTensor(name = self$name,
                                nDim = self$nDim,
                                scalarType = cType))
        }
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
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    print = function() {
      writeLines(paste0(self$name,
                        ": symbolTBD of type '",
                        self$type, "'"))
    },
    genCppVar = function() {
      stop("Trying to generate a C++ type from a TBD type ('",
           self$name,
           "' of type '",
           self$type, "'.")
    }
  )
)

## Possible TO-DO: do not store the NCgenerator in the symbol.
## Instead, find it by scoping every time it is needed.
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
    genCppVar = function() {
      cppSharedPtrToNC(name = self$name,
                       NCtype = self$NCgenerator$classname)
    }
  )
)

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
    genCppVar = function() {
      cppNCgenerator(name = self$name,
                     baseType = self$NCgenerator$classname)
    })
)

symbolList <- R6::R6Class(
  classname = "symbolList",
  inherit = symbolBase,
  portable = TRUE,
  public = list(
    size = NULL,
    initialize = function(..., size = NA) {
      super$initialize(...)
      self$type = 'list'
      self$size <- size
      self
    },
    shortPrint = function() {
      'List'
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
      return(cppRcppList(name = self$name))
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
      if(type == 'void') return(cppVoid())
      else if(type == 'integer') cType <- 'int'
      else if(type == 'double') cType <- 'double'
      else if(type == 'logical') cType <- 'bool'
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
