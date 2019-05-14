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
                if(isArg) {
                    EigenUseRef <- TRUE
                    if(EigenUseRef) {
                        ## Use plain tensor for experiments with Exporter
                        return(cppEigenTensor(name = self$name,
                                              nDim = self$nDim,
                                              scalarType = cType))
                        ## return(cppEigenTensorRef(name = self$name,
                        ##                          nDim = self$nDim,
                        ##                          scalarType = cType))
                    } else {
                        return(cppTemplate(name = self$name,
                                           ref = TRUE,
                                           const = TRUE))
                    }
                } else {
                    return(cppEigenTensor(name = self$name,
                                          nDim = self$nDim,
                                          scalarType = cType))
                }
            }
        )
    )

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
      self$isRef <- TRUE
      self$implementation <- implementation
    },
    print = function() {
      writeLines(paste0(name, ': symbolNC of type', type))
    },
    genCppVar = function() {
      cppSharedPtrToNC(name = self$name,
                       NCtype = self$type)
    }
  )
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
