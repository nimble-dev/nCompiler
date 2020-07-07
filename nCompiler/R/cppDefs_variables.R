## Remove excess white spaces.  Keep one space.
cleanWhite <- function(s) gsub('[[:blank:]]+', ' ', s)

## This is a base class for c++ variables.
## To avoid having many unused fields in many cases, this can handle only ptrs and refs.
## If you need static, const, template arguments, or other adornments, use cppFullVar
## Below there are some wrappers for common cases like cppDouble, cppNimArrPtr, cppVoid, etc.
cppVarClass <- R6::R6Class(
  classname = 'cppVarClass',
  portable = TRUE,
  public = list(
    baseType = character(),
    ptr = numeric(),
    ref = logical(),
    name = character(),
    initialize = function(...) {
      dotsList <- list(...)
      for(v in names(dotsList))
        self[[v]] <- dotsList[[v]]
    },
    generate = function(printName = Rname2CppName(self$name),
                        ...) {
      ptrs <- if(length(self$ptr) > 0)
                paste(rep('*', self$ptr),
                      collapse = '')
      if(length(printName) > 0)
        printName <- paste0(printName, collapse = ', ')
      cleanWhite(paste(self$baseType,
                       self$ptrs,
                       if(isTRUE(self$ref))
                         '&'
                       else
                         NULL,
                       printName)
                 )
    },
    print = function() writeLines(self$generate()),
    generateUse = function(...) {
      Rname2CppName(self$name)
    },
    generateUseDeref = function(...) { 
      paste0('(',
             paste(rep('*', max(0, ptr)),
                   collapse = ''),
             Rname2CppName(name), ')'
             ) ## used to be ptr-asArg
    }
  )
)

cppVar2cppVarFull <- function(cppVar, ...) {
  ans <- cppVarFullClass$new(name = cppVar$name,
                             baseType = cppVar$baseType, 
                             ptr = cppVar$ptr,
                             ref = cppVar$ref,
                             ...)
  ans
}

## Here is the full version that can handle most c++ variable declarations.
## One thing that cannot be handled is function pointers or member pointers
cppVarFullClass <- R6::R6Class(
  classname = 'cppVarFullClass',
  inherit = cppVarClass,
  portable = TRUE,
  public = list(
    templateArgs = list(),
    baseScope = list(),
    baseConst = logical(),
    baseConstPtr = numeric(),
    const = logical(),
    static = logical(),
    arraySizes = integer(),
    constructor = character(),
    silent = FALSE,
    initialize = function(...) {
      dotsList <- list(...)
      for(v in names(dotsList))
        self[[v]] <- dotsList[[v]]
      super$initialize()
    },
    generateUse = function(deref, ...) {
      if(missing(deref)) {
        ##if(selfDereference) generateUseDeref(...)
        ##else super$generateUse(...)
        super$generateUse(...)
      } else {
        if(isTRUE(deref)) generateUseDeref(...)
        else super$generateUse(...)
      }
    },
    generate = function(printName = Rname2CppName(self$name), ...) {
      if(self$silent) return(character())
      bCP <- if(length(self$baseConst) > 0) { 
        if(length(self$baseConstPtr) > 0)
          paste(paste(rep('*', self$baseConstPtr),
                      collapse = ''),
                'const')
        else
          'const'
      }
      baseTypePlusTemplate <-
        if(length(self$templateArgs)==0)
          self$baseType
      else {
        expandedTemplateArgs <-
          unlist(lapply(self$templateArgs,
                        function(x) {
                          if(inherits(x, 'cppVarClass'))
                            return(x$generate())
                          return(as.character(x))
                        }
                        )
                 )
        paste0(self$baseType,
               '<',
               paste(expandedTemplateArgs,
                     collapse = ', '),
               '>')
      }
      ptrs <- if(length(self$ptr) > 0)
                paste(rep('*', self$ptr),
                      collapse = '')
      if(length(printName) > 0)
        printName <- paste0(printName, collapse = ', ')
      ans <- cleanWhite(
        paste(baseTypePlusTemplate,
              bCP,
              ptrs,
              if(length(self$const) > 0)
                'const',
              if(isTRUE(self$ref))
                '&'
              else
                NULL,
              printName)
      )
      if(length(self$arraySizes) > 0)
        ans <- paste0(ans,
                      '[',
                      paste0(self$arraySizes,
                             collapse =']['), ']'
                      )
      ans <- paste0(ans, self$constructor)
      if(length(self$static) > 0)
        if(self$static[1])
          ans <- paste('static', ans)
      ans
    }
  )
)

## Here are some wrappers for simple types
templateLabelGenerator <- labelFunctionCreator("TT")

cppTemplate <- function(name = character(0),
                        baseType,
                        ...) {
  if(missing(baseType))
    baseType <- templateLabelGenerator()
  cppVarFullClass$new(name = name,
                      baseType = baseType,
                      ref = TRUE,
                      ...)
}

cppTemplateDeclaration <- function(templateNames,
                                   ...) {
  cppVarFullClass$new(name = "",
                      baseType = "template",
                      templateArgs = paste("class", templateNames),
                      ...)
}

cppEigenTensorRef <- function(name = character(),
                              nDim,
                              scalarType) {
  ans <- cppEigenTensor(name, nDim, scalarType)
  ans$ref <- TRUE
  ans
}

cppADinfo <- function(name = character(),
                      ...) {
  cppVarFullClass$new(name = name,
                      baseType = 'nCompilerCppADinfoClass',
                      ...
                      )
}

cppADFun <- function(name = character(), 
                     ...) {
  cppVarFullClass$new(name = name,
                      baseType = 'CppAD::ADFun',
                      templateArgs = list('double'),
                      ...
                      )
}

cppSharedPtrToNC <- function(name = character(),
                             NCtype) {
  cppVarFullClass$new(name = name,
                      baseType = "std::shared_ptr",
                      templateArgs = list(NCtype))
}

cppVectorOfADFunPtr <- function(name = character(),
                                ...) {
  cppVarFullClass$new(name = name,
                      baseType = 'std::vector',
                      templateArgs = list(cppADFun(ptr = 1)),
                      ...)
}

CppADdouble <- function(name = character()) {
  cppVarFullClass$new(name = name,
                      baseType = 'CppAD::AD',
                      templateArgs = 'double')
}

cppVectorOfCppADdouble <- function(name = character()) {
  cppVarFullClass$new(name = name,
                      baseType = 'std::vector',
                      templateArgs = list( CppADdouble() ),
                      ref = FALSE)
}

cppEigenTensor <- function(name = character(),
                           nDim,
                           scalarType) {
  cppVarFullClass$new(name = name,
                      baseType = "Eigen::Tensor",
                      templateArgs = list(scalarType, nDim)
                      )
}

cppRcppType <- function(name = character(0), baseType, ...)
  cppVarClass$new(name = name, baseType = baseType, ...)

# cppRcppList <- function(name = character(0), ...)
#   cppVarClass$new(name = name, baseType = 'Rcpp::List', ...)
# 
# cppRcppNumericVector <- function(name = character(0), ...)
#   cppVarClass$new(name = name, baseType = 'Rcpp::NumericVector', ...)
# 
# cppRcppNumericMatrix <- function(name = character(0), ...)
#   cppVarClass$new(name = name, baseType = 'Rcpp::NumericMatrix', ...)
# 
# cppRcppDataFrame <- function(name = character(0), ...)
#   cppVarClass$new(name = name, baseType = 'Rcpp::DataFrame', ...)

# RcppEigen types
# cppRcppEigenMatrixXd <- function(name = character(0), ...)
#   cppVarClass$new(name = name, baseType = 'Eigen::MatrixXd', ...)



emptyTypeInfo <- function()
  cppVarClass$new(baseType = character()) ## for return type of constructors and destructors

cppBlank <- function() {
  cppVarClass$new(name = "", baseType = "")
}
cppDouble <- function(name = character(0), ...)
  cppVarClass$new(name = name, baseType = 'double', ...)
cppInt <- function(name = character(0), ...)
  cppVarClass$new(name = name, baseType = 'int', ...)
cppBool <- function(name = character(0), ...)
  cppVarClass$new(name = name, baseType = 'bool', ...)
cppVoid <- function(name = character(0), ...)
  cppVarClass$new(name = name, baseType = 'void', ...)

cppSEXP <- function(name = character(0),
                    ...)
  cppVarClass$new(name = name,
                  baseType = 'SEXP',
                  ptr = 0,
                  ...)
