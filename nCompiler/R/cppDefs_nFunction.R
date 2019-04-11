cpp_nFunctionClass <- R6::R6Class(
    classname = 'cpp_nFunctionClass',
    inherit = cppFunctionClass,
    portable = FALSE,
    public = list(
        SEXPwrapper = NULL,
        SEXPwrapperCname = character(),
        NF_Compiler = NULL,
        initialize = function(...) {
            ## conflicting protocols:.nCompiler inserts #include later
            ## inline/Rcpp plugins do not, so we strip them out here
            ## so that they can be inserted later.
            usingEigen <- TRUE
            pluginIncludes <- if(usingEigen) {
                                nCompiler_Eigen_plugin()$includes
                             } else {
                                 nCompiler_plugin()$includes
                             }
            self$Hpreamble <- pluginIncludes
            self$CPPpreamble <- pluginIncludes
            self$Hincludes <- c(self$Hincludes,
                           #nCompilerIncludeFile("NimArr.h"),
                                "<Rinternals.h>",
                               nCompilerIncludeFile("nCompiler_Eigen.h")
                               ##,
                           #nCompilerIncludeFile("accessorClasses.h"),
                           #nCompilerIncludeFile("nimDists.h"),
                           #nCompilerIncludeFile("nimOptim.h")
                           )
            self$CPPincludes <- c(self$CPPincludes,
                                 nCompilerIncludeFile("nCompiler_Eigen.h"),
                                  '<Rmath.h>',
                                  '<math.h>',
                                  nCompilerIncludeFile("cWiseUnary_external.cpp"),
                                  #nCompilerIncludeFile("EigenTypedefs.h"),
                                  #nCompilerIncludeFile("Utils.h"),
                                  #nCompilerIncludeFile("accessorClasses.h"),
                                  if(isTRUE(nOptions('experimentalUseTensorflow')))
                                     nCompilerIncludeFile("tensorflow.h")
                                  else
                                      character()
                             )
            usingEigen <- TRUE
            if(usingEigen) {
                checkPackage <- find.package(c("RcppEigenAD", "Rcereal"),
                                             quiet = TRUE)
                if(length(checkPackage)!=2) {
                    stop("Packages RcppEigenAD and Rcereal must be installed.")
                }
                ## require(RcppEigenAD)
                ## require(Rcereal)
                self$CPPusings <- c(self$CPPusings,
                                    "using namespace Rcpp;",
                                    "// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]",
                                    "// [[Rcpp::depends(RcppEigenAD)]]",
                                    "// [[Rcpp::depends(nCompiler)]]",
                                    "// [[Rcpp::depends(Rcereal)]]")
            } else {
                self$CPPusings <- c(self$CPPusings,
                                    "using namespace Rcpp;",
                                    "// [[Rcpp::plugins(nCompiler_plugin)]]",
                                    "// [[Rcpp::depends(nCompiler)]]")
    
            }
            ## nCompiler_plugin()$includes already have #include, so they go here
            super$initialize(...)
        },
        getDefs = function() {
            super$getDefs()
            ##,self$SEXPwrapper) ## may be NULL
        },
        getHincludes = function() {
            c(self$Hincludes,
              if(!is.null(self$SEXPwrapper))
                  self$SEXPwrapper$getHincludes()
              )
        },
        getCPPincludes = function() {
            c(self$CPPincludes,
              unlist(
                  lapply(
                      self$CPPincludes,
                      function(x)
                          if(is.character(x))
                              NULL
                          else
                              x$getCPPincludes()
                  ),
                  recursive = FALSE
              ), 
              if(!is.null(self$SEXPwrapper))
                  self$SEXPwrapper$getCPPincludes()
              )
        },
        getCPPusings = function() {
            unique(c(self$CPPusings,
                     if(!is.null(self$SEXPwrapper))
                         self$SEXPwrapper$getCPPusings()
                     )
                   )
        },
        buildFunction = function(NF_Compiler,
                                 parentST = NULL) {
            cpp_nFunction_buildFunction(self,
                                            NF_Compiler,
                                            parentST)
        }
    )
)

cpp_nFunction_buildFunction <- function(cppDef,
                                            NF_Compiler,
                                            parentSymbolTable = NULL) {
    cppDef$NF_Compiler <- NF_Compiler
    name <- cppDef$name <- NF_Compiler$name
    const <- NF_Compiler$const
    symTab <- NF_Compiler$symbolTable
    argsCppSymTab <- ## arguments only
        symbolTable2cppSymbolTable(symTab,
                                   args = TRUE,
                                   parentSymbolTable = parentSymbolTable
                                   )
    cppDef$args <- argsCppSymTab
    templateTypes <- unlist(lapply(argsCppSymTab$symbols,
                                   function(x) {
                                       if(substr(x$baseType, 1, 2) == "TT")
                                           x$baseType
                                       else
                                           NULL
                                   })
                            )
    if(length(templateTypes) > 0)
        cppDef$template <- cppTemplateDeclaration(templateTypes)
                               
    localVarsCppSymTab <- ## local variables
        symbolTable2cppSymbolTable(symTab,
                                   args = FALSE,
                                   parentSymbolTable = argsCppSymTab)
    cppDef$code <- cppCodeBlockClass$new(code =  NF_Compiler$code,
                                         symbolTable = localVarsCppSymTab)
    if(is.null(NF_Compiler$returnSymbol))
        stop(paste("returnType not valid.  If a nCompilerList is being",
                   "returned, returnType must be the name of the nCompilerList",
                   "definition."),
             call. = FALSE
             )
    cppDef$returnType <-
        NF_Compiler$returnSymbol$genCppVar()
    if(!cppDef$classMethod)
        cppDef$commentsAbove <- paste0('// [[Rcpp::export]]')
    ## For external calls:
    ## cppDef$CPPincludes <-
    ##     c(cppDef$CPPincludes,
    ##       RCfunProc$RCfun$externalCPPincludes)
    ## cppDef$Hincludes <-
    ##     c(cppDef$Hincludes,
    ##       RCfunProc$RCfun$externalHincludes)
    invisible(NULL)
}

