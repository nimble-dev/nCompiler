cpp_nFunctionClass_init_impl <- function(cppDef) {
  usingEigen <- TRUE
  pluginIncludes <- if(usingEigen) {
                      nCompiler_Eigen_plugin()$includes
                    } else {
                      nCompiler_plugin()$includes
                    }
  cppDef$Hpreamble <- pluginIncludes
  cppDef$Hpreamble <- c(cppDef$Hpreamble,
                        "#define NCOMPILER_USES_EIGEN")
  cppDef$CPPpreamble <- pluginIncludes
  cppDef$CPPpreamble <- c(cppDef$CPPpreamble,
                        "#define NCOMPILER_USES_EIGEN")
  cppDef$Hincludes <- c(cppDef$Hincludes,
                        nCompilerIncludeFile("nCompiler_omnibus_first_h.h"))
  ## cppDef$CPPincludes <- c(cppDef$CPPincludes,
  ##                       nCompilerIncludeFile("nCompiler_omnibus_first_cpp.h"))
  ## cppDef$Hincludes <- c(cppDef$Hincludes,
  ##                     "<Rinternals.h>",
  ##                     nCompilerIncludeFile("nCompiler_Eigen.h"),
  ##                     nCompilerIncludeFile("nCompiler_TBB.h")
  ##                     )
  ## cppDef$CPPincludes <- c(cppDef$CPPincludes,
  ##                       nCompilerIncludeFile("nCompiler_Eigen.h"),
  ##                       nCompilerIncludeFile("nCompiler_TBB.h"),
  ##                       '<Rmath.h>',
  ##                       '<math.h>'
  ##                       )
  usingEigen <- TRUE
  if(usingEigen) {
    checkPackage <- find.package(c("RcppEigenAD", "Rcereal"),
                                 quiet = TRUE)
    if(length(checkPackage)!=2) {
      stop("Packages RcppEigenAD and Rcereal must be installed.")
    }
    ## require(RcppEigenAD)
    ## require(Rcereal)
                                        # We can put includes here that need to occur after the file's own .h is included.
    cppDef$CPPusings <- c(cppDef$CPPusings,
                     #   paste0("#include ", nCompilerIncludeFile("nCompiler_Eigen_fxns.h")),
                        "using namespace Rcpp;",
                        "// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]",
                        "// [[Rcpp::depends(RcppEigenAD)]]",
                        "// [[Rcpp::depends(RcppParallel)]]",
                        "// [[Rcpp::depends(nCompiler)]]",
                        "// [[Rcpp::depends(Rcereal)]]")
  } else {
    cppDef$CPPusings <- c(cppDef$CPPusings,
                        "using namespace Rcpp;",
                        "// [[Rcpp::plugins(nCompiler_plugin)]]",
                        "// [[Rcpp::depends(nCompiler)]]")
  }
  NULL
}

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
      cpp_nFunctionClass_init_impl(self)
      ## nCompiler_plugin()$includes already have #include, so they go here
      super$initialize(...)
    },
    getInternalDefs = function() {
      super$getInternalDefs()
    },
    getExternalDefs = function() {
      super$getExternalDefs()
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
      cpp_include_needed_nFunctions(self, NF_Compiler)
      cpp_include_needed_nClasses(self, NF_Compiler$symbolTable, NF_Compiler)
      cpp_include_aux_content(self, NF_Compiler)
    },
    get_post_cpp_compiler = function() {
      structure(
        list(list(refArgs = self$NF_Compiler$NFinternals$refArgs,
                  blockRefArgs = self$NF_Compiler$NFinternals$blockRefArgs)),
        names = self$NF_Compiler$NFinternals$cpp_code_name)
    }
  )
)

cpp_include_aux_content <- function(self,
                                    NF_Compiler) {
  ## Available aux content:
  ## initializerList for a constructor
  if(is.null(NF_Compiler$NFinternals$aux)) return(invisible(NULL))
  if(!is.null(NF_Compiler$NFinternals$aux$initializerList_exprClasses)) {
    self$initializerList <- 
      NF_Compiler$NFinternals$aux$initializerList_exprClasses
  }
}

cpp_include_needed_nFunctions <- function(cppDef,
                                          NF_Compiler) {
  needed_cpp_code_names <- lapply(NF_Compiler$auxEnv$needed_nFunctions,
                                  function(x)
                                    NFinternals(nGet(x[[1]], where = x[[2]]))$cpp_code_name)
  if(length(needed_cpp_code_names)) {
    needed_filebases <- make_cpp_filebase(unlist(needed_cpp_code_names))
    cppDef$CPPincludes <- c(cppDef$CPPincludes, paste0('\"',needed_filebases, '.h\"'))
  }
  invisible(NULL)
}

cpp_include_needed_nClasses <- function(cppDef,
                                        symTab,
                                        NF_Compiler = NULL) {
  new_Hincludes <- character()
  for(i in seq_along(symTab$symbols)) {
    if(inherits(symTab$symbols[[i]], "symbolNC")) {
      needed_nClass_cppname <- symTab$symbols[[i]]$NCgenerator$classname
      new_Hincludes <- c(new_Hincludes,
                         paste0('\"',
                                make_cpp_filebase(needed_nClass_cppname),
                                '.h\"'))
    }
  }
  #  cppDef$CPPincludes <- c(cppDef$CPPincludes, paste0('\"', needed_nClass_cppname, '.h\"'))
  if(!is.null(NF_Compiler)) {
    auxEnv_needed_nClasses <- NF_Compiler$auxEnv$needed_nClasses
    for(i in seq_along(auxEnv_needed_nClasses)) {
      if(isNCgenerator(auxEnv_needed_nClasses[[i]])) {
        needed_nClass_cppname <- auxEnv_needed_nClasses[[i]]$classname
        new_Hincludes <- c(new_Hincludes,
                           paste0('\"',
                                  make_cpp_filebase(needed_nClass_cppname),
                                  '.h\"'))
      }
    }
  }
  new_Hincludes <- unique(new_Hincludes)  
  cppDef$Hincludes <- c(cppDef$Hincludes, new_Hincludes)
  invisible(NULL)
}

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
  if(!cppDef$classMethod && !NF_Compiler$isAD)
    cppDef$commentsAbove <- paste0('// [[Rcpp::export]]')
  if(NF_Compiler$isAD)
    cppDef$CPPincludes <- c(
      cppDef$CPPincludes,
      nCompilerIncludeFile("nCompiler_CppAD.h"))
  ## For external calls:
  ## cppDef$CPPincludes <-
  ##     c(cppDef$CPPincludes,
  ##       RCfunProc$RCfun$externalCPPincludes)
  ## cppDef$Hincludes <-
  ##     c(cppDef$Hincludes,
  ##       RCfunProc$RCfun$externalHincludes)
  invisible(NULL)
}
