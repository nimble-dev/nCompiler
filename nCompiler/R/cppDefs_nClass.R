## cpp_nClassBaseClass defines commonalities for potential future use.
## Currently, it is only inherited by cpp_nClassClass, below.
cpp_nClassBaseClass <- R6::R6Class(
  'cpp_nClassBaseClass',
  inherit = cppClassClass,##'cppNamedObjectsClass',
  portable = FALSE,
  public = list(
    ## Inherits a functionDefs list for member functions
    ## Inherits an objectDefs list for member data
    ## SEXPmemberInterfaceFuns = 'ANY', ## List of SEXP interface functions, one for each member function
    Compiler = NULL,
    ##nimCompProc = 'ANY', ## nfProcessing or nlProcessing object, needed to get the member data symbol table post-compilation
    
    ##Rgenerator = 'ANY' , ## function to generate and wrap a new object from an R object
    ##CmultiInterface = 'ANY', ## object for interfacing multiple C instances when a top-level interface is not needed
    built = NULL,
    loaded = NULL,
    Cwritten = NULL,
    getInternalDefs = function() {
      super$getInternalDefs()
    },
    getExternalDefs = function() {
      super$getExternalDefs()
    },
    getHincludes = function() {
      super$getHincludes()
    },
    getCPPincludes = function() {
      super$getCPPincludes()
    },
    getCPPusings = function() {
      super$getCPPusings()
    },
    initialize = function(Compiler,
                          debugCpp = FALSE,
                          fromModel = FALSE, ...) {
      usingEigen <- TRUE
      pluginIncludes <- if(usingEigen) {
        nCompiler_Eigen_plugin()$includes
      } else {
        nCompiler_plugin()$includes
      }
      self$Hpreamble <- pluginIncludes
      self$CPPpreamble <- pluginIncludes
      
      self$Hincludes <- c(self$Hincludes,
                          "<Rinternals.h>",
                          nCompilerIncludeFile("nCompiler_Eigen.h"),
                          nCompilerIncludeFile("nCompiler_TBB.h"))
      CPPincludes <<- list()
      usingEigen <- TRUE
      ## The following need to be here, not just in cpp_nFunction, in case there is a nClass with no methods.
      if(usingEigen) {
        checkPackage <- find.package(c("RcppEigenAD", "Rcereal"),
                                     quiet = TRUE)
        if(length(checkPackage)!=2) {
          stop("Packages RcppEigenAD and Rcereal must be installed.")
        }
        ##                require(RcppEigenAD)
        ##                require(Rcereal)
        self$CPPusings <- c(self$CPPusings,
                            paste0("#include ", nCompilerIncludeFile("nCompiler_Eigen_fxns.h")),
                            "using namespace Rcpp;",
                            "// [[Rcpp::plugins(nCompiler_Eigen_plugin)]]",
                            "// [[Rcpp::depends(RcppEigenAD)]]",
                            "// [[Rcpp::depends(RcppParallel)]]",
                            "// [[Rcpp::depends(nCompiler)]]",
                            "// [[Rcpp::depends(Rcereal)]]")
      } else {
        self$CPPusings <- c(self$CPPusings,
                            "using namespace Rcpp;",
                            "// [[Rcpp::plugins(nCompiler_plugin)]]",
                            "// [[Rcpp::depends(nCompiler)]]")
      }
      
      super$initialize(...) ## must call this first because it sets objectDefs to list()
      if(!missing(Compiler))
        process_Compiler(Compiler,
                         debugCpp = debugCpp,
                         fromModel = fromModel)
      if(length(name)==0)
        name <<- Compiler$name
      built <<- FALSE
      loaded <<- FALSE
      Cwritten <<- FALSE
    },
    process_Compiler = function(InputCompiler,
                                debugCpp = FALSE,
                                fromModel = FALSE) {
      ##ncp$cppDef <- .self
      Compiler <<- InputCompiler
        ##genNeededTypes(debugCpp = debugCpp, fromModel = fromModel)
      cpp_include_needed_nClasses(self, Compiler$symbolTable)
      symbolTable <<- symbolTable2cppSymbolTable(Compiler$symbolTable)
      variableNamesForInterface <<- symbolTable$getSymbolNames()
    },
    buildAll = function(where = where) {
      buildSEXPgenerator()
      build_set_nClass_env()
    }
    ## Following could turn out to be useful but was carried over from nimble and perhaps not needed.
    ## , makeCppNames = function() {
    ##   Rnames2CppNames <<- as.list(Rname2CppName(symbolTable$getSymbolNames()))
    ##   names(Rnames2CppNames) <<- symbolTable$getSymbolNames()
    ## }
  )
)

cpp_nClassClass <- R6::R6Class(
  'cpp_nClassClass',
  inherit = cpp_nClassBaseClass,
  portable = FALSE,
  public = list(
    ##NC_Compiler = NULL, 
    ##parentsSizeAndDims = 'ANY',
    getInternalDefs = function() {
      super$getInternalDefs()
    },
    getExternalDefs = function() {
      super$getExternalDefs()
    },
    getHincludes = function() {
      super$getHincludes()
    },
    getCPPincludes = function() {
      super$getCPPincludes()
    },
    getCPPusings = function() {
      unique(super$getCPPusings())
    },
    initialize = function(Compiler,
                          isNode = FALSE,
                          debugCpp = FALSE,
                          fromModel = FALSE,
                          ...) {
      super$initialize(Compiler,
                       debugCpp,
                       fromModel,
                       ...)
      if(!missing(Compiler))
        process_NC_Compiler(Compiler,
                            debugCpp = debugCpp,
                            fromModel = fromModel)
      # isNode clause has not been updated from nimble.
      # It is a placeholder / reminder for later.
      if(isNode) {
        inheritance <<- inheritance[inheritance != 'NamedObjects']
        baseClassObj <- environment(nfProc$nfGenerator)$contains
        if(is.null(baseClassObj)) {
          inheritance <<- c(inheritance, 'nodeFun')
          parentsSizeAndDims <<- environment(nfProc$nfGenerator)$parentsSizeAndDims
        }
      }
    },
    process_NC_Compiler = function(Compiler, debugCpp = FALSE, fromModel = FALSE) {
      buildFunctionDefs()
      for(i in seq_along(memberCppDefs)) {
        memberCppDefs[[i]]$args$setParentST(symbolTable)
      }
      buildParallelClassDefs()
    },
    buildFunctionDefs = function() {
      for(i in seq_along(Compiler$NFcompilers)) {
        RCname <- names(Compiler$NFcompilers)[i]
        memberCppDefs[[RCname]] <<- cpp_nFunctionClass$new(classMethod = TRUE)
        memberCppDefs[[RCname]]$buildFunction(Compiler$NFcompilers[[RCname]])
        self$functionNamesForInterface <<- c(self$functionNamesForInterface, RCname)
      }
    },
    buildParallelClassDefs = function() {
      for(i in seq_along(Compiler$NFcompilers)) {
        parallelContent <- Compiler$NFcompilers[[i]]$auxEnv$parallelContent
        if(!is.null(parallelContent)) {
          for(j in seq_along(parallelContent)) {
            cppDef_TBB <- cppParallelBodyClass$new(loop_body = parallelContent[[j]]$args[[3]],
                                                   loop_var = parallelContent[[j]]$args[[1]],
                                                   symbolTable = memberCppDefs[[i]]$code$symbolTable,
                                                   copyVars = parallelContent[[j]]$args[[4]],
                                                   noncopyVars = parallelContent[[j]]$args[[5]])
            ## The name is hard-wired expecting only a single case of parallel content.
            ## TO-DO: generalize the name with unique identifier.
            self$memberCppDefs[["parallel_loop_body"]] <<- cppDef_TBB
          }
        }
        parallelReduceContent <- Compiler$NFcompilers[[i]]$auxEnv$parallelReduceContent
        ## TODO: if there are multiple nFunctions in a class, they share the
        ## auxEnv and as a result we'll find the parallelReduceContent even for
        ## the wrong NFcompiler (and thus pass the wrong symbolTable to
        ## cppParallelReduceBodyClass$new
        if(!is.null(parallelReduceContent)) {
          for(j in seq_along(parallelReduceContent)) {
            cppDef_TBB <- cppParallelReduceBodyClass$new(
              loop_body = parallelReduceContent[[j]]$args[[3]],
              loop_var = parallelReduceContent[[j]]$args[[1]],
              symbolTable = memberCppDefs[[i]]$code$symbolTable,
              copyVars = list(),
              noncopyVars = list(parallelReduceContent[[j]]$args[[4]],
                                 parallelReduceContent[[j]]$args[[5]])
            )
            ## The name is hard-wired expecting only a single case of parallel content.
            ## TO-DO: generalize the name with unique identifier.
            self$memberCppDefs[["parallel_reduce_body"]] <<- cppDef_TBB
          }
        }
      }
    },
    addTypeTemplateFunction = function( funName ) {
      newFunName <- paste0(funName, '_AD_')
      regularFun <- memberCppDefs[[funName]]
      memberCppDefs[[newFunName]] <<- makeTypeTemplateFunction(newFunName, regularFun)
      invisible(NULL)
    },
    addADtapingFunction = function( funName, 
                                    independentVarNames, 
                                    dependentVarNames ) {
      ADfunName <- paste0(funName, '_AD_')
      regularFun <- memberCppDefs[[funName]]
      newFunName <- paste0(funName, '_callForADtaping_')
      memberCppDefs[[newFunName]] <<- makeADtapingFunction(newFunName,
                                                             regularFun, 
                                                             ADfunName, 
                                                             independentVarNames, 
                                                             dependentVarNames, 
                                                             isNode = FALSE,
                                                             memberCppDefs)
      invisible(NULL)
    },
    addADmethodMacros = function(funName, args) {
      ## fun will be named foo_derivs_.
      newName <- paste0(funName, "_derivs_")
      memberCppDefs[[newName]] <<- cppADmethodMacroClass$new(name = newName,
                                                               base_name = funName,
                                                               args = args)
      self$functionNamesForInterface <<- c(self$functionNamesForInterface, newName)
      invisible(NULL)
    },
    addADargumentTransferFunction = function( funName, independentVarNames ) {
      newFunName <- paste0(funName, '_ADargumentTransfer_')
      regularFun <- memberCppDefs[[funName]]
      funIndex <- which(NCinternals(self$Compiler$NCgenerator)$enableDerivs == funName) ## needed for correct index for allADtapePtrs_
      memberCppDefs[[newFunName]] <<- makeADargumentTransferFunction(newFunName,
                                                                       regularFun, 
                                                                       independentVarNames, 
                                                                       funIndex 
                                                                       #, parentsSizeAndDims #was relevant to nodeFuns
      )
    },
    addStaticInitClass = function() {
      internalCppDefs[['staticInitClass']] <<- makeStaticInitClass(self,
                                                                 NCinternals(self$Compiler$NCgenerator)$enableDerivs) ##
      invisible(NULL)
    },
    addADclassContentOneFun = function(funName) {
      outSym <- self$Compiler$NFcompilers[[funName]]$returnSymbol
      checkADargument(funName, outSym, returnType = TRUE)
      if(length(self$Compiler$NFcompilers[[funName]]$nameSubList) == 0)
        stop(paste0('Derivatives cannot be enabled for method ', 
                    funName, 
                    ', since this method has no arguments.'))
      ## Not updated:
      if(FALSE) {
        if(!nfProc$isNode){
          for(iArg in seq_along(functionDefs[[funName]]$args$symbols)){
            arg <- functionDefs[[funName]]$args$symbols[[iArg]]
            argSym <- nfProc$RCfunProcs[[funName]]$compileInfo$origLocalSymTab$getSymbolObject(arg$name)
            argName <- names(nfProc$RCfunProcs[[funName]]$nameSubList)[iArg]
            checkADargument(funName, argSym, argName = argName)
          }
        }
      }
      addTypeTemplateFunction(funName)
      independentVarNames <- self$memberCppDefs[[funName]]$args$getSymbolNames() ## Is this the right layer?
      if(FALSE)
        if(nfProc$isNode) independentVarNames <- independentVarNames[-1]  ## remove ARG1_INDEXEDNODEINFO__ from independentVars
      
      addADtapingFunction(funName,
                          independentVarNames = independentVarNames,
                          dependentVarNames = 'ANS_' )
      addADargumentTransferFunction(funName,
                                    independentVarNames = independentVarNames)
      addADmethodMacros(funName,
                        self$memberCppDefs[[funName]]$args)
    },
    checkADargument = function(funName, 
                               argSym, 
                               argName = NULL,
                               returnType = FALSE){
      argTypeText <- if(returnType) 
        'returnType'
      else
        'argument'
      if(argSym$type != 'double')
        stop(paste0('The ', argName, ' ', argTypeText, ' of the ', funName, ' method is not a double.  Therefore this method cannot have derivatives enabled.'))
      if(!(argSym$nDim %in% c(0,1)))
        stop(paste0('The ', argName, ' ', argTypeText, ' of the ', funName, ' method must be a double scalar or double vector for derivatives to be enabled.'))
      if((argSym$nDim == 1) && is.na(argSym$size)) stop(paste0('To enable derivatives, size must be given for the ', argName, ' ', argTypeText, ' of the ', funName,
                                                               ' method,  e.g. double(1, 3) for a length 3 vector.' ))
    },
    
    addADclassContent = function() {
      self$Hincludes <- c(
        nCompilerIncludeFile("nCompiler_CppAD.h"), Hincludes)
      self$symbolTable$addSymbol(cppVectorOfADFunPtr(name = 'allADtapePtrs_', static = TRUE))
      self$symbolTable$addSymbol(cppADinfo(name = 'ADtapeSetup'))
      for(adEnabledFun in NCinternals(self$Compiler$NCgenerator)$enableDerivs){
        addADclassContentOneFun(adEnabledFun)
      }
      ## static declaration in the class definition
      ## globals to hold the global static definition
      globals <- cppGlobalObjectClass$new(name = paste0('staticGlobals_', name),
                                          staticMembers = TRUE)
      globals$symbolTable$addSymbol(cppVectorOfADFunPtr(name = paste0(name,'::allADtapePtrs_')))
      internalCppDefs[['allADtapePtrs_']] <<- globals
      addStaticInitClass()
      invisible(NULL)
    },
    buildAll = function(interfaceCalls = TRUE, where = where) {
      super$buildAll(where)
      if(isTRUE(get_nOption('serialize')))
        addSerialization()
      if(isTRUE(get_nOption('automaticDerivatives')))
        addADclassContent()
      addGenericInterface(interfaceCalls = interfaceCalls)
    }
  )
)
