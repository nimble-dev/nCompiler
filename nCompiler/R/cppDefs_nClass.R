## cpp_nClassBaseClass defines commonalities for potential future use.
## Currently, it is only inherited by cpp_nClassClass, below.

nClassBaseClass_init_impl <- function(cppDef) {
  usingEigen <- TRUE
  pluginIncludes <- if(usingEigen) {
                      nCompiler_Eigen_plugin()$includes
                    } else {
                      nCompiler_plugin()$includes
                    }
  cppDef$Hpreamble <- pluginIncludes
  cppDef$Hpreamble <- c(cppDef$Hpreamble,
                        "#define NCOMPILER_USES_EIGEN",
                        "#define NCOMPILER_USES_TBB",
                        "#define NCOMPILER_USES_NLIST",
                        "#define USES_NCOMPILER")
  cppDef$CPPpreamble <- pluginIncludes
  cppDef$CPPpreamble <- c(cppDef$CPPpreamble,
                        "#define NCOMPILER_USES_EIGEN",
                        "#define NCOMPILER_USES_TBB",
                        "#define NCOMPILER_USES_NLIST",
                        "#define USES_NCOMPILER")

  cppDef$Hincludes <- c(cppDef$Hincludes,
                      "<Rinternals.h>")#,
#                      nCompilerIncludeFile("nCompiler_Eigen.h"),
#                      nCompilerIncludeFile("nCompiler_TBB.h"))
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
    cppDef$CPPusings <- c(cppDef$CPPusings,
                        ##paste0("#include ", nCompilerIncludeFile("nCompiler_Eigen_fxns.h")),
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
      #self$Hpreamble <- NULL # Evidently we need to just touch self or it doesn't exist yet for the next call
      force(self)
      nClassBaseClass_init_impl(self)
      super$initialize(...) ## must call this first because it sets objectDefs to list()
      if(!missing(Compiler))
        process_Compiler(Compiler,
                         debugCpp = debugCpp,
                         fromModel = fromModel)
      if(length(name)==0)
        name <<- Compiler$name
      if(length(self$compileInfo$exportName)==0)
        self$compileInfo$exportName <- self$name
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
#      variableNamesForInterface <<- symbolTable$getSymbolNames()
    },
    buildAll = function(where = where) {
      buildDefaultSEXPgenerator <- !isFALSE(self$compileInfo$createFromR)
      ##buildDefaultSEXPgenerator <- buildDefaultSEXPgenerator &&
      ##  (!identical(self$compileInfo$interface, "none"))
      if(buildDefaultSEXPgenerator) {
        buildSEXPgenerator()
      }
      if(!identical(self$compileInfo$interface, "none"))
        build_set_nClass_env() # We want this even if not generating objects by call from R
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
#    ctor_names = character(),
#    dtor_names = character(),
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
      process_inheritance(Compiler)
      if(!missing(Compiler))
        process_NC_Compiler(Compiler,
                            debugCpp = debugCpp,
                            fromModel = fromModel)
      #
      ctor_dtor_status <- memberCppDefs |>
        lapply(\(x) {
          if(isTRUE(x$compileInfo$destructor))
            1 else if(isTRUE(x$compileInfo$constructor))
                2 else 0}) |> unlist() # 0 = neither, 1 = destructor, 2 = constructor
      if(sum(ctor_dtor_status == 1) > 1)
        warning("Multiple destructors found in an nClass. This may fail C++ compilation.")
      dtor_name <- names(memberCppDefs)[ctor_dtor_status == 1]
      if(length(dtor_name)) {
        self$memberCppDefs[[dtor_name]]$name <- paste0("~", self$name)
        self$memberCppDefs[[dtor_name]]$returnType <- cppBlank()
      }
      ctor_names <- names(memberCppDefs)[ctor_dtor_status == 2]
      for(cn in ctor_names) {
        self$memberCppDefs[[cn]]$name <- self$name
        self$memberCppDefs[[cn]]$returnType <- cppBlank()
      }
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
    process_inheritance = function(Compiler) {
      for(oneInheritance in Compiler$compileInfo$inherit) {
        self$addInheritance(oneInheritance)
      }
      inheritNCinternals <- NCinternals(self$Compiler$NCgenerator)$inheritNCinternals
      if(!is.null(inheritNCinternals)) {
        include_filebase <- make_cpp_filebase(inheritNCinternals$cpp_classname)
        self$Hincludes <- c(self$Hincludes, paste0("\"",include_filebase, ".h", "\""))
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
      message("To-do: Care needed to filter interfaced methods by exportMembers names.")
      for(i in seq_along(Compiler$NFcompilers)) {
        RCname <- names(Compiler$NFcompilers)[i]
        thisNFcomp <- Compiler$NFcompilers[[RCname]]
        memberCppDefs[[RCname]] <<- cpp_nFunctionClass$new(classMethod = TRUE,
                                                           compileInfo = thisNFcomp$compileInfo)
        memberCppDefs[[RCname]]$buildFunction(thisNFcomp)
        ## if(Compiler$NFcompilers[[RCname]]$NFinternals$compileInfo$callFromR)
        ##   self$functionNamesForInterface <<- c(self$functionNamesForInterface, RCname)
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
    addADclassContent = function() {
      addADclassContent_impl(self)
    },
    buildAll = function(interfaceCalls = TRUE, where = where) {
      super$buildAll(where)
      buildDefaultConstructor()
      if(isTRUE(get_nOption('serialize')))
        addSerialization()
      if(isTRUE(get_nOption('enableDerivs')))
        addADclassContent()
      #interfaceCalls controls whether to include get_values, set_value, call_method
      #self$compileInfo$interface controls whether to inherit from base classes for interfacing
      #It would be wierd to do the former without the latter,
      # so unless/until we get a case where that behavior is needed
      # we will prevent it.
      interface_needed <- !identical(self$compileInfo$interface, "none")
      interfaceCalls <- interfaceCalls && interface_needed
      addGenericInterface(interfaceCalls = interfaceCalls,
                          interface = interface_needed)
    }
  )
)

addADclassContent_impl <- function(cppDef) {
  for(i in seq_along(cppDef$Compiler$NFcompilers)) {
    derivsContent <- cppDef$Compiler$NFcompilers[[i]]$auxEnv$derivsContent
    ADtapeMgrSymbols <- derivsContent$ADtapeMgrSymbols
    if(!is.null(ADtapeMgrSymbols)) {
      for(iSym in seq_along(ADtapeMgrSymbols))
        cppDef$symbolTable$addSymbol(ADtapeMgrSymbols[[iSym]]$clone(deep=TRUE))
    }
    ADconstructorInits <- derivsContent$ADconstructorInits
    if(!is.null(ADconstructorInits)) {
      constructorDef <- cppDef$memberCppDefs[[ cppDef$name ]]
      if(is.null(constructorDef))
        warning("Could not find class constructor for ", cppDef$name, " when setting up AD tape managers.")
      constructorDef$initializerList <- c(constructorDef$initializerList,
                                          ADconstructorInits)
    }
  }
}
