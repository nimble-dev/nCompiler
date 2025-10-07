nClassIDMaker <- labelFunctionCreator('NCID')
## These classes handle compilation of a nClass from its generator

NC_CompilerClass <- R6::R6Class(
  classname = "NC_CompilerClass",
  portable = FALSE,
  public = list(
    NCgenerator = NULL
  , NFcompilers = list() ## list of NF_CompilerClass objects for methods
  , symbolTable = NULL
  , cppDef = NULL
  , name = character()
  , compileInfo = list()
  , neededTypes = list(),
    initialize = function(NC = NULL,
                          className = NULL,
                          compileInfo = NULL) {
      if(!isNCgenerator(NC)) {
        if(isNC(NC))
          stop(paste0("nClass object was provided to NCvirtual_CompilerClass. ",
                      "It requires a nClass generator\n"),
               call. = FALSE)
        stop(paste0("NC argument to NCvirtual_CompilerClass must be a ",
                    "nClass generator\n"),
             call. = FALSE)
      }
      NCgenerator <<- NC
      myNCinternals <- NCinternals(NCgenerator)
      if(is.null(className)) {
        name <<- myNCinternals$cpp_classname
      } else {
        name <<- className
      }
      if(is.null(compileInfo))
        self$compileInfo <- myNCinternals$compileInfo
      else
        self$compileInfo <- compileInfo
      if(length(compileInfo$exportName)==0) {
        self$compileInfo$exportName <- paste0("new_", self$name)
      }
      # name <<- name  #???
      ## In the past we've a system that makes every name unique by
      ## pasting on a unique ID.  We need that to not happen for
      ## predefined classes.  For now I am turning off the behavior
      ## altogether.  If we determine that we need it again for
      ## non-predefined classes, we can toggle it as needed.
      # name <<- paste(name,
      #                nClassIDMaker(),
      #                sep = "_")

      methodNames <- myNCinternals$methodNames
      for(m in methodNames) {
        thisMethod <- NCgenerator$public_methods[[m]]
        thisName <- NULL
        if(isConstructor(thisMethod)) {
          #NFinternals(thisMethod)$cpp_code_name <- self$name
          NFinternals(thisMethod)$cpp_code_name <- self$name
        } else {
          thisName <- myNCinternals$all_methodName_to_cpp_code_name[[m]]
        }
        NFcompilers[[m]] <<- NF_CompilerClass$new(f = thisMethod,
                                                  name = thisName)
      }
    },
    setupMethodSymbolTables = function() {
      for(i in seq_along(NFcompilers)) {
        NFcompilers[[i]]$setupSymbolTable(parentSymbolTable = symbolTable)
      }
    },
    createCppMethods = function(control,
                                sourceObj) {
      for(i in seq_along(NFcompilers)) {
        NFcompilers[[i]]$createCpp(sourceObj = sourceObj)
      }
    },
    createCpp = function(control = list(),
                         sourceObj, #this will be the same as NC, so seems redundant and should be considered for removal/cleanup
                         interfaceCalls = TRUE) {
      controlFull <- updateDefaults(
        get_nOption('compilerOptions'),
        control
      )
      process(control = controlFull,
              sourceObj)
      cppDef <<- cpp_nClassClass$new(
        Compiler = self,
        name = self$name,
        compileInfo = self$compileInfo
      )
      # note interfaceCalls controls including set_value, get_value, call_method
      # that is distinct from inheriting from base classes for interfacing,
      # managed by compileInfo$interface == "none"
      cppDef$buildAll(interfaceCalls = interfaceCalls)
      invisible(NULL)
    },
    process = function(control = list(),
                       sourceObj) {
      controlFull <- updateDefaults(
        get_nOption('compilerOptions'),
        control
      )
      if(is.null(symbolTable)) {
        makeSymbolTables()
      }
      createCppMethods(control = controlFull,
                       sourceObj)
      ##collectNeededTypes()
      invisible(NULL)
    },
    makeSymbolTables = function() {
      if(is.null(symbolTable)) {
        symbolTable <<- NCinternals(NCgenerator)$symbolTable$clone(deep = TRUE)
        ## Update any symbolTBD symbols by scoped lookup
        resolveTBDsymbols(symbolTable,
                          NCgenerator)
        setupMethodSymbolTables()
      }
    }
  )
)
