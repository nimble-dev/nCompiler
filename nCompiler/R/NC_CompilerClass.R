nClassIDMaker <- labelFunctionCreator('NCID')
## These classes handle compilation of a nClass from its generator

NCvirtual_CompilerClass <- R6::R6Class(
  classname = "NCvirtual_CompilerClass",
  portable = FALSE,
  public = list(
    NCgenerator = NULL
  , NFcompilers = list() ## list of NF_CompilerClass objects for methods
  , symbolTable = NULL
  , cppDef = NULL
  , name = character()
  , compileInfo = list(),
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
        if(isConstructor(thisMethod)) {
          NFinternals(thisMethod)$cpp_code_name <- self$name
        }
        NFcompilers[[m]] <<- NF_CompilerClass$new(f = thisMethod)
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
    }
  )
)

NC_CompilerClass <- R6::R6Class(
  classname = "NC_CompilerClass",
  inherit = NCvirtual_CompilerClass,
  portable = FALSE,
  public = list(
    neededTypes = list(),
    initialize = function(NC = NULL,
                          className = NULL,
                          compileInfo = NULL) {
      super$initialize(NC = NC,
                       className = className,
                       compileInfo = compileInfo)
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
