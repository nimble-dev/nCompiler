
NC_InternalsClass <- R6::R6Class(
  classname = "NC_InternalsClass",
  portable = FALSE,
  public = list(
    symbolTable = NULL,
    cppSymbolNames = NULL,
    methodNames = character(),
    allMethodNames = character(), # including inherited methods
    allMethodNames_self = character(), # not including inherited methods
    fieldNames = character(),
    allFieldNames = character(), # including inherited methods
    allFieldNames_self = character(), # not including inherited methods
    classname = character(),
    cpp_classname = character(),
    compileInfo = list(),
    inherit_base_provided = FALSE,
    # compileInfo will include interface ("full", "generic", or "none"),
    # interfaceMembers, exportName, and depends
    depends = list(),
    RcppPacket = NULL,
    isOnlyC = FALSE, ## somewhat redundant but perhaps convenient - TBD.
    enableDerivs = NULL,
    enableSaving = NULL,
    predefined = FALSE,
    inheritNCinternals = NULL,
    env = NULL,
    inheritQ = NULL,
    process_inherit_done = FALSE,
    initialize = function(classname,
                          Cpublic,
                          isOnlyC = FALSE,
                          enableDerivs = NULL,
                          enableSaving = get_nOption("enableSaving"),
                          inheritQ = NULL,
                          compileInfo = list(),
                          predefined = FALSE,
                          env = parent.frame()) {
      self$env <- env
      self$inheritQ <- inheritQ
      self$compileInfo <- compileInfo
      self$classname <- classname
      self$cpp_classname <- Rname2CppName(classname)
      self$isOnlyC = isOnlyC
      numEntries <- length(Cpublic)
      if(numEntries) {
        isMethod <- rep(FALSE, numEntries)
        for(i in seq_along(Cpublic)) {
          if(isNF(Cpublic[[i]])) {
            isMethod[i] <- TRUE
            NFinternals(Cpublic[[i]])$isMethod <- TRUE
            next;
          }
          if(is.function(Cpublic[[i]])) {
            stop(paste0('Cpublic methods should be provided as nFunctions, ',
                        'not functions. ', names(Cpublic)[i], ' is a function.'),
                 call. = FALSE)
          }
        }
        self$symbolTable <- argTypeList2symbolTable(Cpublic[!isMethod], evalEnv = env)
        self$cppSymbolNames <- Rname2CppName(symbolTable$getSymbolNames())
        self$methodNames <- names(Cpublic)[isMethod]
        self$allMethodNames_self <- methodNames
        self$allMethodNames <- methodNames
        self$fieldNames <- names(Cpublic)[!isMethod]
        self$allFieldNames_self <- fieldNames
        self$allFieldNames <- fieldNames
        if(!is.null(self$compileInfo$inherit$base))
          self$inherit_base_provided <- TRUE
      }
      if(!is.null(enableDerivs)) {
        if(!is.list(enableDerivs))
          enableDerivs <- as.list(enableDerivs)
        for(i in enableDerivs) {
          if(!(i %in% self$methodNames))
            stop(paste0('enableDerivs entry ', i, ' is not a method in Cpublic.'))
        }
        self$enableDerivs <- enableDerivs
      }
      self$predefined <- predefined
      self$enableSaving <- enableSaving
    },
    connect_inherit = function() {
      # These are steps that need to be done after all classes are defined
      # and do not require recursion up the inheritance tree.
      if(!is.null(self$inheritQ)) {
        inherit_obj <- eval(self$inheritQ, envir = self$env)
        if(!isNCgenerator(inherit_obj))
          stop("An inherit argument that was provided to nClass is not nClass generator.")
        self$inheritNCinternals <- NCinternals(inherit_obj)
        message("add check that base class has interface 'none'")
        if(!self$inherit_base_provided)
          self$compileInfo$inherit$base <- paste("public",
                                            self$inheritNCinternals$cpp_classname)
        process_inherit_done <- FALSE
      } else {
        process_inherit_done <- TRUE
      }
    },
    process_inherit = function() {
      # These are steps that need to be done after connect_inherit
      # and require recursion up the inheritance tree, using flags.
      if(self$process_inherit_done) return()
      if(!is.null(self$inheritQ)) {
        self$inheritNCinternals$process_inherit()
        self$symbolTable$setParentST(self$inheritNCinternals$symbolTable)
        self$allMethodNames <- c(self$allMethodNames_self, self$inheritNCinternals$allMethodNames)
        self$allFieldNames <- c(self$allFieldNames_self, self$inheritNCinternals$allFieldNames)
      }
      self$process_inherit_done <- TRUE
    }
  )
)
