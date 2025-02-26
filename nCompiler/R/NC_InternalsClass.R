
NC_InternalsClass <- R6::R6Class(
  classname = "NC_InternalsClass",
  portable = FALSE,
  public = list(
    symbolTable = NULL,
    cppSymbolNames = NULL,
    methodNames = character(),
    allMethodNames = character(), # including inherited methods
    fieldNames = character(),
    allFieldNames = character(), # including inherited methods
    classname = character(),
    cpp_classname = character(),
    compileInfo = list(),
    # compileInfo will include interface ("full", "generic", or "none"),
    # interfaceMembers, exportName, and depends
    depends = list(),
    RcppPacket = NULL,
    isOnlyC = FALSE, ## somewhat redundant but perhaps convenient - TBD.
    enableDerivs = NULL,
    enableSaving = NULL,
    predefined = FALSE,
    inheritNCinternals = NULL,
    initialize = function(classname,
                          Cpublic,
                          isOnlyC = FALSE,
                          enableDerivs = NULL,
                          enableSaving = get_nOption("enableSaving"),
                          inherit = NULL,
                          compileInfo = list(),
                          predefined = FALSE) {
      if(!is.null(inherit)) {
        self$inheritNCinternals <- NCinternals(inherit)
        message("add check that base class has interface 'none'")
        if(is.null(compileInfo$inherit$base))
          compileInfo$inherit$base <- paste("public",
                                            inheritNCinternals$cpp_classname)
        if(is.null(compileInfo$inherit$h_file))
          compileInfo$inherit$h_file   <- paste0(make_cpp_filename(inheritNCinternals$cpp_classname),'.h')
      }
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
        self$symbolTable <- argTypeList2symbolTable(Cpublic[!isMethod])
        self$cppSymbolNames <- Rname2CppName(symbolTable$getSymbolNames())
        self$methodNames <- names(Cpublic)[isMethod]
        self$allMethodNames <- methodNames
        self$fieldNames <- names(Cpublic)[!isMethod]
        self$allFieldNames <- fieldNames
        if(!is.null(inherit)) {
          self$symbolTable$setParentST(inheritNCinternals$symbolTable)
          self$allMethodNames <- c(self$allMethodNames, inheritNCinternals$allMethodNames)
          self$allFieldNames <- c(self$allFieldNames, inheritNCinternals$allFieldNames)
        }
      }
      if(!is.null(enableDerivs)) {
        if(!is.list(enableDerivs))
          enableDerivs <- as.list(enableDerivs)
        for(i in enableDerivs) {
          if(!(i %in% self$methodNames))
            stop(paste0('enableDerivs entry ', i, ' is not a method in Cpublic.'))
        }
        self$enableDerivs <- enableDerivs
        self$predefined <- predefined
      }
      self$enableSaving <- enableSaving
    }
  )
)
