
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
    all_methodName_to_cpp_code_name = list(),
    orig_methodName_to_cpp_code_name = list(),
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
    virtualMethodNames_self = character(), # will be used when checking inherited method validity, only for locally implemented methods
    virtualMethodNames = character(),
    check_inherit_done = FALSE,
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
        isVirtual <- rep(FALSE, numEntries)
        for(i in seq_along(Cpublic)) {
          if(isNF(Cpublic[[i]])) {
            isMethod[i] <- TRUE
            isVirtual[i] <- isTRUE(NFinternals(Cpublic[[i]])$compileInfo$virtual)
            # NFinternals(Cpublic[[i]])$isMethod <- TRUE 
            next;
          }
          if(is.function(Cpublic[[i]])) {
            stop(paste0('Cpublic methods should be provided as nFunctions, ',
                        'not functions. ', names(Cpublic)[i], ' is a function.'),
                 call. = FALSE)
          }
        }
        self$virtualMethodNames <- names(Cpublic)[isVirtual]
        self$symbolTable <- argTypeList2symbolTable(Cpublic[!isMethod], evalEnv = env)
        self$cppSymbolNames <- Rname2CppName(symbolTable$getSymbolNames())
        self$methodNames <- names(Cpublic)[isMethod]
        self$allMethodNames_self <- methodNames
        self$virtualMethodNames_self <- names(Cpublic)[isVirtual]
        self$allMethodNames <- methodNames
        self$fieldNames <- names(Cpublic)[!isMethod]
        self$allFieldNames_self <- fieldNames
        self$allFieldNames <- fieldNames
        self$orig_methodName_to_cpp_code_name <- structure(vector("list", length=length(methodNames)),
                                                       names = methodNames)
        for(mN in methodNames) {
          self$orig_methodName_to_cpp_code_name[[mN]] <- NFinternals(Cpublic[[mN]])$CPPCODENAME2
        }
      }
      # An over-riding base class can be provided either through inherit or nClass_inherit.
      if(!is.null(self$compileInfo$inherit$base) || !is.null(self$compileInfo$nClass_inherit$base))
          self$inherit_base_provided <- TRUE
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
        inherit_obj <- eval(self$inheritQ, envir = self$env) #inheritQ can be an expression but it must always return the same generator object
        if(!isNCgenerator(inherit_obj))
          stop("An inherit argument that was provided to nClass is not nClass generator.")
        self$inheritNCinternals <- NCinternals(inherit_obj)
        message("add check that base class has interface 'none'")
        if(!self$inherit_base_provided) {
          self$compileInfo$nClass_inherit$base <- self$inheritNCinternals$cpp_classname # don't paste "public" because it will go in interface_resolver<
        }
      }
      self$process_inherit_done <- FALSE
      self$check_inherit_done <- FALSE
    },
    process_inherit = function() {
      # These are steps that need to be done after connect_inherit
      # and require recursion up the inheritance tree, using flags.
      # TO-DO: Error trap in methods of same name but different argument signatures.
      if(self$process_inherit_done) return()
      if(!is.null(self$inheritQ)) {
        self$inheritNCinternals$process_inherit()
        self$symbolTable$setParentST(self$inheritNCinternals$symbolTable)
        newMethodNames <- setdiff(self$allMethodNames_self,
                                  self$inheritNCinternals$allMethodNames)
        self$allMethodNames <- c(newMethodNames, self$inheritNCinternals$allMethodNames)
        self$all_methodName_to_cpp_code_name <- c(self$orig_methodName_to_cpp_code_name[newMethodNames],
                                                self$inheritNCinternals$all_methodName_to_cpp_code_name)
        self$allFieldNames <- c(self$allFieldNames_self, self$inheritNCinternals$allFieldNames)
      } else {
        self$allMethodNames <- self$allMethodNames_self
        self$all_methodName_to_cpp_code_name <- self$orig_methodName_to_cpp_code_name
        self$allFieldNames <- self$allFieldNames_self
        self$symbolTable$setParentST(NULL)
      } 
      self$process_inherit_done <- TRUE
    }
  )
)
