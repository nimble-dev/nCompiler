
NC_InternalsClass <- R6::R6Class(
  classname = "NC_InternalsClass",
  portable = FALSE,
  public = list(
    symbolTable = NULL,
    cppSymbolNames = NULL,
    methodNames = character(),
    #allMethodNames = character(), # including inherited methods
    allMethodNames_self = character(), # not including inherited methods
    fieldNames = character(),
    #allFieldNames = character(), # including inherited methods
    allFieldNames_self = character(), # not including inherited methods
    classname = character(),
    cpp_classname = character(),
    #all_methodName_to_cpp_code_name = list(),
    orig_methodName_to_cpp_code_name = list(),
    compileInfo = list(),
    inherit_base_provided = FALSE,
    # compileInfo will include interface ("full", "generic", or "none"),
    # interfaceInclude, exportName, and depends
    depends = list(),
    RcppPacket = NULL,
    isOnlyC = FALSE, ## somewhat redundant but perhaps convenient - TBD.
    enableDerivs = NULL,
    enableSaving = NULL,
    predefined = FALSE, # directory for reading and (default) writing predefined nClass saved RcppPacket. Writing location can be over-ridden by compileInfo$predefined_output_dir
    # inheritNCinternals = NULL,
    env = NULL,
    inheritQ = NULL, # quoted inherit expression, to defer access to the inherited nClass generator itself.
    # process_inherit_done = FALSE,
    virtualMethodNames_self = character(), # will be used when checking inherited method validity, only for locally implemented methods
    # virtualMethodNames = character(),
    #check_inherit_done = FALSE,
    classID = NULL,
    #Cpub_class_code = NULL,
    #main_class_code = NULL,
    RpublicNames = character(),
    initialize = function(classname,
                          Cpublic,
                          RpublicNames = character(),
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
      self$cpp_classname <- if(!is.null(compileInfo$cpp_classname)) compileInfo$cpp_classname else Rname2CppName(classname)
      self$classID <- self$cpp_classname
      self$RpublicNames <- RpublicNames
      self$isOnlyC = length(RpublicNames) == 0
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
        has_Cpublic_init <- "initialize" %in% names(Cpublic)
        #self$virtualMethodNames <- names(Cpublic)[isVirtual]
        self$symbolTable <- typeList2symbolTable(Cpublic[!isMethod], where = env)
        self$cppSymbolNames <- Rname2CppName(symbolTable$getSymbolNames())
        self$methodNames <- names(Cpublic)[isMethod]
        if(has_Cpublic_init) {
          if("initialize" %in% self$methodNames) {
            stop("The name 'initialize' in Cpublic can only be used for an R function to provide any special initialization handling (usually not needed).",
                 call. = FALSE)
          }
        }
        self$allMethodNames_self <- methodNames
        self$virtualMethodNames_self <- names(Cpublic)[isVirtual]
        #self$allMethodNames <- methodNames
        self$fieldNames <- names(Cpublic)[!isMethod]
        if(has_Cpublic_init) self$fieldNames <- setdiff(self$fieldNames, "initialize")
        self$allFieldNames_self <- fieldNames
        #self$allFieldNames <- fieldNames
        self$orig_methodName_to_cpp_code_name <- structure(vector("list", length=length(methodNames)),
                                                       names = methodNames)
        for(mN in methodNames) {
          self$orig_methodName_to_cpp_code_name[[mN]] <- NFinternals(Cpublic[[mN]])$cpp_code_name
        }
        # The next three are normally set up during inheritance processing below,
        # but if an nClass is predefined and used in wierd compilation workflow 
        # like in nimble2, then we need defaults set up, and here they are:
        # self$allMethodNames <- self$allMethodNames_self. # already done above
        #self$all_methodName_to_cpp_code_name <- self$orig_methodName_to_cpp_code_name
        # self$allFieldNames <- self$allFieldNames_self. # already done above
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
      classname_provided <- !identical(names(classname), "generated")
      packageNames <- c(uncompiled = "", compiled = "")
      if(!is.null(self$compileInfo$packageNames)) packageNames <- self$compileInfo$packageNames
      if(is.list(packageNames)) packageNames <- unlist(packageNames)
      if(is.null(names(packageNames)))
        names(packageNames) <- c("uncompiled", "compiled")[seq_along(packageNames)]
      if(is.na(packageNames["compiled"])) packageNames["compiled"] <- ""
      if(is.na(packageNames["uncompiled"])) packageNames["uncompiled"] <- ""
      packageNames <- packageNames[c("uncompiled", "compiled")]
      if(classname_provided && packageNames["uncompiled"] == "") 
        packageNames["uncompiled"] <- classname
      self$compileInfo$packageNames <- packageNames

      self$predefined <- predefined
      self$enableSaving <- enableSaving
    },
    # connect_inherit = function(inheritInfo, symbolTable, project_env) {
    #   # These are steps that need to be done after all classes are defined
    #   # and do not require recursion up the inheritance tree.
    #   if(!is.null(self$inheritQ)) {
    #     inherit_obj <- eval(self$inheritQ, envir = self$env) #inheritQ can be an expression but it must always return the same generator object
    #     if(!isNCgenerator(inherit_obj))
    #       stop("An inherit argument that was provided to nClass does not evaluate to an nClass generator.")
    #     # self$inheritNCinternals <- NCinternals(inherit_obj)
    #     parent_nClass_Info <- register_known_nClass(inherit_obj, project_env)
    #     symbolTable$setParentST(parent_nClass_Info$symbolTable)
    #     inheritInfo$inheritNCinternals <- NCinternals(inherit_obj)
    #     inheritInfo$nClass_inherit <- self$compileInfo$nClass_inherit
    #     if(!self$inherit_base_provided) {
    #       #self$compileInfo$nClass_inherit$base <- self$inheritNCinternals$cpp_classname # don't paste "public" because it will go in interface_resolver<
    #       inheritInfo$nClass_inherit$base <- self$inheritNCinternals$cpp_classname
    #     }
    #   }
    #   inheritInfo$process_inherit_done <- FALSE
    #   inheritInfo$check_inherit_done <- FALSE
    # },
    process_inherit = function(inheritInfo, symbolTable, project_env) {
      # These are steps that need to be done after connect_inherit
      # and require recursion up the inheritance tree, using flags.
      # TO-DO: Error trap in methods of same name but different argument signatures.
      if(isTRUE(inheritInfo$process_inherit_done)) return()
      if(!is.null(self$inheritQ)) {
        inherit_obj <- eval(self$inheritQ, envir = self$env) #inheritQ can be an expression but it must always return the same generator object
        if(!isNCgenerator(inherit_obj))
          stop("An inherit argument that was provided to nClass does not evaluate to an nClass generator.")
        # self$inheritNCinternals <- NCinternals(inherit_obj)
        parent_nClass_Info <- register_known_nClass(inherit_obj, project_env)
        symbolTable$setParentST(parent_nClass_Info$symbolTable)
        inheritInfo$inheritNCinternals <- NCinternals(inherit_obj)
        inheritInfo$nClass_inherit <- self$compileInfo$nClass_inherit
        # if(!self$inherit_base_provided) {
        #   #self$compileInfo$nClass_inherit$base <- self$inheritNCinternals$cpp_classname # don't paste "public" because it will go in interface_resolver<
        #   inheritInfo$nClass_inherit$base <- inheritInfo$inheritNCinternals$cpp_classname
        # }
        #self$inheritNCinternals$process_inherit()
        #self$symbolTable$setParentST(self$inheritNCinternals$symbolTable)
        newMethodNames <- setdiff(self$allMethodNames_self,
                                  parent_nClass_Info$inheritInfo$allMethodNames)
        inheritInfo$allMethodNames <- c(newMethodNames, parent_nClass_Info$inheritInfo$allMethodNames)
        inheritInfo$all_methodName_to_cpp_code_name <- c(self$orig_methodName_to_cpp_code_name[newMethodNames],
                                                parent_nClass_Info$inheritInfo$all_methodName_to_cpp_code_name)
        inheritInfo$allFieldNames <- c(self$allFieldNames_self, parent_nClass_Info$inheritInfo$allFieldNames)
        # self$allMethodNames <- c(newMethodNames, self$inheritNCinternals$allMethodNames)
        # self$all_methodName_to_cpp_code_name <- c(self$orig_methodName_to_cpp_code_name[newMethodNames],
        #                                         self$inheritNCinternals$all_methodName_to_cpp_code_name)
        # self$allFieldNames <- c(self$allFieldNames_self, self$inheritNCinternals$allFieldNames)
        # 
        # copy inherited overloadDefs and then add or replace with any overloadDefs from this class.
        # This should automatically create the hierarchy correctly.
        overloadDefs <- parent_nClass_Info$inheritInfo$overloadDefs
        new_overloadDefs <- self$compileInfo$overloadDefs
        for(mN in names(new_overloadDefs)) {
          overloadDefs[[mN]] <- new_overloadDefs[[mN]]
        }
        inheritInfo$overloadDefs <- overloadDefs
      } else {
        inheritInfo$allMethodNames <- self$allMethodNames_self
        inheritInfo$all_methodName_to_cpp_code_name <- self$orig_methodName_to_cpp_code_name
        inheritInfo$allFieldNames <- self$allFieldNames_self
        inheritInfo$overloadDefs <- self$compileInfo$overloadDefs %||% list()
        symbolTable$setParentST(NULL)
      } 
      inheritInfo$process_inherit_done <- TRUE
      inheritInfo$check_inherit_done <- FALSE
    }
  )
)
