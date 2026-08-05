
NC_InternalsClass <- R6::R6Class(
  classname = "NC_InternalsClass",
  portable = FALSE,
  public = list(
    symbolTable = NULL,
    cppSymbolNames = NULL,
    methodNames = character(), # this class's own methods, not including inherited ones
    #allMethodNames = character(), # including inherited methods
    fieldNames = character(), # this class's own fields, not including inherited ones
    #allFieldNames = character(), # including inherited methods
    classname = character(),
    cpp_classname = character(),
    #all_methodName_to_cpp_code_name = list(),
    orig_methodName_to_cpp_code_name = list(),
    orig_methodInfo = list(),
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
    virtualMethodNames = character(), # this class's own virtual methods, not including inherited ones; will be used when checking inherited method validity
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
        self$symbolTable <- typeList2symbolTable(Cpublic[!isMethod], where = env)
        self$cppSymbolNames <- Rname2CppName(symbolTable$getSymbolNames())
        self$methodNames <- names(Cpublic)[isMethod]
        if(has_Cpublic_init) {
          if("initialize" %in% self$methodNames) {
            stop("The name 'initialize' in Cpublic can only be used for an R function to provide any special initialization handling (usually not needed).",
                 call. = FALSE)
          }
        }
        self$virtualMethodNames <- names(Cpublic)[isVirtual]
        #self$allMethodNames <- methodNames
        self$fieldNames <- names(Cpublic)[!isMethod]
        if(has_Cpublic_init) self$fieldNames <- setdiff(self$fieldNames, "initialize")
        #self$allFieldNames <- fieldNames
        self$orig_methodName_to_cpp_code_name <- structure(vector("list", length=length(methodNames)),
                                                       names = methodNames)
        # orig_methodInfo carries the raw, per-own-method ingredients
        # addGenericInterface_impl (cppDefs_core.R) needs to emit a method(...) line --
        # owning C++ class, argument names/passing-mode flags, and the destructor/
        # constructor/callFromR flags -- as plain data, not assembled C++ text and not
        # yet folded with interfaceInclude/interfaceExclude (that decision needs
        # self$compileInfo, but is deferred to process_inherit, which computes it in
        # one place shared with fields). Built here, not in process_inherit, only
        # because it needs the actual method objects (Cpublic), which process_inherit
        # doesn't receive -- only initialize does. The actual C++ identifier for a method
        # (needed for override/virtual-dispatch consistency with the base class) comes
        # from all_methodName_to_cpp_code_name, not from anything stored here.
        self$orig_methodInfo <- structure(vector("list", length=length(methodNames)),
                                          names = methodNames)
        for(mN in methodNames) {
          NFint <- NFinternals(Cpublic[[mN]])
          NFcompInfo <- NFint$compileInfo
          self$orig_methodName_to_cpp_code_name[[mN]] <- NFint$cpp_code_name
          self$orig_methodInfo[[mN]] <- list(argNames = NFint$argSymTab$getSymbolNames(), # we do not want cpp names here.
                                             refArgs = NFint$refArgs,
                                             blockRefArgs = NFint$blockRefArgs,
                                             ownerClassName = self$cpp_classname,
                                             destructor = isTRUE(NFcompInfo$destructor),
                                             constructor = isTRUE(NFcompInfo$constructor),
                                             callFromR = isTRUE(NFcompInfo$callFromR))
        }
        # The next three are normally set up during inheritance processing below,
        # but if an nClass is predefined and used in wierd compilation workflow 
        # like in nimble2, then we need defaults set up, and here they are:
        # self$allMethodNames <- self$methodNames. # already done above
        #self$all_methodName_to_cpp_code_name <- self$orig_methodName_to_cpp_code_name
        # self$allFieldNames <- self$fieldNames. # already done above
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
      # allFieldInfo carries everything addGenericInterface_impl (cppDefs_core.R)
      # needs to emit a field(...) line for each field, flattened across the whole
      # inheritance chain: which C++ class it belongs to, its cpp-mangled name, its
      # final generic-interface inclusion decision, and its (symbol-owned, already
      # C++-ready) interfaceAux text. Building it here -- rather than in
      # addGenericInterface_impl, which would otherwise have to walk ancestors via
      # raw/unresolved NCinternals -- means callers get one complete, already-merged,
      # already-deduplicated (self's own field wins on a name collision, mirroring
      # allMethodNames/all_methodName_to_cpp_code_name below) map, and
      # addGenericInterface_impl never needs to know how a field's inclusion was
      # decided or what kind of aux content a symbol contributes.
      #
      # The inclusion decision folds in both the field's own (already TBD-resolved,
      # via the symbolTable passed in here) interface flag and this class's
      # interfaceInclude/interfaceExclude override -- previously computed in
      # addGenericInterface_impl per ancestor level, now computed here per class
      # since self$compileInfo is exactly the level whose override should govern
      # its own fields, same as before.
      # self$compileInfo's interfaceInclude/interfaceExclude governs both fields and
      # methods identically, so both inclusion decisions below fold in the same
      # useIM/use_include/interfaceInclude/interfaceExclude.
      interfaceInclude <- self$compileInfo$interfaceInclude
      interfaceExclude <- self$compileInfo$interfaceExclude
      useIM <- !is.null(interfaceInclude) || !is.null(interfaceExclude)
      if(useIM && !is.null(interfaceInclude) && !is.null(interfaceExclude)) {
        stop("interfaceExclude and interfaceInclude cannot both be non-null.  Something is wrong.")
      }
      use_include <- useIM && !is.null(interfaceInclude)
      # Needs to be built here because it relies on resolved symbols. cppName is
      # looked up from cppSymbolNames (already computed in initialize) rather than
      # recomputing Rname2CppName(nm) here -- keyed by name (via symbolTable's own
      # names), not by position, since fieldNames can be a strict subset of
      # symbolTable's names (the has_Cpublic_init/"initialize" case in initialize
      # adjusts fieldNames but not symbolTable/cppSymbolNames).
      cppNameLookup <- setNames(self$cppSymbolNames, symbolTable$getSymbolNames())
      self_fieldInfo_all <- structure(
        lapply(self$fieldNames, \(nm) {
          sym <- symbolTable$getSymbol(nm)
          included <- if(useIM) {
            if(use_include) nm %in% interfaceInclude
            else isTRUE(sym$interface) && !(nm %in% interfaceExclude)
          } else {
            isTRUE(sym$interface)
          }
          list(cppName = cppNameLookup[[nm]],
               ownerClassName = self$cpp_classname,
               interface = included,
               interfaceAux = sym$interfaceAux)
        }),
        names = self$fieldNames)
      # Folds the interfaceInclude/interfaceExclude decision into the raw per-method
      # data built in initialize (destructor/constructor always excluded; otherwise
      # interfaceInclude/interfaceExclude if set, else the method's own callFromR
      # flag) -- the plain-data ingredients (argNames/refArgs/blockRefArgs) pass
      # through unchanged; addGenericInterface_impl (cppDefs_core.R) is responsible
      # for assembling them into actual args({...}) C++ text, since that's the C++
      # code generation stage.
      self_methodInfo_all <- structure(
        lapply(self$methodNames, \(mN) {
          raw <- self$orig_methodInfo[[mN]]
          included <- !raw$destructor && !raw$constructor &&
                      (if(useIM) {
                        if(use_include) (mN %in% interfaceInclude)
                        else !(mN %in% interfaceExclude)
                      } else {
                        raw$callFromR
                      })
          list(argNames = raw$argNames,
               refArgs = raw$refArgs,
               blockRefArgs = raw$blockRefArgs,
               ownerClassName = raw$ownerClassName,
               interface = included)
        }),
        names = self$methodNames)
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
        newMethodNames <- setdiff(self$methodNames,
                                  parent_nClass_Info$inheritInfo$allMethodNames)
        inheritInfo$allMethodNames <- c(newMethodNames, parent_nClass_Info$inheritInfo$allMethodNames)
        inheritInfo$all_methodName_to_cpp_code_name <- c(self$orig_methodName_to_cpp_code_name[newMethodNames],
                                                parent_nClass_Info$inheritInfo$all_methodName_to_cpp_code_name)
        # allMethodInfo/allFieldInfo are the opposite precedence from
        # all_methodName_to_cpp_code_name above: self's own record wins on a name
        # collision (ownerClassName/argNames/refArgs/blockRefArgs/cppName/interfaceAux
        # come from wherever the name is most-derived), matching the old per-level walk
        # in addGenericInterface_impl, which started at the derived class and skipped a
        # name only once already output -- i.e. the derived declaration's own info was
        # captured first. Only all_methodName_to_cpp_code_name is intentionally
        # base-wins, since virtual dispatch requires the override to share the base's
        # C++ identifier; that's unrelated to which class's info populates these maps.
        inheritInfo$allMethodInfo <- c(self_methodInfo_all,
                                       parent_nClass_Info$inheritInfo$allMethodInfo[
                                         setdiff(names(parent_nClass_Info$inheritInfo$allMethodInfo), self$methodNames)])
        inheritInfo$allFieldNames <- c(self$fieldNames, parent_nClass_Info$inheritInfo$allFieldNames)
        inheritInfo$allFieldInfo <- c(self_fieldInfo_all,
                                      parent_nClass_Info$inheritInfo$allFieldInfo[
                                        setdiff(names(parent_nClass_Info$inheritInfo$allFieldInfo), self$fieldNames)])
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
        inheritInfo$allMethodNames <- self$methodNames
        inheritInfo$all_methodName_to_cpp_code_name <- self$orig_methodName_to_cpp_code_name
        inheritInfo$allMethodInfo <- self_methodInfo_all
        inheritInfo$allFieldNames <- self$fieldNames
        inheritInfo$allFieldInfo <- self_fieldInfo_all
        inheritInfo$overloadDefs <- self$compileInfo$overloadDefs %||% list()
        symbolTable$setParentST(NULL)
      } 
      inheritInfo$process_inherit_done <- TRUE
      inheritInfo$check_inherit_done <- FALSE
    }
  )
)
