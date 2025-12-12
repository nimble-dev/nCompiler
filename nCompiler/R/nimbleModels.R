# Here we are drafting support for new implementation of nimbleModels.
# This should eventually live in a separate package, but for now it is easier to draft here.
#
# see test-nimbleModel too.

## modelBase_nClass will be a base class with methods that
## have separate Rfun and Cfun contents and are predefined.
##
## model_nClass will inherit from modelBase_nClass and in C++ will
## use CRTP for a derived model.
## It will also split Rfun and Cfun and provide a custom inheritance statement
## It may provide different sets of calculate modes.
## It will also be predefined (will it get an interface?)

## a model will inherit from model_nClass

nodeInstr_nClass <- nClass(
  classname = "nodeInstr_nClass",
  Cpublic = list(
    methodInstr = 'integerVector',
    indsInstrVec = "nList('integerVector')"
  ),
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |>
               file.path("nodeInstr_nC")),
  compileInfo=list(interface="full",
                   createFromR = TRUE,
                   exportName = "nodeInstr_nClass"
                   )
)

calcInstr_nClass <- nClass(
  classname = "calcInstr_nClass",
  Cpublic = list(
    nodeIndex = 'integerScalar',
    nodeInstrVec = "nList('nodeInstr_nClass')"
  ),
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |>
               file.path("calcInstr_nC")),
  compileInfo=list(interface="full",
                   createFromR = TRUE,
                   # The Hincludes should be picked up automatically but I think it's not 
                   # because it is in the nList type and that is not being scanned for needed nClasses.
                   # These do need to be in "" not <>, for case of nCompile(...., package=TRUE)
                   Hincludes = '"nodeInstr_nClass_c_.h"',
                   # In the format here, needed_units is a list with either objects (nFunction or nClass (generators),
                   # or names. If names, we will use scoping to look them up and decide what they are.
                   # The list can mix objects and names of nClasses and nFunctions.
                   needed_units = list("nodeInstr_nClass"),
                   exportName = "calcInstr_nClass"
                   )
)

calcInstrList_nClass <- nClass(
  classname = "calcInstrList_nClass",
  Cpublic = list(
    calcInstrList = "nList('calcInstr_nClass')"
  ),
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |>
               file.path("calcInstrList_nC")),
  compileInfo=list(interface="full",
                   createFromR = TRUE,
                   Hincludes = '"calcInstr_nClass_c_.h"',
                   exportName = "calcInstrList_nClass",
                   needed_units = list("calcInstr_nClass")
                   )
)

nodeFxnBase_nClass <- nClass(
  classname = "nodeFxnBase_nClass",
  Cpublic = list(
    ping = nFunction(
      name = "ping",
      function() {return(TRUE); returnType(logical())},
      compileInfo = list(virtual=TRUE)
    ),
    calculate = nFunction(
      name = "calculate",
      function(nodeInstr = 'nodeInstr_nClass') {return(0); returnType(double())},
      compileInfo = list(virtual=TRUE)
    )
  ),
  # We haven't dealt with ensuring a virtual destructor when any method is virtual
  # For now I did it manually by editing the .h and .cpp
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |>
               file.path("nodeFxnBase_nC")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   exportName = "nodeFxnBase_nClass")
)

# nCompile(nodeFxnBase_nClass, control=list(generate_predefined=TRUE))

modelBase_nClass <- nClass(
  classname = "modelBase_nClass",
  Cpublic = list(
    ping = nFunction(
        name = "ping",
        function() {return(TRUE); returnType(logical())},
        compileInfo = list(virtual=TRUE)
    ),
    calculate = nFunction(
        name = "calculate",
        function(calcInstrList) {
          cat("In uncompiled calculate\n")
          # This is where uncompiled stepping through the calcInstrList happens.
          for(calcInstr in calcInstrList$calcInstrList) {
            nodeIdx <- calcInstr$nodeIndex
            nodemember_name <- self$nodeObjNames[nodeIdx] # nodeObjNames is found in the derived class
            for(nodeInstr in calcInstr$nodeInstrVec) {
              self[[nodemember_name]]$calculate(nodeInstr)
            }
          }
          return(0)
        },
        returnType = 'numericScalar',
        compileInfo = list(
          C_fun = function(calcInstrList='calcInstrList_nClass') {
            cppLiteral('Rprintf("modelBase_nClass calculate (should not see this)\\n");'); return(0)},
          virtual=TRUE
        )
    )  
  ),
  # See comment above about needing to ensure a virtual destructor
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |> file.path("modelBase_nC")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   Hincludes = c('"nodeFxnBase_nClass_c_.h"', '"calcInstrList_nClass_c_.h"'), # do we need "<nodeFxnBase_nClass_c_.h>" too?
                   needed_units = list("nodeFxnBase_nClass","calcInstrList_nClass"), #do we need nodeFxnBase_nClass here too?
                   exportName = "modelBase_nClass"
                   )
)

# nCompile(modelBase_nClass, control=list(generate_predefined=TRUE))

## The two "addModelDollarSign" functions are borrowed directly from nimble.
## This should add model$ in front of any names that are not already part of a '$' expression
nm_addModelDollarSign <- function(expr, exceptionNames = character(0)) {
    if(is.numeric(expr)) return(expr)
    if(is(expr, 'srcref')) return(expr)
    if(is.name(expr)) {
        if((as.character(expr) %in% exceptionNames) || (as.character(expr) == ''))    return(expr)
        proto <- quote(model$a)
        proto[[3]] <- expr
        return(proto)
    }
    if(is.call(expr)) {
        if(expr[[1]] == '$'){
            expr[2] <- lapply(expr[2], function(listElement) nm_addModelDollarSign(listElement, exceptionNames))
            return(expr)
        } 
        if(expr[[1]] == 'returnType')
            return(expr)
        if(length(expr) > 1) {
            expr[2:length(expr)] <- lapply(expr[-1], function(listElement) nm_addModelDollarSign(listElement, exceptionNames))
            return(expr)
        }
    }
    return(expr)
}

## test <- nClass(
##   inherit = modelBase_nClass,
##   classname = "my_model",
##   Cpublic = list(
##     simulate = nFunction(function(nodes=SEXP()) {cppLiteral('Rprintf("In Derived simulate")')})
##   ),
##   compileInfo = list(
##     nClass_inherit = list(base="modelClass_<my_model>")
##   )
## )

## comp <- nCompile(test, modelBase_nClass, nodeFxnBase_nClass)
## obj <- comp$test$new()
## obj$calculate(NULL)

# Turn variables and methods into a nodeFxn nClass
make_node_nClass <- function(varInfo = list(),
                            methods = list(),
                            classname) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  # These are the model member variables to be used by the nodeFxn.
  # They will be used in a constructor to set up C++ references to model variables.
  varInfo_2_symbol <- \(x) nCompiler:::symbolBasic$new(
    type="double", nDim=x$nDim, name="", isRef=TRUE, isConst=FALSE, interface=FALSE) # In future maybe isConst=TRUE, but it might not matter much
  symbolList <- varInfo |> lapply(varInfo_2_symbol)
  names(symbolList) <- varInfo |> lapply(\(x) x$name) |> unlist()
  numVars <- length(varInfo)

  CpublicVars <- names(symbolList) |> lapply(\(x) eval(substitute(quote(T(symbolList$NAME)),
                                                    list(NAME=as.name(x)))))
  names(CpublicVars) <- names(symbolList)
  initFun <- function(){}
  
  if(numVars > 0) {
    ctorArgNames <- paste0(names(symbolList), '_')
    # List used when generating C++ constructor code to allow direct initializers, necessary for references.
    initializersList <- paste0(names(symbolList), '(', ctorArgNames ,')')
    formals(initFun) <- structure(as.pairlist(CpublicVars), names = ctorArgNames)
  } else {
    initializersList <- character()
  }
  if(missing(classname))
    classname <- nodeFxnLabelCreator()

  baseclass <- paste0("nodeFxnClass_<", classname, ">")

  # Rpublic method to set the model pointer/reference.
  setModel <- function(model) {
    if(!isCompiled())
      self$model <- model
    else
       warning("setModel called on compiled object; no action taken")
  }

#  This was a prototype
  node_nClass <- substitute(
    nClass(
      inherit = nodeFxnBase_nClass,
      classname = CLASSNAME,
      Rpublic = RPUBLIC,
      Cpublic = CPUBLIC,
      compileInfo = list(
        createFromR = FALSE, # Without a default constructor (which we've disabled here), createFromR is impossible
        nClass_inherit = list(base = BASECLASS))  # Ideally this line would be obtained from a base nClass, but we insert it directly for now
    ),
    list(
      CPUBLIC = c(
        list(
          nFunction(
            initFun,
            compileInfo = list(constructor=TRUE, initializers = initializersList)
          )
        ) |> structure(names = classname),
        CpublicVars,
        methods
      ),
      RPUBLIC = list(model = NULL,
                     setModel = setModel),
      CLASSNAME = classname,
      BASECLASS = baseclass
    ))
  eval(node_nClass)
}
#test <- nCompiler:::argType2symbol('CppVar(baseType = argType2Cpp("numericVector"), ref=TRUE, const=TRUE)')

# Make all the info needed to include a node in a model class.
# The nodeFxn_nClass should be created first.
# Currently it needs to have a name to include in nCompile(). Later we might be able to pass the object itself
# At first drafting this is fairly trivial but could grow in complexity.

make_node_info_for_model_nClass <- function(membername,
                           nodeFxnName,
                           classname,
                           varInfo = list()
                           ) {
  ctorArgs <- varInfo |> lapply(\(x) x$name) |> unlist()

  list(nodeFxnName = nodeFxnName,
       membername = membername,
       classname = classname,
       ctorArgs = ctorArgs)
}

makeModel_nClass <- function(varInfo,
                             nodes = list(),
                             classname,
                             sizes = list(),
                             inits = list(),
                             env = parent.frame()
                             ) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  CpublicModelVars <- varInfo |> lapply(\(x) paste0("numericArray(nDim=",x$nDim,")"))
  names(CpublicModelVars) <- varInfo |> lapply(\(x) x$name) |> unlist()
  opDefs <- list(
    base_ping = getOperatorDef("custom_call"),
    setup_node_mgmt = getOperatorDef("custom_call"),
    do_setup_node_mgmt_from_names = getOperatorDef("custom_call")
  )
  opDefs$base_ping$returnType <- nCompiler:::argType2symbol(quote(void()))
  opDefs$base_ping$labelAbstractTypes$recurse <- FALSE
  opDefs$setup_node_mgmt$returnType <- nCompiler:::argType2symbol(quote(void()))
  opDefs$setup_node_mgmt$labelAbstractTypes$recurse <- FALSE
  opDefs$do_setup_node_mgmt_from_names$returnType <- nCompiler:::argType2symbol(quote(void()))
  opDefs$do_setup_node_mgmt_from_names$labelAbstractTypes$recurse <- FALSE

  if(missing(classname))
    classname <- modelLabelCreator()

  CpublicMethods <- list(
    do_setup_node_mgmt = nFunction(
      name = "call_setup_node_mgmt",
      function() {},
      compileInfo=list(
        C_fun = function() {setup_node_mgmt()})
    ),
    setup_node_mgmt_from_names = nFunction(
      name = "call_setup_node_mgmt_from_names",
      function(nodeNames) {},
      compileInfo=list(
        C_fun = function(nodeNames="RcppCharacterVector") {do_setup_node_mgmt_from_names(nodeNames)})
    ),
    print_nodes = nFunction(
      name = "print_nodes",
      function() {},
      compileInfo=list(
        C_fun = function() {cppLiteral('modelClass_::c_print_nodes();')})
    ),
    set_from_list = nFunction(
      name = "set_from_list",
      function(Rlist) {for(v in names(Rlist))
        if(exists(v, self, inherits=FALSE)) self[[v]] <- Rlist[[v]]},
      compileInfo=list(
        C_fun=function(Rlist = 'RcppList') {cppLiteral('modelClass_::set_from_list(Rlist);')})
    ),
    resize_from_list = nFunction(
      name = "resize_from_list",
      function(Rlist) {for(v in names(Rlist))
        if(exists(v, self, inherits=FALSE)) self[[v]] <- nArray(dim=Rlist[[v]])},
      compileInfo = list(
        C_fun=function(Rlist = 'RcppList') {cppLiteral('modelClass_::resize_from_list(Rlist);')})
    )
  )
  # nodes will be a list of membername, nodeFxnName, (node) classname, ctorArgs (list)
  node_pieces <- nodes |> lapply(\(x) {
    #nClass_type <- paste0(x$nodeFxnName, "()")
    init_string <- paste0('nCpp("', x$membername, '( new ', x$classname, '(',
                                    paste0(x$ctorArgs, collapse=","), '))")')
    list(nClass_type = x$nodeFxnName,
         init_string = init_string,
         membername = x$membername)
  })
  nodeObjNames <- (node_pieces |> lapply(\(x) x$membername) |> unlist()) %||% character()
  # nodeObjNames also serves for canonical lookup of names by index.
  # e.g. nodeObjNames[i] gives the member name of the index=i node member.
  nodeObjName_2_nodeIndex <- seq_along(nodeObjNames) |> structure(names=nodeObjNames)
  # Inversely, nodeobjName_2_nodeIndex["node_3"] gives the index of that node.
  CpublicNodeFuns <- node_pieces |> lapply(\(x) x$nClass_type) |> setNames(nodeObjNames)
  # CpublicNodeFuns <- list(
  #   beta_node = 'node_dnorm()'
  # )
  CpublicCtor <- list(
    nFunction(
      function(){},
      compileInfo = list(constructor=TRUE,
                         #initializers = c('nCpp("beta_node(new node_dnorm(mu, beta, 1))")'))
                         initializers = node_pieces |> lapply(\(x) x$init_string) |> unlist())
    )
  ) |> structure(names = classname)
  initialize <- function(sizes = list(), inits = list()) {
    # It is not very easy to set debug onto the initialize function, so
    # here is a magic flag.
    if(isTRUE(.GlobalEnv$.debugModelInit)) browser()
    if(isCompiled())
      self$setup_node_mgmt_from_names(self$nodeObjNames)
    if(!isCompiled()) {
      for(nodeObj in self$nodeObjNames) {
        self[[nodeObj]] <- eval(as.name(self$CpublicNodeFuns[[nodeObj]]))$new()
        self[[nodeObj]]$setModel(self)
      }
    }
    
    # First expand any provided or default sizes
    # To-Do possibly merge the argument sizes and defaultSizes by element.
    if(missing(sizes)) sizes <- self$defaultSizes
    if(length(sizes)) resize_from_list(sizes)

    # Then any provided inits over-ride any provided sizes
    # To-Do: Ditto
    if(missing(inits)) inits <- self$defaultInits
    if(length(inits)) init_from_list(inits)
  }
  baseclass <- paste0("modelClass_<", classname, ">")
  # CpublicNodeFuns has elements like "node_1 = quote(nodeFxn_1())"
  # We provide it in Cpublic to declare C++ member variables with types.
  # We also place the list itself in the class so that we can look up for uncompiled execution 
  # the objects that need to be created in initialize.
  # If we someday make type declarations and initializations more automatic, we can avoid this duplication.
  ans <- substitute(
    nClass(
      classname = CLASSNAME,
      inherit = modelBase_nClass,
      compileInfo = list(opDefs = OPDEFS,
                         nClass_inherit = list(base=BASECLASS)
                         #inherit = list(base = "public modelClass_<mymodel>"),
                         #Hincludes = "<nCompiler/nC_inter/post_Rcpp/nCompiler_model_base_devel.h>"
                         ),
      Rpublic = RPUBLIC,
      Cpublic = CPUBLIC,
      env = env
    ),
    list(OPDEFS = opDefs,
        # A list of individual elements
        RPUBLIC = list(initialize=initialize, 
                      nodeObjNames = nodeObjNames,
                      nodeObjName_2_nodeIndex = nodeObjName_2_nodeIndex, 
                      defaultSizes = sizes,
                      defaultInits = inits,
                      CpublicNodeFuns = CpublicNodeFuns),
        # A concatenation of lists
        CPUBLIC = c(CpublicNodeFuns, CpublicModelVars, CpublicCtor, CpublicMethods),
        CLASSNAME = classname,
        BASECLASS = baseclass)
  )
  eval(ans)
}

## Get varInfo from new nimbleModel
get_varInfo_from_nimbleModel <- function(model) {
  mDef <- model$modelDef
  extract <- \(x) x |> lapply(\(x) list(name = x$varName, nDim = x$nDim))
  vars <- mDef$varInfo |> extract()
  logProbVars <- mDef$logProbVarInfo |> extract()
  # The resize_from_list method will error out if a scalar is included.
  # The maxs is empty for scalars, so they are automatically omitted from the sizes result here.
  extract_sizes <- \(x) x|> lapply(\(x) x$maxs)
  sizes <- mDef$varInfo |> extract_sizes()
  logProb_sizes <- mDef$logProbVarInfo |> extract_sizes()
  list(
    vars = c(vars, logProbVars),
    sizes = c(sizes, logProb_sizes)
  )
}

# make_stoch_calculate <- function(LHSrep, RHSrep, logProbExprRep) {
#   lenRHS <- length(RHSrep)
#   if(length(RHS) > 1) {
#     RHSrep[3:(lenRHS+1)] <- RHSrep[2:lenRHS]
#     names(RHSrep)[3:(lenRHS+1)] <- names(RHSrep)[2:lenRHS]
#   }
#   RHSrep[[2]] <- LHSrep
#   names(RHSrep)[2] <- ""
#   RHSrep[[lenRHS+2]] <- 1
#   names(RHSrep)[lenRHS+2] <- "log"
#   # We create separate code for R and C execution.
#   calc1Cfun <- substitute(
#     function(idx) {LHS <- RHS; return(LHS)},
#     list(LHS = logProbExprRep, RHS = RHSrep)
#   ) |> eval()
#   make_calculate_from_Cfun(calc1Cfun)
# }

make_stoch_sim_line <- function(LHSrep, RHSrep) {
  BUGSdistName <- safeDeparse(RHSrep[[1]])
  distInfo <- getDistributionInfo(BUGSdistName)
  sim_code <- as.name(distInfo$simulateName)
  if(is.null(sim_code)) stop("Could not find simulation ('r') function for ", BUGSdistName)
  RHSrep[[1]] <- sim_code
  # scoot all named arguments right 1 position
  if(length(RHSrep) > 1) {    
    for(i in (length(RHSrep)+1):3) {
      RHSrep[i] <- RHSrep[i-1]
      names(RHSrep)[i] <- names(RHSrep)[i-1]
    } 
  }    
  RHSrep[[2]] <- 1
  names(RHSrep)[2] <- ''
  sim_line <- substitute(
    LHS <- RHS,
    list(LHS = LHSrep, RHS = RHSrep))
  sim_line
}

make_stoch_calc_line <- function(LHSrep, RHSrep, logProbExprRep, diff = FALSE) {
  lenRHS <- length(RHSrep)
  if(length(RHSrep) > 1) {
    RHSrep[3:(lenRHS+1)] <- RHSrep[2:lenRHS]
    names(RHSrep)[3:(lenRHS+1)] <- names(RHSrep)[2:lenRHS]
  }
  RHSrep[[2]] <- LHSrep
  names(RHSrep)[2] <- ""
  RHSrep[[lenRHS+2]] <- 1
  names(RHSrep)[lenRHS+2] <- "log"
  # We create separate code for R and C execution.
  if(!diff) {
    calc_line <- substitute(
      LHS <- RHS,
      list(LHS = logProbExprRep, RHS = RHSrep))
  } else {
    calc_line <- substitute(
        LocalNewLogProb_ <- RHS,
      list(RHS = RHSrep))
  }
  calc_line
}

make_determ_calc_line <- function(LHSrep, RHSrep) {
  calc_line <- substitute(
    LHS <- RHS,
    list(LHS = LHSrep, RHS = RHSrep))
  calc_line
}

make_nFxn_from_Cfun <- function(Cfun) {
  Rfun <- Cfun
  body(calc1Rfun) <- nm_addModelDollarSign(body(Cfun), exceptionNames = c("idx"))
  nFxn <- nFunction(
    name = "calc_one",
    fun = Rfun,
    compileInfo=list(C_fun=Cfun),
    argTypes = list(idx = 'integerVector'),
    returnType = 'numericScalar')
  #nodeVars <- all.vars(body(calc1Cfun)) |> setdiff("idx")
  nFxn
}

make_node_method_nFxn <- function(f, name, returnType='numericScalar') {
  Cfun <- f
  Rfun <- f
  body(Rfun) <- nm_addModelDollarSign(body(f), exceptionNames = c("idx", "LocalNewLogProb_", "LocalAns_"))
  if(is.null(returnType)) returnType <- 'void'
  nFxn <- nFunction(
    name = name,
    fun = Rfun,
    argTypes = list(idx = 'integerVector'),
    returnType = returnType,
    compileInfo=list(C_fun=Cfun),
  )
  nFxn
}

make_node_methods_from_declInfo <- function(declInfo) {
  # pieces are adapted from Chris' code in nimbleModel and/or old nimble.
  #
  # This function creates a calc_one nFunction that calculates single index case.
  # This will then be used by generic iterator over indices.
  # Vectorized cases can be added in this basic framework later.
  modelCode <- declInfo$calculateCode
  LHS <- modelCode[[2]]
  RHS <- modelCode[[3]]
  type <- if(modelCode[[1]]=="~") "stoch" else "determ" # or use declInfo$stoch (logical)
  context <- declInfo$declRule$context
  replacements <- sapply(seq_along(context$singleContexts),
                         function(i) parse(text = paste0('idx[',i,']'))[[1]])
  names(replacements) <- context$indexVarNames
  LHSrep <- eval(substitute(substitute(e, replacements), list(e = LHS)))
  RHSrep <- eval(substitute(substitute(e, replacements), list(e = RHS)))

  if(type == 'determ') {
    methodList <- eval(substitute(
        list(
            sim_one   = (function(idx) {calc_one(idx)}) |>
              make_node_method_nFxn("sim_one", NULL),
            calc_one  = (function(idx) {DETERMCALC; return(invisible(0))}) |>
              make_node_method_nFxn("calc_one"),
            calcDiff_one = (function(idx) {calc_one(idx);return(invisible(0))}) |> 
              make_node_method_nFxn("calcDiff_one"),
            getLogProb_one = (function(idx) {return(0)}) |>
              make_node_method_nFxn("getLogProb_one")
        ),
        list(DETERMCALC = make_determ_calc_line(LHSrep, RHSrep))
        ))
    }
    if(type == 'stoch') {
      logProbExpr <- declInfo$genLogProbExpr()
      logProbExprRep <- eval(substitute(substitute(e, replacements), list(e = logProbExpr)))
      methodList <- eval(substitute(
        list(
            sim_one   = (function(idx) { STOCHSIM }) |>
              make_node_method_nFxn("sim_one", NULL),
            calc_one  = (function(idx) { STOCHCALC;   return(invisible(LOGPROB)) }) |>
              make_node_method_nFxn("calc_one"),
            calcDiff_one = (function(idx) {STOCHCALC_DIFF; LocalAns_ <- LocalNewLogProb_ - LOGPROB; 
                                           LOGPROB <- LocalNewLogProb_; return(invisible(LocalAns_))}) |>
              make_node_method_nFxn("calcDiff_one"),
            getLogProb_one = (function(idx) { return(LOGPROB) }) |>
              make_node_method_nFxn("getLogProb_one")
        ),
        list( LOGPROB   = logProbExprRep,
              STOCHSIM  = make_stoch_sim_line(LHSrep, RHSrep),
              STOCHCALC = make_stoch_calc_line(LHSrep, RHSrep, logProbExprRep),
              STOCHCALC_DIFF = make_stoch_calc_line(LHSrep, RHSrep, logProbExprRep, diff=TRUE))
      ))
  }
  methodList
}

make_model_from_nimbleModel <- function(m, compile=FALSE) {
  mDef <- m$modelDef
  allVarInfo <- get_varInfo_from_nimbleModel(m)
  modelVarInfo <- allVarInfo$vars
  nodeFxnNames <- character()
  nodeInfoList <- list()
  nodeFxnList <- list()
  # two vectors for canonical use for calculation instructions
  # to move between names and indices of nodeFxns:
  for(i in seq_along(mDef$declInfo)) {
    declInfo <- mDef$declInfo[[i]]
    node_methods <- make_node_methods_from_declInfo(declInfo)
    nodeVars <- node_methods |> lapply(\(x) all.vars(body(x))) |> unlist() |> unique() |> setdiff(c("idx", "LocalNewLogProb_", "LocalAns_", "model")) %||% character()
    nodeVarInfo <- modelVarInfo[nodeVars]
    SLN <- declInfo$sourceLineNumber
    node_classname <- paste0("nodeClass_", SLN) # name of an nClass generator
    node_RvarName <- paste0("nodeFxn_", SLN)    # name of an R variable holding the nClass generator
    node_membername <- paste0("node_", SLN)     # name of model member variable holding an instance of the nClass
    # Currently, we can't just make a list of these but need them as named objects in the environment
    nodeFxnList[[node_RvarName]] <- make_node_nClass(nodeVarInfo, node_methods, node_classname)
    assign(node_RvarName,
      nodeFxnList[[node_RvarName]]
    )
    nodeInfoList[[i]] <- nCompiler:::make_node_info_for_model_nClass(node_membername, node_RvarName, node_classname, nodeVarInfo)
  }
  model <- makeModel_nClass(modelVarInfo, nodeInfoList, classname = "my_model", env = environment())
  if(!compile)
    return(model)
  Cmodel <- nCompile(model)
  return(Cmodel)
}

calcInputList_to_calcInstrList <- function(calcInputList, comp) {
  message("need to set up nodeFxn_2_nodeIndex")
  if(missing(comp))
    stop("comp should be a list returned from nCompile including calcInstr_nClass and nodeInstr_nClass")
  calcInstrList <- vector(length = length(calcInputList), mode='list')
  for(iCalc in seq_along(calcInputList)) {
    calcInstr <- comp$calcInstr_nClass$new()
    calcInput <- calcInputList[[iCalc]]
    calcInstr$nodeIndex <- nodeFxn_2_nodeIndex[ calcInput[[1]] ] #$nodeFxn]
    nodeInputVec <- calcInput[[2]]#$nodeInputVec
    nodeInstrVec <- vector(length=length(nodeInputVec), mode='list')
    for(iMethod in seq_along(nodeInputVec)) {
      nodeInstr <- comp$nodeInstr_nClass$new()
      nodeInput <- nodeInputVec[[iMethod]]
      nodeInstr$methodInstr <- nodeInput[[1]]#$methodInput
      nodeInstr$indsInstrVec <- nodeInput[[2]]#$indsInputVec
      nodeInstrVec[[iMethod]] <- nodeInstr
    }
  calcInstr$nodeInstrVec <- nodeInstrVec
  calcInstrList[[iCalc]] <- calcInstr
  }
  calcInstrListObj <- comp$calcInstrList_nClass$new()
  calcInstrListObj$calcInstrList <- calcInstrList
  return(calcInstrListObj)
}
