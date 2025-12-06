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
               file.path("nodeInstr_nClass")),
  compileInfo=list(interface="full",
                   createFromR = TRUE#,
                   #predefined_output_dir = "nodeInstr_nClass"
                   )
)

calcInstr_nClass <- nClass(
  classname = "calcInstr_nClass",
  Cpublic = list(
    nodeIndex = 'integerScalar',
    nodeInstrVec = "nList('nodeInstr_nClass')"
  ),
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |>
               file.path("calcInstr_nClass")),
  compileInfo=list(interface="full",
                   createFromR = TRUE,
                   Hincludes = "<nodeInstr_nClass_c_.h>"
                   #predefined_output_dir = "calcInstr_nClass"
                   )
)

calcInstrList_nC <- nClass(
  classname = "calcInstrList_nC",
  Cpublic = list(
    calcInstrList = "nList('calcInstr_nClass')"
  ),
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |>
               file.path("calcInstrList_nC")),
  compileInfo=list(interface="full",
                   createFromR = TRUE,
                   Hincludes = "<calcInstr_nClass_c_.h>")
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
               file.path("nodeFxnBase_nClass")),
  compileInfo=list(interface="full",
                   createFromR = FALSE)
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
        function(calcInstrList) {cat("In uncompiled calculate\n")},
        returnType = 'numericScalar',
        compileInfo = list(
          C_fun = function(calcInstrList='calcInstrList_nC') {
            cppLiteral('Rprintf("modelBase_nClass calculate (should not see this)\\n");'); return(0)},
          virtual=TRUE
        )
    )  
  ),
  # See comment above about needing to ensure a virtual destructor
  predefined = quote(system.file(file.path("include","nCompiler", "predef"), package="nCompiler") |> file.path("modelBase_nClass")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   Hincludes = c("<nodeFxnBase_nClass_c_.h>", "<calcInstrList_nC_c_.h>"))
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
make_node_fun <- function(varInfo = list(),
                          methods = list(),
                          classname) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  varInfo_2_cppVar <- \(x) nCompiler:::symbolBasic$new(
    type="double", nDim=x$nDim, name="", isRef=TRUE, isConst=FALSE, interface=FALSE) # In future maybe isConst=TRUE, but it might not matter much
  typeList <- varInfo |> lapply(varInfo_2_cppVar)
  names(typeList) <- varInfo |> lapply(\(x) x$name) |> unlist()

  CpublicVars <- names(typeList) |> lapply(\(x) eval(substitute(quote(T(typeList$NAME)),
                                                    list(NAME=as.name(x)))))
  names(CpublicVars) <- names(typeList)

  ctorArgNames <- paste0(names(typeList), '_')
  initializersList <- paste0(names(typeList), '(', ctorArgNames ,')')
  initFun <- function(){}
  formals(initFun) <- structure(as.pairlist(CpublicVars), names = ctorArgNames)

  if(missing(classname))
    classname <- nodeFxnLabelCreator()

  baseclass <- paste0("nodeFxnClass_<", classname, ">")

  setModel <- function(model) {
    if(!isCompiled())
      self$model <- model
    else
       warning("setModel called on compiled object; no action taken")
  }

#  This was a prototype
  node_nClass <- substitute(
    nClass(
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

make_node_info <- function(membername,
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
                             classname
                             ) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  CpublicModelVars <- varInfo |> lapply(\(x) paste0("numericArray(nDim=",x$nDim,")"))
  names(CpublicModelVars) <- varInfo |> lapply(\(x) x$name) |> unlist()
  opDefs <- list(
    base_ping = getOperatorDef("custom_call"),
    setup_node_mgmt = getOperatorDef("custom_call")
  )
  opDefs$base_ping$returnType <- nCompiler:::argType2symbol(quote(void()))
  opDefs$base_ping$labelAbstractTypes$recurse <- FALSE
  opDefs$setup_node_mgmt$returnType <- nCompiler:::argType2symbol(quote(void()))
  opDefs$setup_node_mgmt$labelAbstractTypes$recurse <- FALSE

  if(missing(classname))
    classname <- modelLabelCreator()

  CpublicMethods <- list(
    do_setup_node_mgmt = nFunction(
      name = "call_setup_node_mgmt",
      function() {},
      compileInfo=list(
        C_fun = function() {setup_node_mgmt()})
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
    nClass_type <- paste0(x$nodeFxnName, "()")
    init_string <- paste0('nCpp("', x$membername, '( new ', x$classname, '(',
                                    paste0(x$ctorArgs, collapse=","), '))")')
    list(nClass_type = nClass_type,
         init_string = init_string,
         membername = x$membername)
  })
  nodeObjNames <- node_pieces |> lapply(\(x) x$membername) |> unlist()
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
  initialize <- function(sizes, inits) {
    browser()
    if(isCompiled())
      self$do_setup_node_mgmt()
    if(!isCompiled()) {
      for(nodeObj in self$nodeObjNames) {
        self[[nodeObj]] <- CpublicNodeFuns[[nodeObj]]$new()
        self[[nodeObj]]$setModel(self)
      }
    }
    if(length(inits)) init_from_list(inits)
    else if(length(sizes)) resize_from_list(sizes)
  }
  baseclass <- paste0("modelClass_<", classname, ">")
  env <- new.env(parent = parent.frame())
  env$CpublicNodeFuns <- CpublicNodeFuns
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
        RPUBLIC = list(initialize=initialize, nodeObjNames = nodeObjNames),
        CPUBLIC = c(CpublicNodeFuns, CpublicModelVars, CpublicCtor, CpublicMethods),
        CLASSNAME = classname,
        BASECLASS = baseclass)
  )
  eval(ans)
}

## Get varInfo from new nimbleModel
get_varInfo_from_nimbleModel <- function(model) {
  mDef <- m$modelDef
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

make_nodeFxn_from_declInfo <- function(declInfo) {
  modelCode <- declInfo$calculateCode
  LHS <- modelCode[[2]]
  RHS <- modelCode[[3]]
  type <- if(modelCode[[1]]=="~") "stoch" else "determ" # or use declInfo$stoch (logical)
  logProbExpr <- declInfo$genLogProbExpr()
  context <- declInfo$declRule$context
  replacements <- sapply(seq_along(context$singleContexts),
                         function(i) parse(text = paste0('idx[',i,']'))[[1]])
  names(replacements) <- context$indexVarNames
  LHSrep <- eval(substitute(substitute(e, replacements), list(e = LHS)))
  RHSrep <- eval(substitute(substitute(e, replacements), list(e = RHS)))
  logProbExprRep <- eval(substitute(substitute(e, replacements), list(e = logProbExpr)))
  lenRHS <- length(RHSrep)
  if(length(RHS) > 1) {
    RHSrep[3:(lenRHS+1)] <- RHSrep[2:lenRHS]
    names(RHSrep)[3:(lenRHS+1)] <- names(RHSrep)[2:lenRHS]
  }
  RHSrep[[2]] <- LHSrep
  names(RHSrep)[2] <- ""
  RHSrep[[lenRHS+2]] <- 1
  names(RHSrep)[lenRHS+2] <- "log"
  calc1Cfun <- substitute(
    function(idx) {LHS <- RHS; return(LHS)},
    list(LHS = logProbExprRep, RHS = RHSrep)
  ) |> eval()
  calc1Rfun <- calc1Cfun
  body(calc1Rfun) <- nm_addModelDollarSign(body(calc1Cfun), exceptionNames = c("idx"))
  calc_one <- nFunction(
    name = "calc_one",
    fun = calc1Rfun,
    compileInfo=list(C_fun=calc1Cfun),
    argTypes = list(idx = 'integerVector'),
    returnType = 'numericScalar')
  nodeVars <- all.vars(body(calc1Cfun)) |> setdiff("idx")
  list(calc_one = calc_one, nodeVars = nodeVars)
}

make_model_from_nimbleModel <- function(m, compile=TRUE) {
  mDef <- m$modelDef
  allVarInfo <- get_varInfo_from_nimbleModel(m)
  modelVarInfo <- allVarInfo$vars
  nodeFxnNames <- character()
  nodeInfoList <- list()
  nodeFxnList <- list()
  for(i in seq_along(mDef$declInfo)) {
    declInfo <- mDef$declInfo[[i]]
    nodeFxn <- make_nodeFxn_from_declInfo(declInfo)
    nodeVars <- nodeFxn$nodeVars
    calc_one <- nodeFxn$calc_one
    SLN <- declInfo$sourceLineNumber
    node_classname <- paste0("nodeClass_", SLN)
    nodeFxnName <- paste0("nodeFxn_", SLN)
    node_membername <- paste0("node_", SLN)
    nodeVarInfo <- modelVarInfo[nodeVars]
    # Currently, we can't just make a list of these but need them as named objects in the environment
    nodeFxnList[[nodeFxnName]] <- make_node_fun(nodeVarInfo, list(calc_one=calc_one), node_classname)
    assign(nodeFxnName,
      nodeFxnList[[nodeFxnName]]
    )
    nodeInfoList[[i]] <- nCompiler:::make_node_info(node_membername, nodeFxnName, node_classname, nodeVarInfo)
#    nodeFxnNames <- c(nodeFxnNames, nodeFxnName)

  }
  model <- makeModel_nClass(modelVarInfo, nodeInfoList, classname = "my_model")
  # Currently we must compile from here because here is where we know the nodeFxnName[s].
  # We have a situation where order matters: model needs to come after the utility classes. Fix me.
  if(!compile)
    return(model)
  argList <- list("modelBase_nClass", "nodeFxnBase_nClass", "calcInstrList_nC", "calcInstr_nClass", "nodeInstr_nClass", "model")
  argList <- c(argList, "nodeFxnList")
  argList <- argList |> lapply(as.name)
  Cmodel <- do.call("nCompile", argList)
  #Cncm1 <- nCompile(modelBase_nClass, nodeFxnBase_nClass, calcInstr_nClass, nodeInstr_nClass, ncm1, nodeFxn_3)
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
  calcInstrListObj <- comp$calcInstrList_nC$new()
  calcInstrListObj$calcInstrList <- calcInstrList
  return(calcInstrListObj)
}
