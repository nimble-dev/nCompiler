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
  predefined = quote(system.file(file.path("include","nCompiler", "predefined_nClasses"), package="nCompiler") |>
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
  predefined = quote(system.file(file.path("include","nCompiler", "predefined_nClasses"), package="nCompiler") |>
               file.path("calcInstr_nClass")),
  compileInfo=list(interface="full",
                   createFromR = TRUE#,
                   #predefined_output_dir = "calcInstr_nClass"
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
  predefined = quote(system.file(file.path("include","nCompiler", "predefined_nClasses"), package="nCompiler") |>
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
        function(calcInstr='calcInstr_nClass') {cppLiteral('Rprintf("modelBase_nClass calculate (should not see this)\\n");'); return(0)},
        returnType = 'numericScalar',
        compileInfo = list(virtual=TRUE)
    )
  ),
  # See comment above about needing to ensure a virtual destructor
  predefined = quote(system.file(file.path("include","nCompiler", "predefined_nClasses"), package="nCompiler") |> file.path("modelBase_nClass")),
  compileInfo=list(interface="full",
                   createFromR = FALSE,
                   Hincludes = "<nodeFxnBase_nClass_c_.h>")
)

# nCompile(modelBase_nClass, control=list(generate_predefined=TRUE))

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
    type="double", nDim=x$nDim, name="", isRef=TRUE, isConst=FALSE, interface=FALSE) # We could in future make some isConst=TRUE, but it might not matter much
  # varInfo_2_cppVar <- \(x) nCompiler:::symbolCppVar$new(
  #   baseType = nCompiler:::symbolBasic$new(type="double", nDim=x$nDim, name="")$genCppVar()$generate(),
  #   ref=TRUE, const=TRUE)
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

#  This was a prototype
  node_nClass <- substitute(
    nClass(
      classname = CLASSNAME,
      Cpublic = CPUBLIC,
      compileInfo = list(
        createFromR = FALSE, # Without a default constructor (which we've disabled here), createFromR is impossible
        nClass_inherit = list(base = BASECLASS))  # Ideally this line would be obtained from a base nClass, but we insert it directly for now
    ),
    list(CPUBLIC = c(
      list(
        nFunction(
          initFun,
          compileInfo = list(constructor=TRUE, initializers = initializersList)
        )
      ) |> structure(names = classname),
      CpublicVars,
      methods
      ),
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
      function() {setup_node_mgmt()}
    ),
    set_from_list = nFunction(
      name = "set_from_list",
      function(Rlist = 'RcppList') {cppLiteral('modelClass_::set_from_list(Rlist);')}
    ),
    resize_from_list = nFunction(
      name = "resize_from_list",
      function(Rlist = 'RcppList') {cppLiteral('modelClass_::resize_from_list(Rlist);')}
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
  membernames <- node_pieces |> lapply(\(x) x$membername) |> unlist()
  CpublicNodeFuns <- node_pieces |> lapply(\(x) x$nClass_type) |> setNames(membernames)
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
  baseclass <- paste0("modelClass_<", classname, ">")
  ans <- substitute(
    nClass(
      classname = CLASSNAME,
      inherit = modelBase_nClass,
      compileInfo = list(opDefs = OPDEFS,
                         nClass_inherit = list(base=BASECLASS)
                         #inherit = list(base = "public modelClass_<mymodel>"),
                         #Hincludes = "<nCompiler/nClass_interface/post_Rcpp/nCompiler_model_base_devel.h>"
                         ),
      Cpublic = CPUBLIC
    ),
    list(OPDEFS = opDefs,
        CPUBLIC = c(CpublicNodeFuns, CpublicModelVars, CpublicCtor, CpublicMethods),
        CLASSNAME = classname,
        BASECLASS = baseclass)
  )
  eval(ans, envir = parent.frame())
}
