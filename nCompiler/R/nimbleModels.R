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

# This wrapper to system.file is important because without it
# (if we had system.file directory in the nClass calls below),
# then system.file would be evaluated at build time in the temporary build directory
# and could become hard-coded there and then invalid in the installation
# directory. This triggered an error when R CMD INSTALL was used on
# github actions testing.
get_system_file <- function(...) {
  system.file(..., package = "nCompiler")
}

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
      function(to_do = integer(1)) {return(0); returnType(double())},
      compileInfo = list(virtual=TRUE)
    )
  ),
  # We haven't dealt with ensuring a virtual destructor when any method is virtual
  # For now I did it manually by editing the .h and .cpp
  predefined = get_system_file(file.path("include","nCompiler", "predefined_nClasses")) |> file.path("nodeFxnBase_nClass"),
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
        function(nodes=SEXP()) {cppLiteral('Rprintf("modelBase_nClass calculate (should not see this)\\n");')},
        compileInfo = list(virtual=TRUE)
    )
  ),
  # See comment above about needing to ensure a virtual destructor
  predefined = get_system_file(file.path("include","nCompiler", "predefined_nClasses")) |> file.path("modelBase_nClass"),
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

make_node_fun <- function(varInfo) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  foo <- \(x) nCompiler:::symbolCppVar$new(baseType = nCompiler:::symbolBasic$new(type="double", nDim=x$nDim, name="")$genCppVar()$generate(),
                                           ref=TRUE, const=TRUE)
  typeList <- varInfo |> lapply(foo)
  names(typeList) <- varInfo |> lapply(\(x) x$name) |> unlist()

  CpublicVars <- names(typeList) |> lapply(\(x) eval(substitute(quote(T(typeList$NAME)),
                                                    list(NAME=as.name(x)))))
  names(CpublicVars) <- names(typeList)

  ctorArgNames <- paste0(names(typeList), '_')
  initializersList <- paste0(names(typeList), '(', ctorArgNames ,')')
  initFun <- function(){}
  formals(initFun) <- structure(as.pairlist(CpublicVars), names = ctorArgNames)

#  This was a prototype
  node_dnorm <- substitute(
    nClass(
      classname = "node_dnorm",
      Cpublic = CPUBLIC,
      compileInfo = list(
        createFromR = FALSE, # Without a default constructor (which we've disabled here), createFromR is impossible
        nClass_inherit = list(base = "nodeFxnClass_<node_dnorm>"))
    ),
    list(CPUBLIC = c(
      list(
      node_dnorm = nFunction(
        initFun,
        compileInfo = list(constructor=TRUE, initializers = initializersList)
      )
      ),
      CpublicVars
      )))
  eval(node_dnorm)
}
#test <- nCompiler:::argType2symbol('CppVar(baseType = argType2Cpp("numericVector"), ref=TRUE, const=TRUE)')

makeModel_nClass <- function(varInfo) {
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
  CpublicNodeFuns <- list(
    beta_node = 'node_dnorm()'
  )
  CpublicCtor <- list(
    mymodel = nFunction(
      function(){},
      compileInfo = list(constructor=TRUE,
                         initializers = c('nCpp("beta_node(new node_dnorm(mu, beta, 1))")'))
    )
  )
  ans <- substitute(
    nClass(
      classname = "mymodel",
      inherit = modelBase_nClass,
      compileInfo = list(opDefs = OPDEFS,
                         nClass_inherit = list(base="modelClass_<mymodel>")
                         #inherit = list(base = "public modelClass_<mymodel>"),
                         #Hincludes = "<nCompiler/nClass_interface/post_Rcpp/nCompiler_model_base_devel.h>"
                         ),
      Cpublic = CPUBLIC
    ),
    list(OPDEFS = opDefs,
        CPUBLIC = c(CpublicNodeFuns, CpublicModelVars, CpublicCtor, CpublicMethods))
  )
  eval(ans, envir = parent.frame())
}
