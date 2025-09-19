# Here we are drafting support for new implementation of nimbleModels.
# This should eventually live in a separate package, but for now it is easier to draft here.
#
# see test-nimbleModel too.

modelBase_nClass <- nClass(
  classname = "modelBase_nClass",
  Cpublic = list(
    hw = nFunction(
        name = "hw",
        function() {cppLiteral('Rprintf("modelBase_nClass hw (should not see this)\\n");')},
        compileInfo = list(virtual=TRUE)
    ),
    bye = nFunction(
        name = "bye",
        function() {cppLiteral('Rprintf("modelBase_nClass hw (should not see this)\\n");')},
        compileInfo = list(virtual=TRUE)
    )
  ),
  compileInfo=list(interface="none",
                   createFromR = FALSE)
)

makeModel_nClass <- function(varInfo) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  CpublicModelVars <- varInfo |> lapply(\(x) paste0("numericArray(nDim=",x$nDim,")"))
  names(CpublicModelVars) <- varInfo |> lapply(\(x) x$name) |> unlist()
  opDefs <- list(
    base_hw = getOperatorDef("custom_call"),
    setup_node_mgmt = getOperatorDef("custom_call")
  )
  opDefs$base_hw$returnType <- nCompiler:::argType2symbol(quote(void()))
  opDefs$base_hw$labelAbstractTypes$recurse <- FALSE
  opDefs$setup_node_mgmt$returnType <- nCompiler:::argType2symbol(quote(void()))
  opDefs$setup_node_mgmt$labelAbstractTypes$recurse <- FALSE

  CpublicMethods <- list(
    hw = nFunction(
      name = "hw",
      function() {cppLiteral('Rprintf("hw\\n");')}
    ),
    # base_hw = nFunction(
    #   name = "base_hw",
    #   function() {cppLiteral('modelBaseClass::base_hw();')}
    # ),
    call_base_hw = nFunction(
      name = "call_base_hw",
      function() {base_hw()}
    ),
    call_setup_node_mgmt = nFunction(
      name = "setup_node_mgmt",
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
 #     inherit = modelBase_nClass,
      compileInfo = list(opDefs = OPDEFS,
                        inherit = list(base = "public modelClass_<mymodel>"),
                        Hincludes = "<nCompiler/nClass_interface/post_Rcpp/nCompiler_model_base_devel.h>"),
      Cpublic = CPUBLIC
    ),
    list(OPDEFS = opDefs,
        CPUBLIC = c(CpublicNodeFuns, CpublicModelVars, CpublicCtor, CpublicMethods))
  )
  eval(ans, envir = parent.frame())
}

make_node_fun <- function(varInfo) {
  # varInfo will be a list (names not used) of name, nDim, sizes.
  foo <- \(x) nCompiler:::symbolCppVar$new(baseType = nCompiler:::symbolBasic$new(type="double", nDim=x$nDim, name="")$genCppVar()$generate(),
                                           ref=TRUE, const=TRUE)
  typeList <- varInfo |> lapply(foo)
  names(typeList) <- varInfo |> lapply(\(x) x$name) |> unlist()

#  baseTypeStrings <- varInfo |> lapply(\(x) paste0("numericArray(nDim=",x$nDim,")")) |> unlist()
#  typeStrings <- paste0('CppVar(baseType=argType2Cpp("',baseTypeStrings,'"),ref=TRUE,const=TRUE)')
#  typeList <- typeStrings |> lapply(nMakeType)
#  names(typeList) <- names(varInfo)
  #names(Cpublic) <- varInfo |> lapply(\(x) x$name) |> unlist()
  CpublicVars <- names(typeList) |> lapply(\(x) eval(substitute(quote(T(typeList$NAME)),
                                                    list(NAME=as.name(x)))))
  names(CpublicVars) <- names(typeList)

  ctorArgNames <- paste0(names(typeList), '_')
  initializersList <- paste0(names(typeList), '(', ctorArgNames ,')')
  initFun <- function(){}
  formals(initFun) <- structure(as.pairlist(CpublicVars), names = ctorArgNames)

#  This was a prototype
#  mu_type <- list(a = nMakeType('CppVar(baseType = argType2Cpp("numericVector"), ref=TRUE, const=TRUE)'))
  node_dnorm <- substitute(
    nClass(
      classname = "node_dnorm",
      Cpublic = CPUBLIC,
      compileInfo = list(createFromR = FALSE,
                         inherit = list(base = "public nodeFunctionClass_<node_dnorm>"),
                         Hincludes = "<nCompiler/nClass_interface/post_Rcpp/nCompiler_model_base_devel.h>")
    ),
    list(CPUBLIC = c(
      list(
#      mu2 = quote(T(mu_type$a)),
#      mean = quote(ref('numericScalar')),
      node_dnorm = nFunction(
        initFun, #function(mu_ = T(mu_type$a)) {},
        compileInfo = list(constructor=TRUE, initializers = initializersList) #list('mu2(mu_)'))
      )
      ),
      CpublicVars
      )))
  eval(node_dnorm)
}
#test <- nCompiler:::argType2symbol('CppVar(baseType = argType2Cpp("numericVector"), ref=TRUE, const=TRUE)')
