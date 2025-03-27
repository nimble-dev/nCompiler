## Drafting backward compatibility for nimble here.
## Eventually this should live in the nimble package itself.

RCfun_2_nFun <- function(RCfun) {
  if(inherits(RCfun, "nfMethodRC")) nfMethodRCobj <- RCfun
  else nfMethodRCobj <- environment(RCfun)$nfMethodRCobject
  fun <- function() {}
  body(fun) <- nfMethodRCobj$code
  formals(fun) <- nfMethodRCobj$argInfo
  nFun <- nFunction(
    name = nfMethodRCobj$uniqueName, # This might be helpful
    fun = fun,
    returnType = nfMethodRCobj$returnType
  )
  nFun
}

BROWSE_COMPILE_NIMBLE <- FALSE

## nf <- nimbleFunction(
##   setup = function() {x <- 1:2},
##   run = function() {return(x[1]); returnType(double())}
## )

## nf1 <- nf()

## test2 <- nCompile_nimbleFunctionClass(nf1)

## nC1 <- nClass(Cpublic = list(x1 = 'integerVector'))
## test <- nC1$new()

buildCopyFromNimbleFunction <- function(nComp_types_list) {
  buildOneCopyLine <- function(type) {
    paste0('SEXP_to_type(', type$name, ', SdataEnv["',type$name,'"]);')
  }
  copyLines <- nComp_types_list |> lapply(buildOneCopyLine) |> unlist()
#  copyLines <- 'Rprintf("copying\\n");'
  copyfun <- eval(substitute(
    nFunction(
      function(NFobj = 'SEXP') {
        nCpp(c("Rcpp::Environment SdataEnv = get_NF_dataenv(NFobj);",
               COPYLINES))
      }),
    list(COPYLINES=copyLines)
  ))
}

NF_2_nClass <- function(nf) {
  #browser()
  dirName = tempdir()
  projectName <- ''
  project <- nimble:::nimbleProjectClass(dirName, name = projectName)
  generatorName <- nimble:::nfGetDefVar(nf, 'name')
  nfProc <- nimble:::nfProcessing(nf, generatorName, fromModel = FALSE, project = project, isNode = FALSE)
  nfProc$setupTypesForUsingFunction()
  setupSymTab <- nfProc$setupSymTab
  nComp_types_list <- nimbleSymTab_to_nComp_types(setupSymTab)
  origMethods <- nfProc$origMethods
  nComp_methods_list <- origMethods |> lapply(RCfun_2_nFun)
  copyFromNimbleFunction <-
    list(copyFromNF = buildCopyFromNimbleFunction(nComp_types_list))
  nCans <- nClass(
    classname = nfProc$name,
    Cpublic = c(nComp_types_list, nComp_methods_list, copyFromNimbleFunction)
  )
  nCans
}

nCompile_nimbleFunctionClass <- function(nf) {
  #browser()
  nCans <- NF_2_nClass(nf)
  CnCans <- nCompile(nCans)
  obj <- CnCans$new()
  obj$copyFromNF(nf)
  obj
}

nimbleSymTab_to_nComp_types <- function(symTab) {
  symbolNames <- symTab$getSymbolNames()
  result <- list()
  for(sn in symbolNames) {
    obj <- symTab$getSymbolObject(sn)
    symClass <- class(obj)
    if(symClass[length(symClass)] == "symbolBasic") { # numeric, integer, logical
      nDim <- obj$nDim
      scalarType <- obj$type
      result[[sn]] <- nType(name = sn, scalarType=scalarType, nDim=nDim)
    }
  }
  result
}

compileNimble <- function(..., project, dirName = NULL, projectName = '',
                          control = list(),
                          resetFunctions = FALSE,
                          showCompilerOutput = getNimbleOption('showCompilerOutput')) {
  ## 1. Extract compilation items
  reset <- FALSE
  if(BROWSE_COMPILE_NIMBLE) browser()
    ## This pulls out ... arguments, makes names from their expressions if names weren't provided, and combines them with any ... arguments that are lists.
  controlDefaults = list(debug = FALSE, debugCpp = FALSE, compileR = TRUE, writeFiles = TRUE, compileCpp = TRUE, loadSO = TRUE, returnAsList = FALSE)

  dotsDeparses <- unlist(lapply( substitute(list(...))[-1], deparse ))
  origList <- list(...)
  if(is.null(names(origList))) names(origList) <- rep('', length(origList))
  boolNoName <- names(origList)==''
  origIsList <- unlist(lapply(origList, is.list))
  dotsDeparses[origIsList] <- ''
  names(origList)[boolNoName] <- dotsDeparses[boolNoName]
  units <- do.call('c', origList)
  if(any(sapply(units, is, "MCMCconf")))
    stop("You have provided an MCMC configuration object, which cannot be compiled. Instead, use run 'buildMCMC' on the configuration object and compile the resulting MCMC object.")
  unitTypes <- nimble:::getNimbleTypes(units)
  if(length(grep('unknown', unitTypes)) > 0) stop(paste0('Some items provided for compilation do not have types that can be compiled: ', paste0(names(units), collapse = ' '), '.  The types provided were: ', paste0(unitTypes, collapse = ' '), '. Be sure only specialized nimbleFunctions are provided, not nimbleFunction generators.'), call. = FALSE)
  if(is.null(names(units))) names(units) <- rep('', length(units))
  if(length(units) == 0) stop('No objects for compilation provided')

  ## 2. Get project or make new project
  if(missing(project)) {
    if(reset) warning("You requested 'reset = TRUE', but no project was provided.  If you are trying to re-compiled something into the same project, give it as the project argument as well as a compilation item. For example, 'compileNimble(myFunction, project = myFunction, reset = TRUE)'.")
    if(!is.null(getNimbleOption('nimbleProject'))) project <- getNimbleOption('nimbleProject')
    else project <- nimble:::nimbleProjectClass(dirName, name = projectName)

    ## Check for uncompiled models.
    if(!any(sapply(units, is, 'RmodelBaseClass'))) {
      mcmcUnits <- which(sapply(units, class) == "MCMC")
      if(any(sapply(mcmcUnits, function(idx) {
        class(units[[idx]]$model$CobjectInterface) == "uninitializedField"
      })))
        stop("compileNimble: The model associated with an MCMC is not compiled. Please compile the model first.")
    }
  } else {
    project <- getNimbleProject(project, TRUE)
    if(!inherits(project, 'nimbleProjectClass'))
      stop("Invalid project argument; note that models and nimbleFunctions need to be compiled before they can be used to specify a project. Once compiled you can use an R model or nimbleFunction to specify the project.", call. = FALSE)
  }
  if(resetFunctions) project$resetFunctions()

  for(i in names(controlDefaults)) {
    if(!i %in% names(control)) control[[i]] <- controlDefaults[[i]]
  }

  if(!showCompilerOutput) {
    messageIfVerbose("Compiling via nCompiler\n  [Note] This may take a minute.\n  [Note] Use 'showCompilerOutput = TRUE' to see C++ compilation details.")
  }
  if(showCompilerOutput) {
    messageIfVerbose("Compiling via nCompiler\n  [Note] This may take a minute.\n  [Note] On some systems there may be some compiler warnings that can be safely ignored.")
  }

  ans <- list()
  nComp_units <- vector(mode="list", length = length(units))
  rcfUnits <- unitTypes == 'rcf'
  if(sum(rcfUnits) > 0) {
    whichUnits <- which(rcfUnits)
    for(i in whichUnits) {
      if(isTRUE(getNimbleOption("enableDerivs"))) {
        if(!isFALSE(environment(units[[i]])$nfMethodRCobject$buildDerivs))
          stop(paste0("A nimbleFunction without setup code and with buildDerivs = TRUE can't be included\n",
                      "directly in a call to compileNimble.  It can be called by another nimbleFunction and,\n",
                      "in that case, will be automatically compiled."))
      }
      nComp_units[[i]] <- RCfun_2_nFun(units[[i]])
      environment(units[[i]])$nfMethodRCobject[['nimbleProject']] <- project
#      ans[[i]] <- project$compileRCfun(units[[i]], control = control, showCompilerOutput = showCompilerOutput)
#      if(names(units)[i] != '') names(ans)[i] <- names(units)[i]
    }
  }

  nfUnits <- unitTypes == 'nf'
  if(sum(nfUnits) > 0) {
    whichUnits <- which(nfUnits)
    if(length(whichUnits)>1) message("Still need to check for multiple objects of the same nimbleFunction class.")
    for(i in whichUnits) {
      nComp_units[[i]] <- NF_2_nClass(units[[i]])
    }
    #nfAns <- project$compileNimbleFunctionMulti(units[whichUnits], control = control,
    #                                            reset = reset, showCompilerOutput = showCompilerOutput)
    #ans[whichUnits] <- nfAns
    #for(i in whichUnits) if(names(units)[i] != '') names(ans)[i] <- names(units)[i]
  }

  names(nComp_units) <- names(units)
  ans <- do.call(nCompile, nComp_units)
  if(sum(nfUnits) > 0) {
    whichUnits <- which(nfUnits)
    for(i in whichUnits) {
      obj <- if(is.list(ans)) ans[[i]]$new() else ans$new()
      obj$copyFromNF(units[[i]])
      if(is.list(ans))
        ans[[i]] <- obj
      else
        ans <- obj # Add checking that there is one and only one unit.
    }
  }
  ans
}
