## Status as of 11/1/18:
## These functions "work" as called from cppDefs_nClass in the sense that they execute without error.
## But they have not been checked for validity of the result.  At the time when they were adapted from
## the old system, not all the necessary pieces of C++ output generation were in place.

## Convert one symbol object for a C++ var into a symbol object for C++ templated CppAD code
# See symbolTable2templateTypeSymbolTable
cppVarSym2templateTypeCppVarSym <- function(oldSym, 
                                            addRef = FALSE, 
                                            clearRef = FALSE, 
                                            replacementBaseType = 'TYPE_',
                                            replacementTemplateArgs = list()) {
  if(oldSym$baseType == 'double') {
    newSym <- cppVarFullClass$new(name = oldSym$name,
                                  baseType = replacementBaseType, 
                                  ref = addRef,
                                  templateArgs = replacementTemplateArgs)
    return(newSym)
  }

  newSym <- oldSym$clone(deep = TRUE)
  if(newSym$baseType == 'Eigen::Tensor') {
      if(newSym$templateArgs[[1]] == 'double') {
          if(length(replacementTemplateArgs)==0)
              newSym$templateArgs[[1]] <- replacementBaseType
          else
              newSym$templateArgs[[1]] <- cppVarFullClass$new(name='',
                                                              baseType = replacementBaseType, 
                                                              templateArgs = replacementTemplateArgs)
          if(clearRef) ## unclear if this is needed in new system
              newSym$ref <- FALSE
      }
  }
  newSym
  ##Need to deal with Eigen types  
}

## Convert a symbol table for C++ vars into a symbol table for C++ for templated CppAD code
## For CppAD, we wrap C++ code in template<class TYPE_> 
## and replace any double with TYPE_
## This includes Eigen templated types.
symbolTable2templateTypeSymbolTable <- function(symTab,
                                                addRef = FALSE,
                                                clearRef = FALSE,
                                                replacementBaseType = 'TYPE_',
                                                replacementTemplateArgs = list()) {
  newSymTab <- symbolTableClass$new()
  symNames <- symTab$getSymbolNames()
  for(sn in symNames) {
    oldSym <- symTab$getSymbol(sn)
    newSym <- cppVarSym2templateTypeCppVarSym(oldSym,
                                              addRef = addRef,
                                              clearRef = clearRef,
                                              replacementBaseType = replacementBaseType,
                                              replacementTemplateArgs = replacementTemplateArgs)
    newSymTab$addSymbol(newSym)
  }
  newSymTab
}

## This makes a Cpp function definition object wrapped in template<class TYPE_> and with
## doubles converted to TYPE_s (including in templated use if NimArr and Eigen).
## This is called from an existing version of the cppFunctionDef and returns a separate one
makeTypeTemplateFunction = function(newName, self) {
  newCppFunDef <- cpp_nFunctionClass$new(name = newName,
                                             static = TRUE)
  ## use typedefs to change.nCompiler's general typedefs for Eigen locally
  typeDefs <- symbolTableClass$new()
  ## Need to add here replacement of Eigen<double> types with Eigen<AD<double> > 
  newCppFunDef$name <- newName
  newCppFunDef$template <- cppTemplateDeclaration('TYPE_')
  newCppFunDef$args <- symbolTable2templateTypeSymbolTable(self$args, addRef = TRUE)
  localArgs <- symbolTable2templateTypeSymbolTable(self$code$symbolTable)
  newCppFunDef$returnType <- cppVarSym2templateTypeCppVarSym(self$returnType)
  newCppFunDef$code <- cppCodeBlockClass$new(code = self$code$code, 
                                    symbolTable = localArgs,
                                    typeDefs = typeDefs, 
                                    generatorSymTab = self$code$symbolTable,
                                    cppADCode = 2L)
  newCppFunDef
}

## This makes the function to be called once for CppAD taping
## It sets up AD variables, copies from regular variables into them
## calls the templated version of the member function
## copies the results back out.
## Not that values in the regular variables are not really important during taping.
## Currently those values are intialized to 0.5, which should satisfy needs for (-inf, inf), [0, inf) and [0, 1].
## Ending the tape is not done here.  That is done from the calling function
## (which is in permanent C++, not generated from R)
## We do not assume that in the target function the arguments are independent variables and the
## returned value is the dependent variable.  Those are set by the independentVarNames and dependentVarNames
makeADtapingFunction <- function(newFunName = 'callForADtaping',
                                 targetFunDef, 
                                 ADfunName, 
                                 independentVarNames, 
                                 dependentVarNames, 
                                 isNode, 
                                 allFunDefs, 
                                 className = "className") {
  ## Make new function definition to call for taping (CFT)
  CFT <- cppFunctionClass$new(name = newFunName,
                              static = TRUE)
  CFT$returnType <- cppADFun(name = 'RETURN_TAPE_',
                             ptr = 1)

  ## args will always be same; these do not depend on case.  actually now these will be empty.
  CFT$args <- symbolTableClass$new()
  ## create vector< CppAD::AD<double> > ADindependentVars
  
  ADindependentVarsSym <- cppVectorOfCppADdouble(name = 'ADindependentVars') ## was ref = TRUE if taking as argument
  ## create vector< CppAD::AD<double> ADresponseVars
  ADresponseVarsSym <- cppVectorOfCppADdouble(name = 'ADresponseVars') ## ditto
  ## Add them to arguments symbol table ## switch design and make these local
  ##    CFT$args$addSymbol( ADindependentVarsSym )
  ##    CFT$args$addSymbol( ADresponseVarsSym )
  ## Make local AD variables for all function inputs and outputs
  ## e.g. if the original targetFun takes NimArr<1, double>, it's templated CppAD version will take NimArr<1, TYPE_>
  ## Next line creates local variables for passing to that templated CppAD version
  localVars <- symbolTable2templateTypeSymbolTable(targetFunDef$args, 
                                                   clearRef = TRUE,
                                                   replacementBaseType = 'CppAD::AD',
                                                   replacementTemplateArgs = list('double'))
   if(isNode){
    localVars$removeSymbol('ARG1_INDEXEDNODEINFO__')
    indexNodeInfoSymbol <- symbolInternalType(name = 'ARG1_INDEXEDNODEINFO__', argList = list('indexedNodeInfoClass'))
  }
  
  ## and similar for the return variable
  initADptrCode <- cppLiteral("RETURN_TAPE_ = new CppAD::ADFun<double>;")
  ansSym <- cppVarSym2templateTypeCppVarSym(targetFunDef$returnType,
                                            clearRef = TRUE, 
                                            replacementBaseType = 'CppAD::AD', 
                                            replacementTemplateArgs = list('double'))
  ansSym$name <- 'ANS_'
  localVars$addSymbol(ansSym)
  symNames <- localVars$getSymbolNames()
  ## set up a set of index variables for copying code, up to six to be arbitrary (allowing up to 6-dimensional.nCompiler objects to be handled)
  indexVarNames <- paste0(letters[9:14],'_')
  ## set any sizes, which must be known
 .nCompilerSymTab <- targetFunDef$NF_Compiler$symbolTable ##targetFunDef$RCfunProc$compileInfo$newLocalSymTab
  
  ## This creates lines like setSize(z, 2 3)
  ## which the C++ output generator turns into something like z.resize(2, 3)
  setSizeLines <- vector('list', length(symNames) + 2) ## extra 2 are for the ADindependentVars and ADresponseVars
  iNextLine <- 1
  
  for(iSym in seq_along(symNames)) {
    thisSymName <- symNames[iSym]
    if(thisSymName == 'ANS_') {
      thisSym <- targetFunDef$NF_Compiler$returnSymbol ##targetFunDef$RCfunProc$compileInfo$returnSymbol
    } else {
      thisSym <-.nCompilerSymTab$getSymbol(thisSymName)
    }
    if(thisSym$nDim > 0) {
      setSizeCall <- do.call('call',
                             c(list('.method',
                                    quote(as.name(thisSymName)),
                                    quote(as.name('resize'))),
                               as.list(thisSym$size)))
      setSizeLines[[iNextLine]] <- setSizeCall ##RparseTree2ExprClasses(setSizeCall)
      iNextLine <- iNextLine + 1
    } else {
      setSizeLines[[iNextLine]] <- NULL
    }
  }
  
  localVars$addSymbol( ADindependentVarsSym )
  localVars$addSymbol( ADresponseVarsSym )
  localVars$addSymbol( CFT$returnType )
  
  ## call CppAD::Independent(ADindependentVars)
  ## This starts CppADs taping system
  CppADindependentCode <- quote(`CppAD::Independent`(ADindependentVars)) #.nCompiler:::RparseTree2ExprClasses(quote(`CppAD::Independent`(ADindependentVars)))
  
  ## make copying blocks into independent vars
  ## This looks like e.g.
  ## for(i_ in 1:3) {ADindependentVars[netIncrement_] = x[i]; netIncrement_ <- netIncrement + 1;}
  numIndependentVars <- length(independentVarNames)
  copyIntoIndepVarCode <- vector('list', numIndependentVars+1)
  ## create the netIncrement_ variable and code to initialize it to 1
  localVars$addSymbol( cppInt(name = 'netIncrement_'))
  copyIntoIndepVarCode[[1]] <- quote(netIncrement_ <- 1) 
  ## getting the sizes is going to be trickier when an independent var is really an expression, in particular with indexing, like model$x[3]
  ## for now let's assume only cleanly defined vars.
  ## one approach would be intermediate variables
  totalIndependentLength <- 0
  maxSize <- 1
  for(ivn in seq_along(independentVarNames)) {
    thisName <- independentVarNames[ivn]
    thisSym <-.nCompilerSymTab$getSymbol(thisName)
    if(thisSym$nDim > 0) {
      thisSizes <- thisSym$size
      sizeList <- lapply(thisSizes, function(x) c(1, x))
      names(sizeList) <- indexVarNames[1:length(sizeList)]
      if(length(sizeList) > maxSize) 
        maxSize <- length(sizeList)
      newRcode <- makeCopyingCodeBlock(as.name(thisName), 
                                       quote(ADindependentVars), 
                                       sizeList, 
                                       indicesRHS = FALSE, 
                                       incrementIndex = quote(netIncrement_), 
                                       isNode)
      copyIntoIndepVarCode[[ivn+1]] <- newRcode 
      totalIndependentLength <- totalIndependentLength + prod(thisSizes)
    } else {
      copyIntoIndepVarCode[[ivn+1]] <- 
        substitute({
          LHS <- ADindependentVars[netIncrement_]; 
          netIncrement_ <- netIncrement_ + 1
        },
        list(LHS = as.name(thisName))
        ) 
      totalIndependentLength <- totalIndependentLength + 1
    }
  }
  
  ## put dummy values in ADindependentVars
  dummyValueRcode <-
    substitute(for(III in 1:TOTLENGTH) 
      ADindependentVars[III] = 1,
      list(III = as.name(indexVarNames[1]),
           TOTLENGTH = totalIndependentLength))
  
  if(isNode){
    dummyIndexNodeInfoCode <-
      list(cppLiteral('indexedNodeInfo ARG1_INDEXEDNODEINFO__ = generateDummyIndexedNodeInfo();'))
  }
  else   
    dummyIndexNodeInfoCode <- list()
  ## call the taping function
  TCFcall <- do.call('call',
                     c(list(ADfunName), 
                       lapply(targetFunDef$args$getSymbolNames(), 
                              as.name)),
                     quote = TRUE)
  tapingCallRCode <- substitute(ANS_ <- TCF, 
                                list(TCF = TCFcall))
  
  ## make copying blocks from dependent vars
  numDependentVars <- length(dependentVarNames)
  copyFromDepVarCode <- vector('list', numDependentVars+1)
  copyFromDepVarCode[[1]] <- quote(netIncrement_ <- 1) 
  totalDepLength <- 0;
  for(ivn in seq_along(dependentVarNames)) {
    thisName <- dependentVarNames[ivn]
    if(thisName == 'ANS_') {
      thisSym <- targetFunDef$NF_Compiler$returnSymbol ##targetFunDef$RCfunProc$compileInfo$returnSymbol
    } else {
      thisSym <-.nCompilerSymTab$getSymbol(thisName)
    }
    if(thisSym$nDim > 0) {
      thisSizes <- thisSym$size
      sizeList <- lapply(thisSizes, function(x) c(1, x))
      names(sizeList) <- indexVarNames[1:length(sizeList)]
      if(length(sizeList) > maxSize) 
        maxSize <- length(sizeList)
      newRcode <- makeCopyingCodeBlock(quote(ADresponseVars),
                                       as.name(thisName),
                                       sizeList, 
                                       indicesRHS = TRUE, 
                                       incrementIndex = quote(netIncrement_))
      copyFromDepVarCode[[ivn+1]] <- newRcode 
      totalDepLength <- totalDepLength + prod(thisSizes)
    } else {
      copyFromDepVarCode[[ivn+1]] <-
        substitute({
          ADresponseVars[netIncrement_] <- RHS
          netIncrement_ <- netIncrement_ + 1
        },
        list(RHS = as.name(thisName))) 
      totalDepLength <- totalDepLength + 1
    }
  }
  
  for(ivn in 1:maxSize)
    localVars$addSymbol( cppInt(name = indexVarNames[ivn]))

  ## Now that we know how big ADindependenVars and ADresponseVars should be, 
  ## we can make two more entries to setSizeCalls for them
  ## Note that code for these will appear above code that uses them.
  setSizeLines[[iNextLine]] <- substitute(.method(ADindependentVars, resize, TIL),
                                          list(TIL = totalIndependentLength))
  iNextLine <- iNextLine + 1
  setSizeLines[[iNextLine]] <- substitute(.method(ADresponseVars, resize, TDL),
                                          list(TDL = totalDepLength))
  
  ## line to finish taping
  finishTapingCall <- cppLiteral('RETURN_TAPE_->Dependent(ADindependentVars, ADresponseVars);')
  
  ADoptimizeCalls <- list(
    # cppLiteral(paste0("std::cout<<\"about to optimize for ", className,"\"<<std::endl;")),
    # cppLiteral("std::cout<<\"size before optimize = \"<< RETURN_TAPE_->size_var() <<\"\\n\";"),
    cppLiteral("RETURN_TAPE_->optimize();"))
  #                     cppLiteral("std::cout<<\"size after optimize = \"<< RETURN_TAPE_->size_var() <<\"\\n\";"))
  
  returnCall <- cppLiteral("return(RETURN_TAPE_);")
  
  ## Finally put together all the code, parse it into the.nCompiler exprClass system,
  ## and add it to the result (CFT)
  allRcode <- do.call('call', 
                      c(list('{'), 
                        setSizeLines, 
                        dummyIndexNodeInfoCode, 
                        list(initADptrCode, dummyValueRcode, CppADindependentCode),
                        copyIntoIndepVarCode, 
                        list(tapingCallRCode),
                        copyFromDepVarCode, 
                        list(finishTapingCall),
                        ADoptimizeCalls, 
                        list(returnCall)),
                      quote=TRUE)
  allCode <- nParse(allRcode)
  CFT$code <- cppCodeBlockClass$new(code = allCode,
                                    symbolTable = localVars)
  CFT
}

## Generate a block of code for copying to or from CppAD objects, to or from original C++ objects
## On the CppAD side, we are always flattening to 1D.
##
## The code this generates is embedded in the ADtapingFunction made by makeADtapingFunction
##
## Note this does some work similar to BUGScontextClass::embedCodeInForLoop
makeCopyingCodeBlock <- function(LHSvar, 
                                 RHSvar, 
                                 indexList, 
                                 indicesRHS = TRUE,
                                 incrementIndex, 
                                 isNode = FALSE) {
  indexNames <- names(indexList)
  indexedBracketExpr <- do.call('call', c(list('[', as.name('TO_BE_REPLACED')),
                                          lapply(rev(indexNames), as.name)), ## use rev() to force column-major order for results
                                quote = TRUE)
  if(indicesRHS) {
    if(isNode)
      RHS <- eval(
        substitute(
          substitute(
            indexedBracketExpr, 
            list(TO_BE_REPLACED = cppLiteral(paste0('(**model_', deparse(RHSvar), ')')))),
          list(indexedBracketExpr = indexedBracketExpr)
        ))
    else 
      RHS <- eval(
        substitute(
          substitute(
            indexedBracketExpr, 
            list(TO_BE_REPLACED = RHSvar)),
          list(indexedBracketExpr = indexedBracketExpr)
        ))
    LHS <- substitute(A[i], 
                      list(A = LHSvar,
                           i = incrementIndex))
  } else {
    LHS <- eval(
      substitute(
        substitute(
          indexedBracketExpr, 
          list(TO_BE_REPLACED = LHSvar)),
        list(indexedBracketExpr = indexedBracketExpr)
      ))
    RHS <- substitute(A[i],
                      list(A = RHSvar, 
                           i = incrementIndex))
  }
  innerCode <- substitute(
    {
      LHS <- RHS
      incrementIndex <- incrementIndex + 1
    },
    list(LHS = LHS,
         RHS = RHS,
         incrementIndex = incrementIndex))
  for(i in length(indexList):1) {
    newForLoop <- 
      substitute(
        for(NEWINDEX_ in NEWSTART_:NEWEND_) INNERCODE, 
        list(NEWINDEX_ = as.name(indexNames[i]),
             NEWSTART_ = rev(indexList)[[i]][[1]],
             NEWEND_ = rev(indexList)[[i]][[2]],
             INNERCODE = innerCode))
    innerCode <- newForLoop
  }
  innerCode
}


makeADargumentTransferFunction <- function(newFunName = 'arguments2cppad',
                                           targetFunDef, 
                                           independentVarNames, 
                                           funIndex = 0,
                                         ##  parentsSizeAndDims, ## was relevant to nodeFun
                                           ADconstantsInfo) {
  ## modeled closely parts of /*  */
  ## needs to set the ADtapePtr to one element of the ADtape
  TF <- cpp_nFunctionClass$new() ## should it be cppFunctionClass?
  TF$returnType <- cppADinfo(name = 'RETURN_OBJ', 
                             ref = TRUE)
  TF$name <- newFunName
  localVars <- symbolTableClass$new()
##  isNode <- !inherits(parentsSizeAndDims, 'uninitializedField')
  isNode <- FALSE
  if(!isNode)
    TF$args <- targetFunDef$args
  else{
    TF$args <- symbolTable()
    indexNodeInfoSym <- targetFunDef$args$getSymbol('ARG1_INDEXEDNODEINFO__')
    # indexNodeInfoSym$name <-'ARG1_INDEXEDNODEINFO__' ## to conform with original R function indexing
    TF$args$addSymbol(indexNodeInfoSym)   
  }
  
  ## set up index vars (up to 6)
  indexVarNames <- paste0(letters[9:14],'_')
 .nCompilerSymTab <- targetFunDef$NF_Compiler$symbolTable #targetFunDef$RCfunProc$compileInfo$newLocalSymTab
  
  ## assign tape ptr code
  assignTapePtrCode <- substitute(
    .member(ADtapeSetup, ADtape) <- allADtapePtrs_[FUNINDEX],
    list(FUNINDEX = funIndex)
  ) ## This will have to become a unique index in general. -1 added during output
  
  ## create code to copy from arguments into the independentVars
  numIndependentVars <- length(independentVarNames)
  copyIntoIndepVarCode <- vector('list', numIndependentVars+1)
  ## create the netIncrement_ variable and code to initialize it to 1
  localVars$addSymbol( cppInt(name = 'netIncrement_'))
  copyIntoIndepVarCode[[1]] <- quote(netIncrement_ <- 1) 
  totalIndependentLength <- 0
  subArgIndexedInfo <- function(x){
    if(deparse(x[[1]])== 'getNodeFunctionIndexedInfo'){
      x[[2]] <- parse(text = "ARG1_INDEXEDNODEINFO__")[[1]]
    }
    return(deparse(x))
  }
  maxSize <- 0
  for(ivn in seq_along(independentVarNames)) {
    thisName <- independentVarNames[ivn]
    thisSym <-.nCompilerSymTab$getSymbol(thisName)
    if(isNode){ # FALSE for now
      nameSubList <- targetFunDef$NC_Compiler$nameSubList
      thisName <- names(nameSubList)[sapply(nameSubList, 
                                            function(x) 
                                              return(as.character(x) == thisName))]
      thisModelElementNum <- as.numeric(gsub(".*([0-9]+)$", "\\1", thisName)) ## Extract 1, 2, etc. from end of arg name.
      thisName <- sub("_[0-9]+$", "", thisName)
      thisModelName <- paste0('model_', Rname2CppName(thisName)) ## Add model_ at beginning and remove _1, _2, etc. at end of arg name.
      thisSizeAndDims <- parentsSizeAndDims[[thisName]][[thisModelElementNum]]
      if(is.null(thisSizeAndDims)){
        thisConstInfo <- ADconstantsInfo[[thisName]][[thisModelElementNum]]
        copyIntoIndepVarCode[[ivn+1]] <- substitute({.member(ADtapeSetup, independentVars)[netIncrement_] <- ARG1_INDEXEDNODEINFO__.info[INT]; netIncrement_ <- netIncrement_ + 1}, list(INT = thisConstInfo$indexColumn)) 
        totalIndependentLength <- totalIndependentLength + 1
        next
      }
    }
    if(thisSym$nDim > 0) {
      thisSizes <- thisSym$size
      if(isNode){ # FALSE for now
        sizeList <- list()
        for(i in 1:length(thisSizeAndDims$lengths)){
          if(thisSizeAndDims$lengths[i] == 1){
            if(deparse(thisSizeAndDims$indexExpr[[i]][[1]]) == 'getNodeFunctionIndexedInfo'){
              thisSizeAndDims$indexExpr[[i]][[1]] <- parse(text = paste0(
                'ARG1_INDEXEDNODEINFO__.info[', thisSizeAndDims$indexExpr[[i]][[3]], ']'))[[1]]
            }
            sizeList[[i]] <-  list(thisSizeAndDims$indexExpr[[i]][[1]], thisSizeAndDims$indexExpr[[i]][[1]])
          }
          else{
            sizeList[[i]] <-  list(thisSizeAndDims$indexExpr[[i]][[1]], thisSizeAndDims$indexExpr[[i]][[2]])
          }
        }
      }
      else{
        sizeList <- lapply(thisSizes, 
                           function(x) list(1, x))
      }
      names(sizeList) <- indexVarNames[1:length(sizeList)]
      if(length(sizeList) > maxSize) 
        maxSize <- length(sizeList)
      newRcode <- makeCopyingCodeBlock(
        quote(.member(ADtapeSetup, independentVars)),
        as.name(thisName),
        sizeList,
        indicesRHS = TRUE,
        incrementIndex = quote(netIncrement_),
        isNode)
      copyIntoIndepVarCode[[ivn+1]] <- newRcode 
      totalIndependentLength <- totalIndependentLength + prod(thisSizes)
    } 
    else {
      if(isNode){ ## FALSE
        indexBracketInfo <- paste0('[', paste0(sapply(parentsSizeAndDims[[thisName]][[thisModelElementNum]]$indexExpr,
                                                      subArgIndexedInfo), collapse = ', '),']')
        indexName <- paste0("cppLiteral('(**", thisModelName, ")')", indexBracketInfo)
        RHS <- parse(text = substitute(INDEXNAME, list(INDEXNAME = as.name(indexName))))[[1]]
      }
      else{
        RHS <- as.name(thisName)
      } 
      copyIntoIndepVarCode[[ivn+1]] <-
        substitute(
          {
            .member(ADtapeSetup,
                       independentVars)[netIncrement_] <- RHS
            netIncrement_ <- netIncrement_ + 1
          },
          list(RHS = RHS)) 
      totalIndependentLength <- totalIndependentLength + 1
    }
  }
  setSizeLine <-
    substitute(
      .method(
          .member(ADtapeSetup, independentVars), 
          resize,
          TIL)
      , list(TIL = totalIndependentLength))
  returnCall <- cppLiteral("return(ADtapeSetup);")
  
  if(maxSize > 0){
    for(ivn in 1:maxSize)
      localVars$addSymbol( cppInt(name = indexVarNames[ivn],
                                  baseType = 'int') )    
  }
  
  allRcode <- do.call('call',
                      c(list('{'),
                        list(setSizeLine),
                        list(assignTapePtrCode),
                        copyIntoIndepVarCode, 
                        list(returnCall)),
                      quote=TRUE)
  allCode <- nParse(allRcode)
  TF$code <- cppCodeBlockClass$new(code = allCode, 
                                   symbolTable = localVars)
  TF
}

makeStaticInitClass <- function(cppDef, 
                                derivMethods) {
  cppClass <- cppClassClass$new(name = paste0('initTape_', cppDef$name),
                                useGenerator = FALSE) ## useGenerator may be deprecated
  globalsDef <- cppGlobalObjectClass$new(name = paste0('initTapeGlobals_', cppDef$name))
  globalsDef$symbolTable$addSymbol(cppVarFullClass$new(baseType = paste0('initTape_', cppDef$name),
                                                       name = paste0('initTape_', cppDef$name, '_Object_')))
  initializerCodeList <- list()
  for(derivFun in derivMethods){
    initializerDef <- cppFunctionClass$new(name = paste0('initTape_', cppDef$name),
                                           returnType = emptyTypeInfo())
    ## use of parse instead of substitute is so R CMD check doesn't flag CLASSNAME:: as an unmentioned dependency on package named CLASSNAME
    initializerCodeList <- c(initializerCodeList,
                             substitute(cppLiteral(CODE),
                               list(CODE = paste0(cppDef$name, "::allADtapePtrs_.push_back(",
                                                 cppDef$name, "::", paste0(derivFun, "_callForADtaping_"), "() );")))) ##[[1]])
    ## initializerCodeList <- c(initializerCodeList, substitute(push_back(CLASSNAME::allADtapePtrs_, CLASSNAME::ADTAPINGNAME() ), list(CLASSNAME = as.name(cppDef$name),ADTAPINGNAME = as.name(paste0(derivFun, "_callForADtaping_")))))
  }
  initializerCode <- do.call('call', c('{', initializerCodeList), quote = TRUE)
  initializerECcode <- nParse(initializerCode)
  initializerDef$code <- cppCodeBlockClass$new(code = initializerECcode, 
                                               symbolTable = symbolTableClass$new())
  cppClass$cppFunctionDefs[['initializer']] <- initializerDef
  cppClass$neededCppDefs[['globals']] <- globalsDef
  cppClass
}

## New tool needed: A derived cppDefs for the macros
## DERIVS_METHOD_DEF and DERIVS_METHOD_DECL. See inst/include/nCompiler_CppAD.h.
cppADmethodMacroClass <- R6::R6Class(
  classname = 'cppADmethodMacroClass', 
  portable = FALSE,
  inherit = cppDefinitionClass,
  public = list(name = NULL,     #Cfoo_derivs_
                base_name = NULL,#Cfoo
                args = NULL,
                initialize = function(...) {
                  self$name <- character()
                  dotsList <- list(...)
                  for(v in names(dotsList))
                    self[[v]] <- dotsList[[v]]
                  super$initialize()
                },
                generate = function(declaration = FALSE,
                                    scopes = character(),
                                    ...) {
                  argsListToUse <- if(inherits(self$args, 'symbolTableClass'))
                    self$args$getSymbols()
                  else {
                    list()
                  }
                  argsWithTypes <- paste(unlist(
                    lapply(argsListToUse,
                           function(x)
                             x$generate())
                  ),
                  collapse = ', ')
                  if(declaration) {
                    ans <- paste0("DERIVS_METHOD_DECL(", name, ",",
                                  argsWithTypes, ");")
                  } else {
                    argsWithoutTypes <- paste(unlist(
                      lapply(argsListToUse,
                             function(x)
                               x$generateUse())
                    ),
                    collapse = ', ')
                    if(length(scopes)==0) 
                      warning("No scope was provided for DERIVS macros")
                    if(length(scopes) > 1)
                      warning("Multiple scopes were provided for DERIVS macros")
                    ans <- paste0("DERIVS_METHOD_DEF(", scopes[1], ",", base_name,",",
                                  name, ",",
                                  "(", argsWithoutTypes, "),",
                                  argsWithTypes, ");")
                  }
                  ans
                }
  )
)
  
