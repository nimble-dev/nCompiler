## NEW: This is adapted from nCompiler's nfMethodRC
## NF_InternalsClass combines nfMethodRCinterface and nfMethodRC,
## unless we re-encounter a need to separate them.
NF_InternalsClass <- R6::R6Class(
    classname = 'NF_InternalsClass',
    portable = FALSE,
    public = list(
        arguments = NULL,
        argSymTab = list(),
        returnSym = NULL,
        where = NULL,
        isMethod = FALSE,
        uniqueName = character(),
        cpp_code_name = character(),
        template = NULL,
        code = NULL,
        RcppPacket = NULL,
        Rwrapper = NULL,
        aux = NULL, ## Used for constructor initializers.
        needed_nFunctions = list(), ## formerly neededRCfuns
        ## Next two "includes" were only needed for making external calls:
        ## If needed, these will be populated by.nCompilerExternalCall.
        ## It remains to be seen if they are needed in new system.
        externalHincludes = list(), 
        externalCPPincludes = list(),
        initialize = function(fun, ## formerly method
                              name,
                              argTypes = list(),
                              refArgs = list(),
                              blockRefArgs = list(),
                              returnType = NULL,
                              enableDerivs = list(),
                              check = FALSE,
                              ## methodNames, ## used only for nf_checkDSLcode
                              setupVarNames = NULL,
                              where = parent.frame()
                              ) {
            ## uniqueName is only needed if this is not a method of a nClass.
            if(!missing(name))
                uniqueName <<- name
            arguments <<- as.list(formals(fun))
            where <<- where
            if(is.character(refArgs)) {
                refArgs <- structure(as.list(rep(TRUE, length(refArgs))),
                                     names = refArgs)
            }
            if(is.character(blockRefArgs)) {
                blockRefArgs <- structure(as.list(rep(TRUE, length(blockRefArgs))),
                                     names = blockRefArgs)
            }
            argSymTab <<- argTypeList2symbolTable(argTypeList = arguments,
                                                  isArg = rep(TRUE, length(arguments)),
                                                  isRef = refArgs,
                                                  isBlockRef = blockRefArgs,
                                                  explicitTypeList = argTypes,
                                                  evalEnv = where)

            ## nf_changeKeywords changes all.nCompiler keywords,
            ## e.g. 'print' to 'nPrint'; see 'nKeyWords' list in
            ## changeKeywords.R
            code <<- nf_changeKeywords(body(fun)) 
            if(code[[1]] != '{')
                code <<- substitute({CODE}, list(CODE=code))
            ## check all code except.nCompiler package nFunctions
##            if(check && "package.nCompiler" %in% search()) 
            ##                nf_checkDSLcode(code, methodNames, setupVarNames)
            ## Some of this should become unnecessary.
            ## However, a problem is how regular R default values will be used
            ##   in the "decoration" system.  They could be put in "value" argument.
            ##  Either a named "value" or a ... is in all types.

            ## not used until much later
            template <<- Rarguments_2_functionTemplate(arguments) ## generateTemplate()
            returnTypeInfo <- nf_extractReturnType(code)
            returnTypeDecl <- returnTypeInfo$returnType
            if(is.null(returnTypeDecl)) {
                if(is.null(returnType))
                    returnTypeDecl <- quote(void()) ## default behavior
                else
                    returnTypeDecl <- returnType
            } else {
                if(!is.null(returnType))
                    stop(paste0("Return type was declared in code and by returnType argument.\n",
                                "Use one or the other.  Providing both is not allowed.\n"),
                         call. = FALSE)
                code <<- returnTypeInfo$code ## with returnType() line stripped
            }
            returnSym <<- argType2symbol(returnTypeDecl,
                                         origName = "returnType")
            ## We set the cpp_code_name here so that other nFunctions
            ## that call this one can determine, during compilation,
            ## what this one's cpp function name will be:
            cpp_code_name <<- paste(name,
                                    nFunctionIDMaker(),
                                    sep = "_")
            
        },
      getFunction = function() {
        functionAsList <- list(as.name('function'))
        functionAsList[2] <- list(NULL)
        if(!is.null(args)) functionAsList[[2]] <- as.pairlist(arguments)
        boolRefArg <- unlist(lapply(argSymTab$symbols,
                                    function(x) x$isRef))
        refArgs <- names(argSymTab$symbols)[boolRefArg]
        boolBlockRefArg <- unlist(lapply(argSymTab$symbols,
                                         function(x) x$isBlockRef))
        blockRefArgs <- names(argSymTab$symbols)[boolBlockRefArg]
        callableCode <- if(length(refArgs) > 0 | length(blockRefArgs > 0))
                          passByReference(code,
                                          refArgs,
                                          blockRefArgs)
                        else
                          code
        functionAsList[[3]] <- callableCode
        ans <- eval(
          parse(text=deparse(as.call(functionAsList)),
                keep.source = FALSE)[[1]])
        environment(ans) <- where
        ans
        }
    )
)

Rarguments_2_functionTemplate <- function(Rarguments) {
    functionAsList <- list(as.name('function'))
    functionAsList[2] <- list(NULL)
    if(!is.null(args)) functionAsList[[2]] <- as.pairlist(Rarguments)
    functionAsList[[3]] <- quote({})
    eval(as.call(functionAsList))
}

nf_extractReturnType <- function(code) {
    returnLineNum <- 0
    for(i in seq_along(code)) {
        if(length(code[[i]]) > 1) {
            if(is.name(code[[i]][[1]])) {
                if(code[[i]][[1]] == 'returnType') {
                    returnLineNum <- i
                    break;
                }
            }
        }
    }
    if(sum(all.names(code) == 'returnType') > 1)
        stop('multiple returnType() declarations in nFunction method; only one allowed')
    if(returnLineNum == 0) {
        ## no returnType() declaration was found;
        ## create default behavior.
        returnTypeDeclaration <- NULL ##quote(void())
    } else {
        ## returnType() declaration was found
        returnTypeDeclaration <- code[[returnLineNum]][[2]]
        code[returnLineNum] <- NULL
    }
    ## a very patchy solution: switch nInteger back to integer
    ## if(as.character(returnTypeDeclaration[[1]]) == 'nInteger')
    ##     returnTypeDeclaration[[1]] <- as.name('integer')
    ## if(as.character(returnTypeDeclaration[[1]]) == 'nLogical')
    ##     returnTypeDeclaration[[1]] <- as.name('logical')
    list(code = code,
         returnType = returnTypeDeclaration)
}
