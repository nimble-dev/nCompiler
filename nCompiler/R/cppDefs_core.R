## This file contains reference classes used for C++ units such as
## cppDefinition (base class for all others)
## cppNamespace (base class for classes)
## cppClass
##

## Base class for C++ file and compilation information
cppDefinitionClass <- R6::R6Class(
  classname = 'cppDefinitionClass', 
  portable = FALSE,
  public = list(
    filename = character(),
    CPPpreamble = character(),
    Hpreamble = character(),
    CPPusings = character(),
    neededCppDefs = list(),
    Hincludes = list(),
    CPPincludes = list(),
    initialize = function(...) {
      dotsList <- list(...)
      for(v in names(dotsList))
        self[[v]] <- dotsList[[v]]
      self
    },
    getHincludes = function() {return(self$Hincludes)},
    getCPPincludes = function() {return(self$CPPincludes)},
    getHpreamble = function() {return(self$Hpreamble)},
    getCPPpreamble = function() {return(self$CPPpreamble)},
    getCPPusings = function() {return(self$CPPusings)},
    ## return all objects to be included.  This allows adjunct objects
    ## like SEXPinterfaceFuns to be included
    getDefs = function() {return(c(list(self),
                                   do.call("c", lapply(neededCppDefs, function(x) x$getDefs()) ) ) )},
    get_post_cpp_compiler = function() NULL)
)

cppMacroCallClass <- R6::R6Class(
  'cppMacroCallClass',
  portable = FALSE,
  inherit = cppDefinitionClass,
  public = list(
    name = character(),
    hContent = "",
    cppContent = "",
    initialize = function(...) {
      super$initialize(...)
    },
    generate = function(declaration = FALSE) {
      if(declaration)
        hContent
      else
        cppContent
    }
  )
)

cppGlobalObjectClass <- R6::R6Class(
  'cppGlobalObjectClass',
  inherit = cppDefinitionClass,
  portable = FALSE,
  public = list(
    name = character(),
    symbolTable = NULL, ## formerly objectDefs
    staticMembers = FALSE,
    initialize = function(...) {
      symbolTable <<- symbolTableClass$new()
      super$initialize(...)
    },
    generate = function(declaration = FALSE) {
      ## For globals for class static members, we want no declaration
      ## Otherwise we want a declaration and put extern in front
      if(staticMembers & declaration) return(character())
      output <- paste0(generateAll(symbolTable$getSymbols(), 
                                   declaration = declaration),
                       ';')
      if(declaration) 
        output <- paste("extern ", output)
      output
    }
  ))
## class for C++ namespaces.
## A namespace includes, objects, classes, functions, typedefs, and other namespaces
## This is incomplete.  The typeDefs and nested namespaces are not used yet.
## The other components are used and so have been more developed.
cppNamespaceClass <- R6::R6Class(
  'cppNamespaceClass',
  inherit = cppDefinitionClass,
  portable = FALSE,
  public = list(
    name = character(),
    symbolTable = NULL, ## list or symbolTable
    cppFunctionDefs = list(),
    #     extraDefs = list(),
    initialize = function(...) {
      super$initialize(...)
    },
    addSym = function(newName, newObj) {
      stop("figure out how objects or symbols should be added to cppNamespaceClass")
      objectDefs[[newName]] <<- newObj # this looks deprecated.
    },
    addFunction = function(newName, newFun) {
      cppFunctionDefs[[newName]] <<- newFun
    },
    generate = function() {
      symbolsToUse <-
        if(inherits(objectDefs, 'symbolTableClass'))
          symbolTable$getSymbols()
      else
        list()
      output <- c(generateNameSpaceHeader(name$generate()),
                  generateObjectDefs(symbolsToUse),
                  generateAll(cppFunctionDefs, declaration = TRUE),
                  '};'
                  )
      unlist(output)
    }
  )
)


## C++ class object.
## A class is like a namespace with inheritance
## At the moment everything is public. 
## This class can build cppFunction objects for a generator function and a finalizer function
## The generator can be called via .Call to return an external pointer to a new object of the class
## The finalizer is the finalizer assigned to the object when the external pointer is made
buildSEXPgenerator_impl <- function(self) {
  self$Hincludes <- c(self$Hincludes
                      # , nCompilerIncludeFile("nCompiler_class_factory.h")
                      , nCompilerIncludeFile("nCompiler_loadedObjectsHook.h") )
  returnLine <- paste0("return CREATE_NEW_NCOMP_OBJECT(",self$name,");")
  allCodeList <-
    list(
      substitute(cppLiteral(RETURNLINE),
                 list(RETURNLINE = as.character(returnLine)))
    )
  
  allCode <- putCodeLinesInBrackets(allCodeList)
  allCode <- nParse(allCode)
  self$SEXPgeneratorDef <-
    cppFunctionClass$new(name = paste0('new_',self$name),
                         args = symbolTableClass$new(),
                         code = cppCodeBlockClass$new(code = allCode,
                                                      symbolTable = symbolTableClass$new(),
                                                      skipBrackets = TRUE),
                         returnType = cppSEXP(),
                         commentsAbove = '// [[Rcpp::export]]'
                         )
  invisible(NULL)
}

build_set_nClass_env_impl <- function(self) {
  self$Hincludes <- c(self$Hincludes
                      , nCompilerIncludeFile("nCompiler_loadedObjectsHook.h") )
  setterLine <- paste0("SET_CNCLASS_ENV(",self$name,", env);")
  allCodeList <-
    list(
      substitute(cppLiteral(SETTERLINE),
                 list(SETTERLINE = as.character(setterLine)))
    )
  
  allCode <- putCodeLinesInBrackets(allCodeList)
  allCode <- nParse(allCode)
  args <- symbolTableClass$new()
  args$addSymbol(cppSEXP(name = "env"))
  self$set_nClass_envDef <-
    cppFunctionClass$new(name = paste0('set_CnClass_env_',self$name),
                         args = args,
                         code = cppCodeBlockClass$new(code = allCode,
                                                      symbolTable = symbolTableClass$new(),
                                                      skipBrackets = TRUE),
                         returnType = cppVoid(),
                         commentsAbove = '// [[Rcpp::export]]'
    )
  invisible(NULL)
}

# make_loadedObjectEnv_cppDef <- function() {
#   LOEfunDef <-
#     cppMacroCallClass$new(
#       cppContent = paste0("#ifndef _ONTHEFLY_LOADEDOBJECTENV\n",
#                           "#define _ONTHEFLY_LOADEDOBJECTENV\n",
#                           "SEXP loadedObjectEnv(SEXP Xptr) {\n",
#                           "Rcpp::Environment nc(\"package:nCompiler\");\n",
#                           "Rcpp::Function newLOE = nc[\"new.loadedObjectEnv\"];\n",
#                           "return newLOE(Xptr);\n",
#                           "}\n",
#                           "#endif\n"),
#       name = "loadedObjectEnv"
#     )
#   LOEfunDef
# }

# addLoadedObjectEnv_impl <- function(self) {
#   LOEfunDef <- make_loadedObjectEnv_cppDef()
#   self$neededCppDefs[["loadedObjectEnv"]] <- LOEfunDef
# }

add_obj_hooks_impl <- function(self) {
  name <- self$name
  self$addInheritance(paste0("loadedObjectHookC<",
                             name,
                             ">"))
}

addGenericInterface_impl <- function(self) {
  name <- self$name
  self$addInheritance(paste0("genericInterfaceC<",
                             name,
                             ">"))
  self$Hincludes <- c(self$Hincludes,
                      nCompilerIncludeFile("nCompiler_class_interface.h"))

  methodNames <- names(self$cppFunctionDefs)
  includeBool <- methodNames %in% self$functionNamesForInterface
  if(sum(includeBool) > 0) {
    # construct arg info sections like
    # args({{'x', ref}, {'y', copy}})
    cppArgInfos <- structure(character(length(methodNames)), names = methodNames)
    for(mName in methodNames) {
      args <- self$cppFunctionDefs[[mName]]$args
      argNames <-
        if(inherits(args, 'symbolTableClass')) {
          names(args$getSymbols())
        } else {
          character()
        }
      post_cpp <- self$cppFunctionDefs[[mName]]$get_post_cpp_compiler()[[1]]
      passingTypes <-
        ifelse(post_cpp$refArgs[argNames] |> lapply(isTRUE) |> unlist(), "ref",
               ifelse(post_cpp$refArgs[argNames] |> lapply(isTRUE) |> unlist(), "refBlock", "copy"))
      step1 <- paste0('\"',argNames,'\"')
      step2 <- paste(step1, passingTypes, sep=',')
      step3 <- paste0('{arg(', step2, ')}', collapse = ',')
      step4 <- paste0('args({', step3, '})')
      cppArgInfos[mName] <- step4
    }

    cppMethodNames <- lapply(self$cppFunctionDefs, function(x) x$name)
    methodsContent <- paste0("method(\"",
                             methodNames[includeBool],
                             "\", &",
                             name,
                             "::",
                             cppMethodNames[includeBool],
                             ", ",
                             cppArgInfos[includeBool],
                             ")", collapse = ",\n")
    methodsContent <- paste0("NCOMPILER_METHODS(\n", methodsContent, "\n)")
  } else
    methodsContent <- "NCOMPILER_METHODS()"
  fieldNames <- self$symbolTable$getSymbolNames()
  fieldNames <- fieldNames[fieldNames %in% self$variableNamesForInterface]
  if(length(fieldNames) > 0) {
    fieldsContent <- paste0("field(\"",
                            fieldNames,
                            "\", &",
                            name,
                            "::",
                            fieldNames,
                            ")", collapse = ",\n")
    fieldsContent <- paste0("NCOMPILER_FIELDS(\n", fieldsContent, "\n)")
  } else
    fieldsContent <- "NCOMPILER_FIELDS()"
  macroCallContent <- paste0("NCOMPILER_INTERFACE(\n",
                             paste(name,
                                   fieldsContent,
                                   methodsContent,
                                   sep=",\n"),
                             "\n)")
  macroCallDef <- cppMacroCallClass$new(cppContent = macroCallContent)
  self$neededCppDefs[["macroCall"]] <- macroCallDef
  invisible(NULL)
}

cppClassClass <- R6::R6Class(
  'cppClassDef',
  inherit = cppNamespaceClass,
  portable = FALSE,
  public = list(
    inheritance = list(),           ## classes to be declared as public 
    ## ancestors = 'list',             ## classes inherited by inherited classes, needed to make all cast pointers
    ##extPtrTypes = 'ANY',
    ##private = 'list',		# 'list'. This field is a placeholder for future functionality.  Currently everything is generated as public
    useGenerator = TRUE,		#'logical', ## not clear if or how this is needed in new system. ## toggled whether to include a SEXPgeneratorFun in old system
    SEXPgeneratorDef = NULL,
    set_nClass_envDef = NULL,
    functionNamesForInterface = character(),
    variableNamesForInterface = character(),
    ##SEXPfinalizerFun = 'ANY',
    # globalObjectsDefs = list(),
    
    initialize = function(...) {
      ##useGenerator <<- TRUE
      Hincludes <<-	c(Hincludes, '<Rinternals.h>', nCompilerIncludeFile("nCompiler_core.h"))	
      CPPincludes <<-	c(CPPincludes, '<iostream>') 
      super$initialize(...)
    },
    getHincludes = function() {
      Hinc <- c(Hincludes,
                if(!is.null(SEXPgeneratorDef))
                  SEXPgeneratorDef$getHincludes(),
                if(!is.null(set_nClass_envDef))
                  set_nClass_envDef$getHincludes(),
                unlist(lapply(cppFunctionDefs,
                              function(x)
                                x$getHincludes()),
                       recursive = FALSE))
      Hinc
    },
    getCPPincludes = function() {
      CPPinc <- c(CPPincludes,
                  if(!is.null(SEXPgeneratorDef))
                    SEXPgeneratorDef$getCPPincludes(),
                  if(!is.null(set_nClass_envDef))
                    set_nClass_envDef$getCPPincludes(),
                  unlist(lapply(cppFunctionDefs,
                                function(x)
                                  x$getCPPincludes()),
                         recursive = FALSE))
      CPPinc
    },
    getCPPusings = function() {
      CPPuse <- unique(c(CPPusings,
                         if(!is.null(SEXPgeneratorDef))
                           SEXPgeneratorDef$getCPPusings(),
                         if(!is.null(set_nClass_envDef))
                           set_nClass_envDef$getCPPusings(),
                         unlist(lapply(cppFunctionDefs,
                                       function(x)
                                         x$getCPPusings()))
                         ))
      CPPuse
    },
    getDefs = function() {
      ans <- if(isTRUE(useGenerator)) {
        if(is.null(SEXPgeneratorDef))
          stop('Trying to getDefs from a CppClassClass with useGenerator==TRUE but SEXPgeneratorDef not defined')
        list(self, SEXPgeneratorDef, set_nClass_envDef)
      } else {
        list(self, set_nClass_envDef)
      }
      if(length(neededCppDefs) > 0)
        ans <- c(ans, 
                 do.call("c", lapply(neededCppDefs, function(x) x$getDefs())))
      ans
    },
    addInheritance = function(newI) {
      inheritance <<- c(inheritance, newI)
    },
    ##addAncestors = function(newI) ancestors <<- c(ancestors, newI), 
    ##setPrivate = function(name) private[[name]] <<- TRUE,
    generate = function(declaration = FALSE, ...) {
      if(declaration) {
        symbolsToUse <- if(inherits(symbolTable, 'symbolTableClass'))
                          symbolTable$getSymbols()
        else {
          list()
        }
        output <- c(generateClassHeader(name, inheritance),
                    list('public:'), ## In the future we can separate public and private
                    lapply(generateObjectDefs(symbolsToUse),
                           function(x)
                             if(length(x)==0)
                               ''
                           else
                             pasteSemicolon(x, indent = '  ')),
                    generateAll(cppFunctionDefs, declaration = TRUE),
                    '};'
                    )
      } else {
        if(length(cppFunctionDefs) > 0) {
          output <- generateAll(cppFunctionDefs, scopes = name)
        } else {
          output <- ""  
        }
      }
      unlist(output)
    },
    ## getExtPtrTypeIndex = function() {
    ##     structure(1:(length(extPtrTypes)+1), names = c(name, extPtrTypes))
    ## },
    buildSEXPgenerator = function() {
      buildSEXPgenerator_impl(self)
    },
    build_set_nClass_env = function() {
      build_set_nClass_env_impl(self)
    },
    addGenericInterface = function() {
      addGenericInterface_impl(self)
      add_obj_hooks_impl(self)
    },
    addSerialization = function(include_DLL_funs = FALSE) {
      addSerialization_impl(self, include_DLL_funs)
    }
    # , addLoadedObjectEnv = function() {
    #   addLoadedObjectEnv_impl(self)
    # }
  )
)


## A cppCodeBlock is an arbitrary collection of annotated syntax tree (exprClass)
## and other cppCodeBlocks (defined below).
## The parse tree can be either an R parse tree or one of our exprClass objects
cppCodeBlockClass <- R6::R6Class(
  classname = 'cppCodeBlockClass',
  portable = FALSE,
  public = list(
    typeDefs = list(),
    symbolTable = list(), ##formerly objectDefs
    code = NULL,
    skipBrackets = NULL,
    cppADCode = NULL,
    generatorSymTab = NULL,
    initialize = function(...) {
      dotsList <- list(...)
      for(v in names(dotsList))
        self[[v]] <- dotsList[[v]]
      self
    },
    generate = function(indent = '',
                        ...) {
      typeDefsToUse <- if(inherits(self$typeDefs, 'symbolTableClass'))
                         self$typeDefs$symbols
      else
        self$typeDefs
      if(length(typeDefsToUse) > 0) {
        outputCppCode <- paste0(indent,
                                generateArguments(typeDefsToUse),
                                ';')
      } else
        outputCppCode <- list()
      symbolTableToUse <- if(inherits(self$symbolTable, 'symbolTableClass'))
                            self$symbolTable$symbols
      else {
        stop("symbolTable should be a symbolTable")
        self$symbolTable
      }
      if(length(symbolTableToUse) > 0) {
        outputCppCode <- c(outputCppCode,
                           paste0(indent,
                                  generateArguments(symbolTableToUse),
                                  ';')
                           )
      }
      if(inherits(self$code, 'exprClass')) {
        if(inherits(self$generatorSymTab, 'symbolTableClass'))
          useSymTab <- self$generatorSymTab
        else
          if(!inherits(self$symbolTable, 'symbolTableClass')) {
            stop(paste0('Error, with exprClass code in the ',
                        'cppCodeBlock, must have symbolTable ',
                        'be a symbolTable'),
                 call. = FALSE)
          } else
            useSymTab <- self$symbolTable
        if(isTRUE(self$cppADCode))
          recurseSetCppADExprs(self$code, TRUE) 
        outputCppCode <- c(outputCppCode,
                           compile_generateCpp(self$code,
                                               useSymTab,
                                               indent = ' ',
                                               showBracket = FALSE)
                           )
        if(isTRUE(self$cppADCode))
          recurseSetCppADExprs(self$code, FALSE) 
      } else {
        stop('code in generate() is not of the right type.',
             call. = FALSE)
      }
      outputCppCode
    }
  )
)

## C++ function definitions
##
cppFunctionClass <- R6::R6Class(
  classname = 'cppFunctionClass',
  portable = FALSE,
  inherit = cppDefinitionClass,
  public = list(name = NULL,
                returnType = NULL,
                args = NULL,
                code = NULL,
                externC = NULL,
                virtual = FALSE,
                static = FALSE,
                abstract = FALSE,
                template = NULL,
                const = FALSE,
                classMethod = FALSE,
                initializerList = NULL,
                commentsAbove = character(),
                initialize = function(...) {
                  self$name <- character()
                  self$CPPincludes <- as.list(
                    c(self$CPPincludes,
                      '<iostream>',
                      nCompilerIncludeFile("nCompiler_core.h"))
                  )
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
                  if(declaration) {
                    outputCode <- paste0(
                      if(self$virtual)
                        'virtual '
                      else
                        character(0),
                      
                      generateFunctionHeader(self$returnType,
                                             self$name,
                                             argsListToUse,
                                             scopes,
                                             self$template,
                                             self$static, ...),
                      
                      if(self$const)
                        ' const '
                      else
                        character(0),

                      if(self$abstract)
                        '= 0'
                      else
                        character(0),

                      ';'
                    ) ## end paste
                    if(isTRUE(self$externC))
                      outputCode <- paste0('extern "C" ', outputCode)
                    return(outputCode) 
                  } else {
                    if(is.null(self$code$code))
                      ## There is no code. This can occur for
                      ## a nFunctionVirtual that is an
                      ## abstract base class.
                      return(character(0))
                  }
                  c(self$commentsAbove,
                    paste0(
                      generateFunctionHeader(self$returnType,
                                             self$name,
                                             argsListToUse,
                                             scopes,
                                             self$template,
                                             static = FALSE,
                                             ...), ' ',
                      if(self$const)
                        ' const '
                      else
                        character(),
                      ' ',
                      
                      if(!is.null(self$initializerList))
                        generatorinitializerList(self$initializerList) ## We can add a symbolTable to use later if necessary
                      else
                        character(0),
                      '{'
                    ), ## end paste,
                    'RESET_EIGEN_ERRORS'[
                      isTRUE(nOptions('compilerOptions')$throwEigenErrors)
                    ],
                    'BEGIN_NC_ERRORTRAP'[
                      isTRUE(nOptions('compilerOptions')$cppStacktrace)
                    ],
                    self$code$generate(...),
                    'END_NC_ERRORTRAP'[
                      isTRUE(nOptions('compilerOptions')$cppStacktrace)
                    ],
                    list('}')
                    )## end c()
                }
                )
)

generatorinitializerList <- function(initializerList) { 
  ## initializerList should be a list of exprClass objects
  if(length(initializerList) == 0) return(character(0))
  ## When no symbolTable is provided to compile_generateCpp, it just outputs
  ## code$name entries.
  initializers <- lapply(initializerList, compile_generateCpp)
  result <- paste0(':\n',
                   paste(initializers, collapse = ",\n"))
  result
}

generateFunctionHeader <- function(returnType,
                                   name,
                                   args,
                                   scopes = character(),
                                   template = character(),
                                   static = FALSE) {
  list(paste(
    paste0(if(length(template) == 0)
      character()
      else
        paste(template$generate(),'\n'),
      
      paste0(if(static)
        'static '
        else
          character(),

        returnType$generate(printName = character()),
        collapse = ''
        )
      ),
    paste(c(scopes, name), collapse = '::'),
    '(',
    paste(unlist(
      lapply(args,
             function(x)
               x$generate())
    ),
    collapse = ', '),
    ')'
  )
  )
}

generateClassHeader <- function(ns, inheritance) {
  inheritancePart <-
    if(length(inheritance) > 0) {
      paste(':',
            paste('public',
                  unlist(inheritance),
                  collapse = ', ')
            )
    } else
      NULL
  list(paste('class',
             ns,
             inheritancePart,
             '{'))
}

generateArguments <- function(argsSymbolTable, ...) {
  generateAll(argsSymbolTable, ...)
}

generateObjectDefs <- function(objectDefs, ...) {
  generateAll(objectDefs, ...)
}
