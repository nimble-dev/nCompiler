## This file contains reference classes used for C++ units such as
## cppDefinition (base class for all others)
## cppNamespace (base class for classes)
## cppClass
##

## Base class for C++ file and compilation information
# This has only a filename, preambles (CPP and H),
# usings (CPP only), includes (CPP and H), and
# internalCppDefs and externalCppDefs.
# (internalCppDefs will be kept in the same RcppPacket and ultimately C++ code files.
#  externalCppDefs are needed but will be generated separately. externalCppDefs will be
#  collected from all cppDefs in a compilation call and put through unique() to avoid
#  duplication. internalCppDefs are assumed to be unique.)
# cppDefinitionClass has getter methods but no generate method.
cppDefinitionClass <- R6::R6Class(
  classname = 'cppDefinitionClass', 
  portable = FALSE,
  public = list(
    filename = character(),
    CPPpreamble = character(),
    Hpreamble = character(),
    CPPusings = character(),
    internalCppDefs = list(),
    externalCppDefs = list(),
    Hincludes = list(),
    CPPincludes = list(),
    compileInfo = list(), # This can be used generically for details about each derived class that are deemed too picayune for a separate member variable
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
    getInternalDefs = function() {return(c(list(self),
                                           do.call("c", lapply(internalCppDefs, function(x) x$getInternalDefs()) ) ) )},
    getExternalDefs = function() {return(c(do.call("c", lapply(internalCppDefs, function(x) x$getExternalDefs()) ),
                                           externalCppDefs,
                                           do.call("c", lapply(externalCppDefs, function(x) x$getExternalDefs()) ) ) )},
    get_post_cpp_compiler = function() NULL)
)

# cppMacroCallClass is the most rudimentary cppDef that can generate content.
# It extends cppDefinitionClass with character vectors of hContent and cppContent
# and has a generate method.
# It was designed for things like preprocessor directives or global macro calls,
# but it can be used in any case where arbitrary text should be included in C++.
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

# cppGlobalObjectsClass include a symbolTable and flag for whether symbols are static.
# The staticMembers is a flag shared by all symbols.
# For the static case, no declaration is needed (only a definition).
# For the non-static case, declarations use "extern".
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

## cppNamespaceClass is for C++ namespaces, which serve as a base class
##   for the C++ class classDefs.
## A namespace includes objects, classes, functions, typedefs, and other namespaces
## This is incomplete.  The typeDefs and nested namespaces are not used yet.
## The other components are used and so have been more developed.
## objects are defined in the symbolTable.
## functions are defined in cppFunctionDefs.
cppNamespaceClass <- R6::R6Class(
  'cppNamespaceClass',
  inherit = cppDefinitionClass,
  portable = FALSE,
  public = list(
    name = character(),
    symbolTable = NULL, ## list or symbolTable
    namespaceCppDefs = list(), # formerly cppFunctionDefs
    initialize = function(...) {
      super$initialize(...)
    },
    ## addSym = function(newName, newObj) {
    ##   stop("figure out how objects or symbols should be added to cppNamespaceClass")
    ##   objectDefs[[newName]] <<- newObj # this looks deprecated.
    ## },
    ## addFunction = function(newName, newFun) {
    ##   cppFunctionDefs[[newName]] <<- newFun
    ## },
    generate = function(declaration=FALSE) {
      symbolsToUse <-
        if(inherits(symbolTable, 'symbolTableClass'))
          symbolTable$getSymbols()
      else
        list() # Ways to hold symbols besides a symbolTable may be deprecated. This also catches NULL
      blankName <- length(name)==0
      output <- c(if(blankName) paste0("namespace ", name, " {\n") else NULL,
                  generateObjectDefs(symbolsToUse),
                  generateAll(namespaceCppDefs, declaration = declaration),
                  if(blankName) '};' else NULL
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
#  self$Hincludes <- c(self$Hincludes
                      # , nCompilerIncludeFile("nCompiler_class_factory.h")
#                      , nCompilerIncludeFile("nCompiler_loadedObjectsHook.h") )
  self$Hpreamble <- c(self$Hpreamble,
                      "#define NCOMPILER_USES_NCLASS_INTERFACE")
  self$CPPpreamble <- c(self$CPPpreamble,
                      "#define NCOMPILER_USES_NCLASS_INTERFACE")
  returnLine <- paste0("return CREATE_NEW_NCOMP_OBJECT(",self$name,");")
  allCodeList <-
    list(
      substitute(cppLiteral(RETURNLINE),
                 list(RETURNLINE = as.character(returnLine)))
    )
  
  allCode <- putCodeLinesInBrackets(allCodeList)
  allCode <- nParse(allCode)
  self$internalCppDefs[["SEXPgenerator"]] <-
    cppFunctionClass$new(name = paste0('new_',self$name),
                         args = symbolTableClass$new(),
                         code = cppCodeBlockClass$new(code = allCode,
                                                      symbolTable = symbolTableClass$new(),
                                                      skipBrackets = TRUE),
                         returnType = cppSEXP(),
                         commentsAbove = paste0('// [[Rcpp::export(name = "', self$compileInfo$exportName ,'")]]')
                         )
  invisible(NULL)
}

# set_nClass_env is the function to be called from R that provides the
# CnCenv (compiled nClass environment) that all loadedObjectEnv objects for
# a particular nClass will use as a parent environment.
build_set_nClass_env_impl <- function(self) {
  #  self$Hincludes <- c(self$Hincludes
  #                      , nCompilerIncludeFile("nCompiler_loadedObjectsHook.h") )
  self$Hpreamble <- c(self$Hpreamble,
                      "#define NCOMPILER_USES_NCLASS_INTERFACE")
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
  self$internalCppDefs[["set_nClass_env"]] <-
    cppFunctionClass$new(name = paste0('set_CnClass_env_',self$name),
                         args = args,
                         code = cppCodeBlockClass$new(code = allCode,
                                                      symbolTable = symbolTableClass$new(),
                                                      skipBrackets = TRUE),
                         returnType = cppVoid(),
                         commentsAbove = paste0('// [[Rcpp::export(name = "set_CnClass_env_',
                                                self$compileInfo$exportName ,'")]]')
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
#  self$Hincludes <- c(self$Hincludes,
#                      nCompilerIncludeFile("nCompiler_class_interface.h"))
  self$Hpreamble <- c(self$Hpreamble,
                      "#define NCOMPILER_USES_NCLASS_INTERFACE")
  self$CPPpreamble <- c(self$CPPpreamble,
                      "#define NCOMPILER_USES_NCLASS_INTERFACE")

  methodNames <- names(self$memberCppDefs)
  includeBool <- methodNames %in% self$functionNamesForInterface
  if(sum(includeBool) > 0) {
    # construct arg info sections like
    # args({{'x', ref}, {'y', copy}})
    cppArgInfos <- structure(character(length(methodNames)), names = methodNames)
    for(mName in methodNames) {
      args <- self$memberCppDefs[[mName]]$args
      argNames <-
        if(inherits(args, 'symbolTableClass')) {
          names(args$getSymbols())
        } else {
          character()
        }
      if(length(argNames)) {
        post_cpp <- self$memberCppDefs[[mName]]$get_post_cpp_compiler()[[1]]
        passingTypes <-
          ifelse(post_cpp$refArgs[argNames] |> lapply(isTRUE) |> unlist(), "ref",
          ifelse(post_cpp$refArgs[argNames] |> lapply(isTRUE) |> unlist(), "refBlock", "copy"))
        step1 <- paste0('\"',argNames,'\"')
        step2 <- paste(step1, passingTypes, sep=',')
        step3 <- paste0('{arg(', step2, ')}', collapse = ',')
      } else {
        step3 <- '{}'
      }
      step4 <- paste0('args({', step3, '})')
      cppArgInfos[mName] <- step4
    }

    cppMethodNames <- lapply(self$memberCppDefs, function(x) x$name)
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
  cpp_fieldNames <- unlist(
    lapply(fieldNames,
           function(x) self$symbolTable$getSymbol(x)$generateUse()))
  if(length(fieldNames) > 0) {
    fieldsContent <- paste0("field(\"",
                            fieldNames,
                            "\", &",
                            name,
                            "::",
                            cpp_fieldNames,
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
  self$internalCppDefs[["macroCall"]] <- macroCallDef
  invisible(NULL)
}

cppClassClass_init_impl <- function(cppDef) {
  cppDef$Hincludes <- c(cppDef$Hincludes, '<Rinternals.h>',
                        nCompilerIncludeFile("nCompiler_omnibus_first_h.h"))
#                        nCompilerIncludeFile("nCompiler_core.h"))
  cppDef$CPPincludes <- c(cppDef$CPPincludes, '<iostream>')
}

# cppClassClass is the cppDef for a C++ class definition.
# It is the base class for the cppDef hierarchy for C++ nClass content.
#
# It is a namespace with the added feature of inheritance labels,
#  a SEXPgenerator function (callable from R to obtain a new C++ class object)
#  a set_nClass_env function (callable from R to provide the CnCenv object from R)
#  information for creating the generic interface to class objects
#    (for calling from R via call_method, get_value, and set_value)
cppClassClass <- R6::R6Class(
  'cppClassDef',
  inherit = cppDefinitionClass, # previously inherited from namespace, but that's not very helpful
  portable = FALSE,
  public = list(
    name = character(),
    symbolTable = NULL, ## list or symbolTable
    memberCppDefs = list(), # formerly cppFunctionDefs
    inheritance = list(),           ## classes to be declared as public
    ## ancestors = 'list',             ## classes inherited by inherited classes, needed to make all cast pointers
    ##extPtrTypes = 'ANY',
    ##private = 'list',     # 'list'. This field is a placeholder for future functionality.  Currently everything is generated as public
    useGenerator = TRUE,    # toggles whether to include a SEXPgeneratorFun.
    # SEXPgeneratorDef = NULL, # put this in internalCppDefs
    # set_nClass_envDef = NULL, # ditto
    functionNamesForInterface = character(),
    variableNamesForInterface = character(),
    ##SEXPfinalizerFun = 'ANY',
    # globalObjectsDefs = list(),
    
    initialize = function(...) {
      ##useGenerator <<- TRUE
      force(self)
      cppClassClass_init_impl(self)
      super$initialize(...)
    },
    getHincludes = function() {
      Hinc <- c(Hincludes,
                if(!is.null(internalCppDefs[["SEXPgenerator"]]))
                  internalCppDefs[["SEXPgenerator"]]$getHincludes(),
                if(!is.null(internalCppDefs[["set_nClass_env"]]))
                  internalCppDefs[["set_nClass_env"]]$getHincludes(),
                unlist(lapply(memberCppDefs,
                              function(x)
                                x$getHincludes()),
                       recursive = FALSE))
      Hinc
    },
    getCPPincludes = function() {
      CPPinc <- c(CPPincludes,
                  if(!is.null(internalCppDefs[["SEXPgenerator"]]))
                    internalCppDefs[["SEXPgenerator"]]$getCPPincludes(),
                  if(!is.null(internalCppDefs[["set_nClass_env"]]))
                    internalCppDefs[["set_nClass_env"]]$getCPPincludes(),
                  unlist(lapply(memberCppDefs,
                                function(x)
                                  x$getCPPincludes()),
                         recursive = FALSE))
      CPPinc
    },
    getCPPusings = function() {
      CPPuse <- unique(c(CPPusings,
                         if(!is.null(internalCppDefs[["SEXPgenerator"]]))
                           internalCppDefs[["SEXPgenerator"]]$getCPPusings(),
                         if(!is.null(internalCppDefs[["set_nClass_env"]]))
                           internalCppDefs[["set_nClass_env"]]$getCPPusings(),
                         unlist(lapply(memberCppDefs,
                                       function(x)
                                         x$getCPPusings()))
                         ))
      CPPuse
    },
    getInternalDefs = function() {
      c(super$getInternalDefs())
      # Don't include memberCppDefs$getInternalDefs() because generate() will produce
      # those recursively
    },
    getExternalDefs = function() {
      c(super$getExternalDefs(),
        do.call("c", lapply(memberCppDefs, function(x) x$getExternalDefs())))
      # Do include memberCppDefs$getExternalDefs() because generate() will not produce
      # those.
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
                    generateAll(memberCppDefs, declaration = TRUE),
                    # it is important to declare methods before variables
                    # because nDerivsMgrClass variables are templated using a macro
                    # that invokes a method address to get its type, so the method
                    # must have been declared before the variable.
                    lapply(generateObjectDefs(symbolsToUse),
                           function(x)
                             if(length(x)==0)
                               ''
                           else
                             pasteSemicolon(x, indent = '  ')),
                    '};'
                    )
      } else {
        if(length(memberCppDefs) > 0) {
          output <- generateAll(memberCppDefs, scopes = name)
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
    addGenericInterface = function(interfaceCalls = TRUE) {
      addGenericInterface_impl(self)
      add_obj_hooks_impl(self)
      # The only case that would omit interface calls is generated predefined code.
      if(interfaceCalls)
        self$externalCppDefs[["R_generic_interface_calls"]]  <- get_R_interface_cppDef()
    },
    buildDefaultConstructor = function() {
      if(is.null(self$memberCppDefs[[name]])) {
        constructor <- cppFunctionClass$new(name = name,
                                            args = symbolTableClass$new(),
                                            code = cppCodeBlockClass$new(
                                                                       code = nParse(quote({})),
                                                                       symbolTable = symbolTableClass$new()
                                                                     ),
                                            initializerList = list(),
                                            returnType = cppBlank())
        self$memberCppDefs[[name]] <- constructor
      }
    },
    addSerialization = function() { #include_DLL_funs = FALSE) {
      addSerialization_impl(self) #, include_DLL_funs)
    }
    # , addLoadedObjectEnv = function() {
    #   addLoadedObjectEnv_impl(self)
    # }
  )
)

## A cppCodeBlock is an arbitrary collection of annotated syntax tree (exprClass)
## and other cppCodeBlocks (defined below).
## The parse tree can be either an R parse tree or one of our exprClass objects
## This class is used for the body (code) of a nFunctions.
## Note that it does not even inherit from cppDefinitionClass. It is entirely self-contained.
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

cppFunctionClass_init_impl <- function(cppDef) {
  cppDef$CPPincludes <- as.list(
    c(cppDef$CPPincludes,
      '<iostream>',
      nCompilerIncludeFile("nCompiler_omnibus_first_cpp.h"))
    #                      nCompilerIncludeFile("nCompiler_core.h"))
  )
}

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
                export = TRUE,
                initialize = function(...) {
                  self$name <- character()
                  cppFunctionClass_init_impl(self)
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
                        generateInitializerList(self$initializerList) ## We can add a symbolTable to use later if necessary
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

generateInitializerList <- function(initializerList) {
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
