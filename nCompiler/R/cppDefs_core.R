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
                                       do.call("c", lapply(neededCppDefs, function(x) x$getDefs()) ) ) )} 
    )
)

cppMacroCallClass <- R6::R6Class(
    'cppMacroCallClass',
    portable = FALSE,
    inherit = cppDefinitionClass,
    public = list(
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
            objectDefs[[newName]] <<- newObj
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
    self$Hincludes <- c(self$Hincludes,
                       nCompilerIncludeFile("nCompiler_class_factory.h"))
    ## The C++ function loadedObjectEnv simply calls new.loadedObjectEnv in R
    returnLine <-  paste0("return(loadedObjectEnv(new_nCompiler_object<",
                          self$name,
                          ">()));")
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
      cppMethodNames <- lapply(self$cppFunctionDefs, function(x) x$name)
      methodsContent <- paste0("method(\"",
                               methodNames[includeBool],
                               "\", &",
                               name,
                               "::",
                               cppMethodNames[includeBool],
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

addSerialization_impl <- function(self) {

    ## This function adds a C++ method like:
    ##   template<class Archive>
    ##   void _SERIALIZE_(Archive & archive) {
    ##   archive(cereal::base_class<genericInterfaceC<fooC> >(this),
    ##           CEREAL_NVP(x),
    ##           CEREAL_NVP(y));
    ## }

    ## construct the central call to archive:
    namesToArchive <- self$symbolTable$getSymbolNames()
    codeText <- paste0(
        "archive(\ncereal::base_class<genericInterfaceC<",self$name,"> >(this),\n",
        paste0("CEREAL_NVP(", namesToArchive, ")", collapse = ",\n"),
        "\n);\n"
    )
    allCodeList <-
        list(
            substitute(cppLiteral(CODETEXT),
                       list(CODETEXT = as.character(codeText)))
        )
    allCode <- putCodeLinesInBrackets(allCodeList)
    allCode <- nParse(allCode)

    ## construct a cppFunctionClass
    serialize_method <- cppFunctionClass$new(
        name = "_SERIALIZE_"
        , template = cppTemplateDeclaration("Archive")
        , args = symbolTableClass$new(
            symbols = list(
                archive = cppVarClass$new(
                    name = "archive",
                    baseType = "Archive",
                    ref = TRUE
                )
            )
        )
        , code = cppCodeBlockClass$new(
            code = allCode,
            symbolTable = symbolTableClass$new(),
            skipBrackets = TRUE
            )
      , returnType = nCompiler:::cppVoid()
    )
    self$cppFunctionDefs[["_SERIALIZE_"]] <- serialize_method
    
    ## The next code creates lines to force the needed specializations of the
    ## template methods above.  These lines will look like:
    ## template void fooC::_SERIALIZE_(cereal::BinaryOutputArchive &archive);
    ## template void fooC::_SERIALIZE_(cereal::BinaryInputArchive &archive);
    cereal_template_instantiations <-
        cppMacroCallClass$new(
            hContent =
                paste0("template void ",
                       self$name,
                       "::_SERIALIZE_(cereal::",
                       c("BinaryOutputArchive", "BinaryInputArchive"),
                       " &archive);",
                       collapse = "\n")
            )
    self$neededCppDefs[["cereal_template_instantiations"]] <-
        cereal_template_instantiations

    ## A line like:
    ## CEREAL_REGISTER_TYPE(fooC)
    cereal_register_type_macro <-
        cppMacroCallClass$new(
            cppContent = paste0("CEREAL_REGISTER_TYPE(", self$name, ")\n")
        )
    self$neededCppDefs[["cereal_register_type_macro"]] <- cereal_register_type_macro

    ## Lines to force dynamic initialization
    cereal_dynamic_init <-
        cppMacroCallClass$new(
            cppContent = paste0("CEREAL_REGISTER_DYNAMIC_INIT(", self$name, ")\n"),
            hContent = paste0("CEREAL_FORCE_DYNAMIC_INIT(", self$name, ")\n")
        )
    self$neededCppDefs[["cereal_dynamic_init"]] <- cereal_dynamic_init

    self$Hpreamble <- c(self$Hpreamble, "#define _INCLUDE_SERIALIZE_AND_DESERIALIZE_FUNCTIONS\n")
    serialize_deserialize <-
      cppMacroCallClass$new(
        cppContent = paste0("//[[Rcpp::export]]\n",
                            "RawVector nComp_serialize(SEXP Sfrom) {\n",
                            "genericInterfaceBaseC *baseobj =\n",
                            "reinterpret_cast<genericInterfaceBaseC*>(reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Sfrom))->get_ptr());\n",
                            "std::unique_ptr<genericInterfaceBaseC> shared_baseobj(baseobj);\n",
                            "std::stringstream ss;\n",
                            "{\n",
                            "cereal::BinaryOutputArchive oarchive(ss);\n",
                            "oarchive(shared_baseobj);\n",
                            "}\n",
                            "shared_baseobj.release();\n",
                            "ss.seekg(0, ss.end);\n",
                            "RawVector retval(ss.tellg());\n",
                            "ss.seekg(0, ss.beg);\n",
                            "ss.read(reinterpret_cast<char*>(&retval[0]), retval.size());\n",
                            "return retval;\n",
                            "}\n",
                            "\n",
                            "//[[Rcpp::export]]\n",
                            "SEXP nComp_deserialize(RawVector src) {\n",
                            "  std::stringstream ss;\n",
                            "  ss.write(reinterpret_cast<char*>(&src[0]), src.size());\n",
                            "  ss.seekg(0, ss.beg);\n",
                            "  std::unique_ptr<genericInterfaceBaseC> shared_baseobj;\n",
                            "  {\n",
                            "    cereal::BinaryInputArchive iarchive(ss);\n",
                            "    iarchive(shared_baseobj);\n",
                            "  }\n",
                            paste0("std::shared_ptr<", self$name,
                                   "> shared(dynamic_cast<", self$name, "*>(shared_baseobj.release()));\n"),
                            paste0("SEXP Sans = PROTECT(return_nCompiler_object<", self$name ,">(shared));\n"),
                            "UNPROTECT(1);\n",
                            "return(Sans);\n",
                            "}\n"
        )
      )
    self$neededCppDefs[["cereal_serialize_deserialize"]] <- serialize_deserialize
    
    ## Was this a test or a way to avoid a possibly empty class?
    dummy <-
        cppMacroCallClass$new(
            cppContent = paste0("int dummy;\n"),
            hContent = paste0("extern int dummy;\n")
        )
    self$neededCppDefs[["dummy"]] <- dummy
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
        functionNamesForInterface = character(),
        variableNamesForInterface = character(),
        ##SEXPfinalizerFun = 'ANY',
        # globalObjectsDefs = list(),
        
        initialize = function(...) {
            ##useGenerator <<- TRUE
            Hincludes <<-	c(Hincludes, '<Rinternals.h>')	
            CPPincludes <<-	c(CPPincludes, '<iostream>') 
            super$initialize(...)
        },
        getHincludes = function() {
            Hinc <- c(Hincludes,
                      if(!is.null(SEXPgeneratorDef))
                          SEXPgeneratorDef$getHincludes(),
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
                       list(self, SEXPgeneratorDef)
                   } else {
                       list(self)
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
    addGenericInterface = function() {
        addGenericInterface_impl(self)
    },
    addSerialization = function() {
        addSerialization_impl(self)
    }
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
                  constructorContent = NULL,
                  commentsAbove = character(),
                  initialize = function(...) {
                      self$name <- character()
                      self$CPPincludes <- as.list(
                          c(self$CPPincludes,
                            '<iostream>')
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
                        paste(
                          generateFunctionHeader(self$returnType,
                                                 self$name,
                                                 argsListToUse,
                                                 scopes,
                                                 self$template,
                                                 static = FALSE,
                                                 ...),
                          if(self$const)
                            ' const '
                          else
                            character(0),
                          
                          if(!is.null(self$constructorContent))
                            generatorConstructorContent(self$constructorContent)
                          else
                            character(0),
                          
                          '{'
                        ), ## end paste
                        self$code$generate(...),
                        list('}')
                      )## end c()
                  }
                  )
)

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
