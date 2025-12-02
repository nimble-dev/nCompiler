# This is called from nCompile or nCompile_nClass

global_serialization_cppDef <-
  cppManualClass$new(
    Hpreamble = c(#nCompiler_plugin()$includes,
      "#define NCOMPILER_USES_CEREAL",
      "#define NCOMPILER_USES_NCLASS_INTERFACE",
      "#define USES_NCOMPILER"),
    CPPpreamble = c(#nCompiler_plugin()$includes,
      "#define NCOMPILER_USES_CEREAL",
      "#define NCOMPILER_USES_NCLASS_INTERFACE",
      "#define USES_NCOMPILER"),
    ## Hincludes = c("<Rinternals.h>",
    ##           nCompilerIncludeFile("nCompiler_class_interface.h"),
    ##           nCompilerIncludeFile("nCompiler_loadedObjectsHook.h"),
    ##           nCompilerIncludeFile("nCompiler_serialization_mgr.h")),
    ## Hincludes = nCompilerIncludeFile("nCompiler_omnibus_first_h.h"),
    #
    # This is done here because it must be done once and only once to avoid
    # duplicate symbols when compiling as a package.
    CPPincludes = c(#nCompilerIncludeFile("nCompiler_omnibus_first_cpp.h"),
                    nCompilerIncludeFile("nC_cereal/post_Rcpp/serialization_mgr.h")),
    CPPusings = c("using namespace Rcpp;",
                  "// [[Rcpp::plugins(nCompiler_plugin)]]",
                  "// [[Rcpp::depends(nCompiler)]]",
                  "// [[Rcpp::depends(Rcereal)]]"),
    cppContent = paste0(
      "// [[Rcpp::export]]\n",
      "SEXP new_serialization_mgr ( ) {\n",
      "return CREATE_NEW_NCOMP_OBJECT(serialization_mgr);\n",
      "}\n",
      "\n",
      "//[[Rcpp::export]]\n",
      "RawVector ", "nComp_serialize_", ## name of serialization function here
      "(SEXP Sfrom) {\n",
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
      "SEXP ", "nComp_deserialize_", ## name of deserialization function here
      "(RawVector src) {\n",
      "  std::stringstream ss;\n",
      "  ss.write(reinterpret_cast<char*>(&src[0]), src.size());\n",
      "  ss.seekg(0, ss.beg);\n",
      "  std::unique_ptr<genericInterfaceBaseC> shared_baseobj;\n",
      "  {\n",
      "    cereal::BinaryInputArchive iarchive(ss);\n",
      "    iarchive(shared_baseobj);\n",
      "  }\n",
      ## paste0("std::shared_ptr<", self$name,
      ##        "> shared(dynamic_cast<", self$name, "*>(shared_baseobj.release()));\n"),
      ## paste0("SEXP Sans = PROTECT(return_nCompiler_object<", self$name ,">(shared));\n"),
      "SEXP Sans = PROTECT((shared_baseobj.release())->make_deserialized_return_SEXP());\n",
      "UNPROTECT(1);\n",
      "return(Sans);\n",
      "}\n"
    ),
    name = "serialization" # internal name of cppDef here
  )

get_serialization_cppDef <- function() {
  global_serialization_cppDef
}

# This is called from cppClassClass$addSerialization
addSerialization_impl <- function(self) { #},
                                  # include_DLL_funs = FALSE) {
  ## This function adds a C++ method like:
  ##   template<class Archive>
  ##   void _SERIALIZE_(Archive & archive) {
  ##   archive(cereal::base_class<genericInterfaceC<fooC> >(this),
  ##           CEREAL_NVP(x),
  ##           CEREAL_NVP(y));
  ## }
  self$Hpreamble = c(self$Hpreamble,
                     "#define NCOMPILER_USES_CEREAL",
                     "#define USES_NCOMPILER")
  self$CPPpreamble = c(self$CPPpreamble,
                       "#define NCOMPILER_USES_CEREAL",
                       "#define USES_NCOMPILER")

  ## construct the central call to archive:
  ## namesToArchive <- self$symbolTable$getSymbolNames()
  namesToArchive <- NCinternals(self$Compiler$NCgenerator)$cppSymbolNames
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
, returnType = cppVoid()
)
  self$memberCppDefs[["_SERIALIZE_"]] <- serialize_method

  ##
  inner_code <- substitute(cppLiteral(MSG),
                           list(MSG = paste0(
                                  "RETURN_THIS_NCOMP_OBJECT(",self$name,");\n"
                                )))
  make_deserialized_return_SEXP_method <- cppFunctionClass$new(
    name = "make_deserialized_return_SEXP"
  , args = symbolTableClass$new()
   , code = cppCodeBlockClass$new(
     code = nParse(inner_code),
     symbolTable = symbolTableClass$new(),
     skipBrackets = TRUE
   )
   , returnType = cppSEXP()
  )
  self$memberCppDefs[["make_deserialized_return_SEXP"]] <- make_deserialized_return_SEXP_method
  
  ## The next code creates lines to force the needed specializations of the
  ## template methods above.  These lines will look like:
  ## template void fooC::_SERIALIZE_(cereal::BinaryOutputArchive &archive);
  ## template void fooC::_SERIALIZE_(cereal::BinaryInputArchive &archive);
  cereal_template_instantiations <-
    cppManualClass$new(
      hContent =
        paste0("template void ",
               self$name,
               "::_SERIALIZE_(cereal::",
               c("BinaryOutputArchive", "BinaryInputArchive"),
               " &archive);",
               collapse = "\n")
    )
  self$internalCppDefs[["cereal_template_instantiations"]] <-
    cereal_template_instantiations

  ## A line like:
  ## CEREAL_REGISTER_TYPE(fooC)
  cereal_register_type_macro <-
    cppManualClass$new(
      cppContent = paste0("CEREAL_REGISTER_TYPE(", self$name, ")\n",
                          "CEREAL_REGISTER_TYPE(shared_ptr_holder<",self$name,">)\n",
                          "CEREAL_REGISTER_TYPE(genericInterfaceC<",self$name,">)\n",
                          "CEREAL_REGISTER_TYPE(loadedObjectHookC<",self$name,">)\n")
      )
  self$internalCppDefs[["cereal_register_type_macro"]] <- cereal_register_type_macro

  ## Lines to force dynamic initialization
  cereal_dynamic_init <-
    cppManualClass$new(
      cppContent = paste0("CEREAL_REGISTER_DYNAMIC_INIT(", self$name, ")\n"),
      hContent = paste0("CEREAL_FORCE_DYNAMIC_INIT(", self$name, ")\n")
    )
  self$internalCppDefs[["cereal_dynamic_init"]] <- cereal_dynamic_init

  self$Hpreamble <- c(self$Hpreamble, "#define _INCLUDE_SERIALIZE_AND_DESERIALIZE_FUNCTIONS\n")
  
  cleanname <- Rname2CppName(self$name)
  # if (!identical(cleanname, self$name)) 
  #   warning("When serializing nClass ", self$name, " name for serialization ",
  #           " functions was changed to ", cleanname)
  # if(include_DLL_funs)
  self$externalCppDefs[["cereal_serialize_deserialize"]] <- get_serialization_cppDef()
  
  ## Was this a test or a way to avoid a possibly empty class?
  dummy <-
    cppManualClass$new(
      cppContent = paste0("#ifndef __dummy__\n#define __dummy__\n int dummy;\n#endif"),
      hContent = paste0("extern int dummy;\n")
    )
  self$internalCppDefs[["dummy"]] <- dummy
  invisible(NULL) 
}
