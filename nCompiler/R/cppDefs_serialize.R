
# Returns the names of the internally-generated serialization helper functions.
getSerialFunNames <- function() {
  c("nComp_serialize_", "nComp_deserialize_", "new_serialization_mgr")
}



# This is called from nCompile or nCompile_nClass
make_serialization_cppDef <- function(funNames = getSerialFunNames(),
                                      defName = "serialization") {
  ans <-
    cppMacroCallClass$new(
      Hincludes = nCompilerIncludeFile("nCompiler_serialization_mgr.h"),
      cppContent = paste0(
        "// [[Rcpp::export]]\n",
        "SEXP ", funNames[3], " ( ) {\n",
        "return(loadedObjectEnv(new_nCompiler_object<serialization_mgr>()));\n",
        "}\n",
        "\n",
        "//[[Rcpp::export]]\n",
                          "RawVector ", funNames[1],
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
                          "SEXP ", funNames[2],
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
      name = defName
    )
  ans
}

# This is called from cppClassClass$addSerialization
addSerialization_impl <- function(self,
                                  include_DLL_funs = FALSE) {
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

  ##
  make_deserialized_return_SEXP_method <- nCompiler:::cppFunctionClass$new(
    name = "make_deserialized_return_SEXP"
  , args = nCompiler:::symbolTableClass$new()
   , code = nCompiler:::cppCodeBlockClass$new(
     code = nCompiler:::nParse(nCompiler:::cppLiteral(
       paste0(
         "std::shared_ptr<", self$name,"> shared(this);\n",
         "SEXP Sans = PROTECT(return_nCompiler_object<", self$name ,
         ">(shared));\n",
         "UNPROTECT(1);\n",
         "return Sans;"
       ))),
     symbolTable = nCompiler:::symbolTableClass$new(),
     skipBrackets = TRUE
   )
   , returnType = nCompiler:::cppSEXP()
  )
  self$cppFunctionDefs[["make_deserialized_return_SEXP"]] <- make_deserialized_return_SEXP_method
  
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
  
  cleanname <- Rname2CppName(self$name)
  # if (!identical(cleanname, self$name)) 
  #   warning("When serializing nClass ", self$name, " name for serialization ",
  #           " functions was changed to ", cleanname)
  if(include_DLL_funs)
    self$neededCppDefs[["cereal_serialize_deserialize"]] <- make_serialization_cppDef()
  
  ## Was this a test or a way to avoid a possibly empty class?
  dummy <-
    cppMacroCallClass$new(
      cppContent = paste0("#ifndef __dummy__\n#define __dummy__\n int dummy;\n#endif"),
      hContent = paste0("extern int dummy;\n")
    )
  self$neededCppDefs[["dummy"]] <- dummy
  invisible(NULL) 
}
