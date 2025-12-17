global_R_interface_cppDef <-
  cppManualClass$new(
    #                      Hincludes = c(nCompilerIncludeFile("nCompiler_class_interface.h"),
    #                                    nCompilerIncludeFile("shared_ptr_holder.h")
    #                                    ),
    #                      Hincludes = nCompilerIncludeFile("nCompiler_omnibus_first_h.h"),
    #CPPincludes = c(nCompilerIncludeFile("nCompiler_omnibus_first_cpp.h"),
    #  nCompilerIncludeFile("nC_inter/post_Rcpp/process_call_args.h")),
    Hpreamble = c("#define NCOMPILER_USES_NCLASS_INTERFACE",
                  "#define USES_NCOMPILER"),
    CPPpreamble = c("#define NCOMPILER_USES_NCLASS_INTERFACE",
                    "#define USES_NCOMPILER"),
    cppContent = paste0(
      "inline genericInterfaceBaseC *get_genericInterfaceBaseC(SEXP Xptr) {\n",
      "  return static_cast<genericInterfaceBaseC*>\n",
      "    (static_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Xptr))->get_ptr());\n",
      "}\n\n",

      "// This is completely generic, good for all derived classes\n",
      "// [[Rcpp::export]]\n",
      "SEXP get_value(SEXP Xptr, const std::string &name) {\n",
      "  genericInterfaceBaseC *obj =\n",
      "    get_genericInterfaceBaseC(Xptr);\n",
      "  //  std::cout << name << std::endl;\n",
      "  return(obj->get_value( name ));\n",
      "}\n\n",

      "// This is completely generic, good for all derived classes\n",
      "// [[Rcpp::export]]\n",
      "SEXP set_value(SEXP Xptr, Rcpp::Nullable<Rcpp::String> &name, SEXP Svalue) {\n",
      "  genericInterfaceBaseC *obj =\n",
      "    get_genericInterfaceBaseC(Xptr);\n",
      "  //std::cout << name << std::endl;\n",
      "  if(name.isNull()) {\n",
      "    obj->set_all_values( Svalue );\n",
      "  } else {\n",
      "  obj->set_value( Rcpp::as<std::string>(name), Svalue );\n",
      "  }\n",
      "  return(R_NilValue);\n",
      "}\n\n",

      "// This is completely generic, good for all derived classes\n",
      "// [[Rcpp::export]]\n",
      "SEXP call_method(SEXP Xptr, const std::string &name, SEXP Sargs) {\n",
      "  genericInterfaceBaseC *obj =\n",
      "    get_genericInterfaceBaseC(Xptr);\n",
      "  //  std::cout << name << std::endl;\n",
      "  return(obj->call_method( name, Sargs ));\n",
      "}\n"),
    name = "R_interfaces"
  )

get_R_interface_cppDef <- function() {
  global_R_interface_cppDef
}
