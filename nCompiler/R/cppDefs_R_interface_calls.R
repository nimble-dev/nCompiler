global_R_interface_cppDef <-
  cppMacroCallClass$new(
                      Hincludes = c(nCompilerIncludeFile("nCompiler_class_interface.h"),
                                    nCompilerIncludeFile("shared_ptr_holder.h")
                                    ),
                      cppContent = paste0(
                        "inline genericInterfaceBaseC *get_genericInterfaceBaseC(SEXP Xptr) {\n",
                        "  return reinterpret_cast<genericInterfaceBaseC*>\n",
                        "    (reinterpret_cast<shared_ptr_holder_base*>(R_ExternalPtrAddr(Xptr))->get_ptr());\n",
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
                        "SEXP set_value(SEXP Xptr, const std::string &name, SEXP Svalue) {\n",
                        "  genericInterfaceBaseC *obj =\n",
                        "    get_genericInterfaceBaseC(Xptr);\n",
                        "  //std::cout << name << std::endl;\n",
                        "  obj->set_value( name, Svalue );\n",
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
