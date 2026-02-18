.onLoad <- function(libname, pkgname) {
    nCompiler::registerOpDef(opDefs)
    packageStartupMessage("This is my test package.")
}

