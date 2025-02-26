

inlineCxxPlugin_env <- new.env()

## This is a special name used by Rcpp::compileAttributes
## Rcpp:::.pluginIncludes, called within Rcpp::compileAttributes
## will ignore any includes result that does not have the
## suffix from Rcpp.plugin.maker, from \n\n#ifndef BEGIN_RCPP to using namespace Rcpp.
##
## It appeared to me that // [[Rcpp::plugins(some_plugin)]] is not used in compileAttributes,
##  even if registered via registerPlugin. But it may be that the plugin is called as long as
##  registerPlugin occurs in the package source code.
inlineCxxPlugin <- function(...) {
  uses_eigen <- !isFALSE(inlineCxxPlugin_env$uses_eigen)
  uses_nClass_interface <- !isFALSE(inlineCxxPlugin_env$uses_nClass_interface)
  uses_nList <- !isFALSE(inlineCxxPlugin_env$uses_nList)
  uses_cereal <- !isFALSE(inlineCxxPlugin_env$uses_cereal)
  uses_TBB <- FALSE # !isFALSE(inlineCxxPlugin_env$uses_TBB) # including here causes error due to #defining FALSE
  include.before <- character()
  if(uses_eigen) include.before <- paste0(include.before, "#define NCOMPILER_USES_EIGEN\n")
  if(uses_nClass_interface) include.before <- paste0(include.before, "#define NCOMPILER_USES_NCLASS_INTERFACE\n")
  if(uses_nList) include.before <- paste0(include.before, "#define NCOMPILER_USES_NLIST\n")
  if(uses_cereal) include.before <- paste0(include.before, "#define NCOMPILER_USES_CEREAL\n")
  if(uses_TBB) include.before <- paste0(include.before, "#define NCOMPILER_USES_TBB\n")
  include.before <- paste0(include.before, "#include <nCompiler/nCompiler_omnibus.h>")
  ans <- Rcpp::Rcpp.plugin.maker(include.before=include.before)()
  ans
}

nCompiler_pluginEnv <- new.env()

## Documentation to understand plugins is somewhat scattered.
## Reading inline::setupBuildEnvironment gives the impression that the
## Makevars entry of an Rcpp plugin is deprecated.  That appears
## to be the case.

make_nCompiler_plugin <- function(nCompiler_pluginEnv) {
  RcppDefaultPlugin <- Rcpp:::Rcpp.plugin.maker()
  force(nCompiler_pluginEnv)
  ans <- function(...) {
    result <- RcppDefaultPlugin(...)
    result$env$PKG_CPPFLAGS <- c(result$env$PKG_CPPFLAGS,
                                 if(length(nCompiler_pluginEnv$includePaths) > 0)
                                   paste0(
                                     "-I",
                                     nCompiler_pluginEnv$includePaths)
                                 else
                                   "")
 #   result$env$PKG_CXXFLAGS <- "-std=c++11"
    result$env$PKG_LIBS <- get_nCompLocal_PKG_LIBS_entry()
    ## Makevars doesn't work
    ## result$Makevars <- "CXX_STD=CXX11" does not seem to work
    result
  }
  ans
}

nCompiler_plugin <- make_nCompiler_plugin(nCompiler_pluginEnv)

make_nCompiler_Eigen_plugin <- function(nCompiler_pluginEnv) {
  force(nCompiler_pluginEnv)
  ans <- function(...) {
    result <- inline::getPlugin("RcppEigen", ...)
    ## The -Wno-invalid-partial-specialization is OS-specific
    ## and we hope it will be unnecessary once Eigen updates and RcppEigen updates.
    result$env$PKG_CPPFLAGS <- paste(result$env$PKG_CPPFLAGS,
                                     "-Wno-invalid-partial-specialization",
                                     "-Wno-unknown-pragmas",
                                     if(length(nCompiler_pluginEnv$includePaths) > 0)
                                       paste0(
                                         "-I",
                                         nCompiler_pluginEnv$includePaths)
                                     else
                                       "")
    # result$env$PKG_CXXFLAGS <- "-std=c++11"
    result$env$PKG_LIBS <- get_nCompLocal_PKG_LIBS_entry()
    if(isTRUE(get_nOption('compilerOptions')$throwEigenErrors)) {
      # replace include directives to enable Eigen errors
      #preamble = system.file(file.path('include', 'nCompiler',
      #                                 'nCompiler_Eigen_EnableErrors.h'),
      #                       package = 'nCompiler')
      #result$includes = readChar(preamble, file.info(preamble)$size)
      result$includes = "#define NCOMPILER_HANDLE_EIGEN_ERRORS"
    }
    if(isTRUE(get_nOption('compilerOptions')$cppStacktrace)) {
      # add include directives to add stack basic traces
      # This could be integrated more with the rest of the include
      # system, but it is fairly stand-alone and works here for now.
      result$includes = c(result$includes,
                          '#include <nCompiler/nCompiler_stacktrace.h>')
    }
    result
  }
  ans
}

nCompiler_Eigen_plugin <- make_nCompiler_Eigen_plugin(nCompiler_pluginEnv)
