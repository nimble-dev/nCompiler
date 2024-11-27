
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
