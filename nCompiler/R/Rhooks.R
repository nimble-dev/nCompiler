## for .onLoad
.onLoad <- function(...) {
    Rcpp::registerPlugin("nCompiler_plugin", nCompiler:::nCompiler_plugin)
    Rcpp::registerPlugin("nCompiler_Eigen_plugin", nCompiler:::nCompiler_Eigen_plugin)
}
