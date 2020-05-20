# Utility to check status of Rcpp

check_Rcpp_for_nCompiler <- function(dir = tempdir(),
                                     verbose = TRUE,
                                     errorOnFail = FALSE) {
  emit <- if(errorOnFail) stop else message
  if(!require(Rcpp)) {
    if(verbose) emit("Rcpp is not installed.")
    return(FALSE)
  }
  curDir <- getwd()
  on.exit(setwd(curDir))
  setwd(dir)
  filename <- "check_Rcpp_for_nCompiler.cpp" 
  zz <- file(filename, open = "w")
  cppCode <- paste(
    "// [[Rcpp::export]]",
    "double RcppSquare_test(double x) {",
    " double ans = x*x;",
    " return(ans);",
    "}",
    sep = "\n"
  )
  writeLines(cppCode, con = zz)
  close(zz)
  sourceCpp(filename, env = environment())
  if(!exists("RcppSquare_test", inherits = FALSE)) {
    if(verbose) emit("Rcpp::sourceCpp did not work.")
    return(FALSE)
  }
  check <- RcppSquare_test(2.5)
  if(check != 2.5*2.5) {
    if(verbose) emit("Rcpp::sourceCpp compiled but does not seem to work.")
    return(FALSE)
  }
  TRUE
}
