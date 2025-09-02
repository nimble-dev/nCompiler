#library(nCompiler)
library(testthat)
args <- R.utils::cmdArgs()
if(!is.null(args$pkgName))
  library(args$pkgName, lib.loc = args$lib, character.only=TRUE)
savefile <- args$savefile
if(!is.null(savefile))
  load(file=savefile)
source(args$codefile)
