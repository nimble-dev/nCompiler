# instructions for re-generating predefined files.
# library(nCompiler)

nCompiler::nCompile(nCompiler::nListBase_nClass, control = list(generate_predefined = TRUE))
# The #include <nCompiler/predef/nList_/nList_.h> after the class declaration
# is now generated from compileInfo$HpostCode, so no manual edit is needed.
#
# optional code to test
library(nCompiler)
nL1 <- nList("integerVector()")
comp <- nCompile(nL1)
obj <- comp$new()
length(obj) <- 3
obj[[1]] <- 1:3
testthat::expect_equal(obj |> as.list(), list(1:3, integer(), integer()))
testthat::expect_equal(obj$get_nDim(), 1L)
testthat::expect_equal(obj$get_elementType(), "integer")
