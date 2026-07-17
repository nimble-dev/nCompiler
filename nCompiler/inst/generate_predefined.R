# instructions for re-generating predefined files.
library(nCompiler)

nCompile(nListBase_nClass, control = list(generate_predefined = TRUE))
# Manually add
# #include <nCompiler/predef/nList_/nList_.h>
# to the nListBase_nClass header file after the class declaration.
#
# optional code to test
nL1 <- nList("integerVector()")
comp <- nCompile(nL1)
obj <- comp$new()
length(obj) <- 3
obj[[1]] <- 1:3
expect_equal(obj |> as.list(), list(1:3, integer(), integer())
