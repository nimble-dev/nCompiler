# miscellaneous tests such as regression tests of corner-case bugs
library(nCompiler)

foo <- nFunction(
  function(m = 'numericMatrix') {
    #D <- DEBUG_EIGEN(diag(dim(m)[1]))
    D <- diag(dim(m)[1])
    return(D)
    returnType('numericMatrix')
  }
)

debug(nCompiler:::compile_eigenize)
cfoo <- nCompile(foo)
