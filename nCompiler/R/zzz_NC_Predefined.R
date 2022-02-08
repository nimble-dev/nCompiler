##
test_predefined <- nClass(
  classname = "test_predefined",
  Cpublic = list(
    a = 'numericScalar'
  )
)

C_test_predefined <- build_compiled_nClass(test_predefined,
                                               function() new_test_predefined())

## need to determine how Eigen tensor's get seen by on-the-fly compilation,
## because they aren't seen by a predefined.

#' @export
nC_derivClass <- nClass(
  classname = "nC_derivClass",
  Cpublic = list(
      value = 'numericVector',
      gradient = 'numericMatrix',
      hessian = 'numericArray(nDim = 3)'
  )
)

#' @export
C_nC_derivClass <- build_compiled_nClass(nC_derivClass,
                                             function() new_nC_derivClass())

#' @export
EigenDecomp <- nClass(
  classname = 'EigenDecomp',
  Cpublic = list(
    values = 'numericVector',
    vectors = 'numericMatrix'
  )
)

#' @export
C_EigenDecomp <- build_compiled_nClass(EigenDecomp,
                                         function() new_EigenDecomp())
