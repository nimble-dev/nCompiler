# predefined classes can be included as compilation objection in a call
# to nCompile just like others.
# The difference is that pre-existing C++ code will be used to implement them.
# Include control list element generate_predefined = TRUE to
# use newly generated code instead of the pre-existing code.
# However, code in the C++ headers that uses the predefined might then break.

##
#' @export
test_predefined <- nClass(
  predefined = "test_predefined_pkg",
  classname = "test_predefined",
  Cpublic = list(
    a = 'numericScalar'
  )
)

## C_test_predefined <- build_compiled_nClass(test_predefined,
##                                                function() new_test_predefined())

## need to determine how Eigen tensors get seen by on-the-fly compilation,
## because they aren't seen by a predefined.

#' @export
derivClass <- nClass(
  classname = "derivClass",
  predefined = "derivClass_pkg",
  Cpublic = list(
      value = 'numericVector',
      gradient = 'numericMatrix',
      hessian = 'numericArray(nDim = 3)'
  )
)

## #' @export
## C_nC_derivClass <- build_compiled_nClass(nC_derivClass,
##                                              function() new_nC_derivClass())

#' @export
EigenDecomp <- nClass(
  classname = 'EigenDecomp',
  predefined = "EigenDecomp_pkg",
  Cpublic = list(
    values = 'numericVector',
    vectors = 'numericMatrix'
  )
)

## #' @export
## C_EigenDecomp <- build_compiled_nClass(EigenDecomp,
##                                          function() new_EigenDecomp())
