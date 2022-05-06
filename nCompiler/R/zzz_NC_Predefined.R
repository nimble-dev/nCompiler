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

#' @export
SVDDecomp <- nClass(
  classname = 'SVDDecomp',
  predefined = "SVDDecomp_pkg",
  Cpublic = list(
    d = 'numericVector',
    v = 'numericMatrix',
    u = 'numericMatrix'
  )
)


#' @export
OptimControlList <- nClass(
  classname = 'OptimControlList',
  predefined = "OptimControlList_pkg",
  Cpublic = list(
    trace = 'integer',
    fnscale = 'double',
    parscale = 'numericVector',
    ndeps = 'numericVector',
    maxit = 'integer', 
    abstol = 'double',
    reltol = 'double',
    alpha = 'double',
    beta = 'double',
    gamma = 'double',
    REPORT = 'integer',
    type = 'integer',
    lmm = 'integer',
    factr = 'double',
    pgtol = 'double',
    tmax = 'integer',
    temp = 'double',
    # method to set members to default values
    initToDefaults = nFunction(
      fun = function() {
        trace <- 0
        fnscale <- 1
        parscale <- nNumeric(length = 1, value = 1.0)
        ndeps <- nNumeric(length = 1, value = 1e-3)
        abstol <- -Inf
        cppLiteral('reltol = std::sqrt(std::numeric_limits<double>::epsilon());')
        cppLiteral('maxit = NA_INTEGER;')
        alpha <- 1.0
        beta <- 0.5
        gamma <- 2.0
        REPORT <- 10
        type <- 1
        lmm <- 5
        factr <- 1e7
        pgtol <- 0
        tmax <- 10
        temp <- 10.0
      }
    )
    
  )
)

#' @export
OptimResultList <- nClass(
  classname = 'OptimResultList',
  predefined = "OptimResultList_pkg",
  Cpublic = list(
    par = 'numericVector',
    value = 'double',
    hessian = 'numericMatrix',
    counts = 'integerVector',
    convergence = 'integer',
    message = 'RcppCharacterVector'
  )
)
