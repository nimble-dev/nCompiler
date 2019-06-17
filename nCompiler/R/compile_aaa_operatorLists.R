## Each operator will have a list of information for each compiler stage.
## The information for each stage will be a list.

## Check in which handlers below returnTypeCode is actually used

returnTypeCodes <- list(
    double = 1L,
    integer = 2L,
    logical = 3L,
    promote = 4L,
    promoteNoLogical = 5L)

operatorDefEnv <- new.env()
##Utility for assigning multiples
assignOperatorDef <- function(ops, def) {
    for(op in ops)
        operatorDefEnv[[op]] <- def
}

updateOperatorDef <- function(op, field, key, val) {
  operatorDefEnv[[op]][[field]][[key]] <- val
}

getOperatorDef <- function(op, field = NULL, key = NULL) {
  opInfo <- operatorDefEnv[[op]]
  if (is.null(opInfo) || is.null(field)) return(opInfo)
  if (is.null(key)) return(opInfo[[field]])
  return(opInfo[[field]][[key]])
}

assignOperatorDef(
  'nFunction',
  list(
    simpleTransformations = list(
      handler = 'Generic_nFunction'),
    labelAbstractTypes = list(
      handler = 'Generic_nFunction'),
    cppOutput = list(
      handler = 'Generic_nFunction')
  )
)

assignOperatorDef(
  c('if', 'while'),
  list(
    labelAbstractTypes = list(
      handler = 'recurse_labelAbstractTypes'),
    cppOutput = list(
      handler = 'IfOrWhile')
  )
)

assignOperatorDef(
  c('for'),
  list(
    labelAbstractTypes = list(
      handler = 'For'),
    cppOutput = list(
      handler = 'For')
  )
)

assignOperatorDef(
  c(':'),
  list(
    labelAbstractTypes = list(
      handler = 'Colon'),
    eigenImpl = list(
      toEigen = 'Maybe'
    )
  )
)

assignOperatorDef(
  c('['),
  list(
    labelAbstractTypes = list(
      handler = 'IndexingBracket'
    ),
    eigenImpl = list(
      handler = 'IndexingBracket'
    ),
    cppOutput = list(
      handler = 'IndexingBracket')
  )
)

assignOperatorDef(
  c('index('),
  list(
    cppOutput = list(
      handler = 'IndexingParen')
  )
)


assignOperatorDef(
  c('<-','<<-','='),
  list(
    labelAbstractTypes = list(
      handler = 'Assign'),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'Assign'
    ),
    cppOutput = list(
      handler = 'MidOperator',
      cppString = ' = ')
  )
)

assignOperatorDef(
    'return',
    list(
        labelAbstractTypes = list(
            handler = 'Return'),
        eigenImpl = list(
          toEigen = 'Yes'
        ),
        cppOutput = list(
            handler = 'Return')
    )
)

assignOperatorDef(
  '::',
  list(cppOutput = list(handler = 'MidOperator'))
)

assignOperatorDef(
  'chainedCall',
  list(cppOutput = list(handler = 'chainedCall'))
)

assignOperatorDef(
    c('+','-'),
    list(
        labelAbstractTypes = list(
          handler = 'BinaryUnaryCwise',
          returnTypeCode = returnTypeCodes$promoteNoLogical),
        eigenImpl = list(
          toEigen = 'Yes',
            handler = 'cWiseAddSub'),
        cppOutput = list(
          handler = 'BinaryOrUnary'),
        testthat = list(
          isBinary = TRUE,
          testMath = TRUE,
          testAD = TRUE)
    )
)
updateOperatorDef('-', 'testthat', 'isUnary', TRUE)
## add descriptive name for filenaming gold files
updateOperatorDef('-', 'testthat', 'alpha_name', 'minus')
updateOperatorDef('+', 'testthat', 'alpha_name', 'plus')

assignOperatorDef(
  c('min', 'max'),
  list(
    simpleTransformations = list(
      handler = 'minMax'),
    labelAbstractTypes = list(
      toEigen = 'Yes',
      handler = 'UnaryReduction',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      handler = 'Reduction',
      method = TRUE),
    cppOutput = list(),
    testthat = list(
      isUnary = TRUE,
      testMath = TRUE)
  )
)
updateOperatorDef('max', 'cppOutput', 'cppString', 'maximum')
updateOperatorDef('min', 'cppOutput', 'cppString', 'minimum')

assignOperatorDef(
  c('pmin', 'pmax'),
  list(
    labelAbstractTypes = list(
      toEigen = 'Yes',
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      handler = 'cWiseBinary',
      method = TRUE),
    cppOutput = list(),
    testthat = list(
      isBinary = TRUE,
      testMath = TRUE)
  )
)
updateOperatorDef('pmax', 'cppOutput', 'cppString', 'cwiseMax')
updateOperatorDef('pmin', 'cppOutput', 'cppString', 'cwiseMin')

assignOperatorDef(
  'eigencast',
  list(
    help = 'eigencast(A, type) is for A.cast<type>().',
    cppOutput = list(
      handler = 'EigenCast'
    )
  )
)

assignOperatorDef(
  '.method',
  list(
    help = '.method(A, foo, b) is for A.foo(b).',
    cppOutput = list(
      handler = 'Method'
    )
  )
)

assignOperatorDef(
  '.member',
  list(
    help = '.member(A, b) is for A.b.',
    cppOutput = list(
      handler = 'Member'
    )
  )
)


assignOperatorDef(
  'cppLiteral',
  list(
    help = 'cppLiteral("x = y;") inserts x = y; directly into the C++ output.',
    labelAbstractTypes = list(handler = 'Literal'),
    cppOutput = list(handler = 'Literal')
  )
)

assignOperatorDef(
  c('mean', 'prod', 'squaredNorm'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryReduction',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'Reduction',
      method = TRUE
    ),
    cppOutput = list(
      handler = 'TensorReduction'
    ),
    testthat = list(
      isUnary = TRUE,
      testMath = TRUE)
  )
)

assignOperatorDef(
  c('all', 'any'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryReduction',
      returnTypeCode = returnTypeCodes$logical),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'Reduction',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    ),
    testthat = list(
      isUnary = TRUE,
      testMath = TRUE)
  )
)

assignOperatorDef(
  c('exp', 'inverse', 'lgamma', 'log', 'rsqrt', 'sqrt', 'tanh'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseUnary',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    ),
    testthat = list(
      isUnary = TRUE,
      testMath = TRUE,
      testAD = TRUE)
  )
)

assignOperatorDef(
  c('abs', 'cube', 'square'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseUnary',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    ),
    testthat = list(
      isUnary = TRUE,
      testMath = TRUE,
      testAD = TRUE)
  )
)

assignOperatorDef(
  c('atan', 'logit'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseUnary_external',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    ),
    testthat = list(
      isUnary = TRUE,
      testMath = TRUE,
      testAD = TRUE)
  )
)

## binaryMidLogicalOperatorsComparison
assignOperatorDef(
  c('==','!=','<=','>=','<','>','&','|'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwiseLogical',
      returnTypeCode = returnTypeCodes$logical),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseBinaryLogical'),
    cppOutput = list(
      handler = 'MidOperator'),
    testthat = list(
      isBinary = TRUE,
      testMath = TRUE)
  )
)
updateOperatorDef('<=', 'eigenImpl', 'swapOp', '>=')
updateOperatorDef('>=', 'eigenImpl', 'swapOp', '<=')
updateOperatorDef('<', 'eigenImpl', 'swapOp', '>')
updateOperatorDef('>', 'eigenImpl', 'swapOp', '<')
updateOperatorDef('&', 'cppOutput', 'cppString', ' && ')
updateOperatorDef('|', 'cppOutput', 'cppString', ' || ')
updateOperatorDef('==', 'testthat', 'alpha_name', 'eq')
updateOperatorDef('!=', 'testthat', 'alpha_name', 'neq')
updateOperatorDef('<=', 'testthat', 'alpha_name', 'le')
updateOperatorDef('>=', 'testthat', 'alpha_name', 'ge')
updateOperatorDef('<', 'testthat', 'alpha_name', 'lt')
updateOperatorDef('>', 'testthat', 'alpha_name', 'gt')
updateOperatorDef('&', 'testthat', 'alpha_name', 'and')
updateOperatorDef('|', 'testthat', 'alpha_name', 'or')

assignOperatorDef(
  c('/'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseMultDiv'),
    cppOutput = list(
      handler = 'MidOperator'),
    testthat = list(
      isBinary = TRUE,
      testMath = TRUE,
      testAD = TRUE)
  )
)
updateOperatorDef('/', 'testthat', 'alpha_name', 'div')

assignOperatorDef(
  c('*'),
  list(
    returnTypeHandling = list(
      code = returnTypeCodes$promoteNoLogical),
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseMultDiv'),
    cppOutput = list(
      handler = 'MidOperator'),
    testthat = list(
      isBinary = TRUE,
      testMath = TRUE,
      testAD = TRUE)
  )
)
updateOperatorDef('*', 'testthat', 'alpha_name', 'mult')

assignOperatorDef(
  c('^'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseByScalar', ## Eigen::Tensor requires the rhs of pow to be scalar
      method = TRUE),
    cppOutput = list(
      cppString = 'pow',
      handler = ''),
    testthat = list(
      isBinary = TRUE,
      testMath = TRUE,
      testAD = TRUE)
  )
)
updateOperatorDef('^', 'testthat', 'alpha_name', 'pow')

assignOperatorDef(
  c('%%'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'cWiseByScalar'), ## Eigen::Tensor requires the rhs of % to be scalar
    cppOutput = list(
      handler = 'MidOperator',
      cppString = ' % '),
    testthat = list(
      isBinary = TRUE,
      testMath = TRUE)
  )
)
updateOperatorDef('%%', 'testthat', 'alpha_name', 'mod')

assignOperatorDef(
  c('dbeta', 'dbinom', 'ddexp', 'dgamma', 'dinvgamma', 'dlnorm', 'dnbinom',
    'dnorm', 'dt', 'dt_nonstandard', 'dunif', 'dweibull'),
  list(
    labelAbstractTypes = list(
      handler = 'Distribution',
      returnTypeCode = returnTypeCodes$double
    ),
    eigenImpl = list(
      toEigen = 'Yes',
      handler = 'PromoteAllButLastArg'
    ),
    cppOutput = list(
      handler = 'RR_Distribution'
    )
  )
)

## assignOperatorDef(
##   c('list'),
##   list(
##     labelAbstractTypes = list(handler = 'List'),
##   eigenImpl = list(toEigen = 'No'),
##   cppOutput = list(handler = 'List')
## )

specificCallReplacements <- list(
#    '^' = 'pow',
#    '%%' = 'nimMod',
    length = 'size',
    is.nan = 'ISNAN',
    is.nan.vec = 'ISNAN',
    is.na = 'ISNA',
    is.na.vec = 'ISNA',
    lgamma = 'lgammafn',
    logfact = 'lfactorial',
    loggam = 'lgammafn',
    gamma = 'gammafn',
    expit = 'ilogit',
    phi = 'iprobit',
    ceiling = 'ceil',
    trunc = 'ftrunc',
    nDim = 'dim',
    checkInterrupt = 'R_CheckUserInterrupt')

for(op in names(specificCallReplacements))
    assignOperatorDef(
        op,
        list(
            simpleTransformations = list(
                handler = 'replace',
                replacement = specificCallReplacements[[op]]
            )
        )
    )

assignmentOperators <- c('<-','<<-','=')

ifOrWhile <- c('if','while')

callToSkipInEigenization <- c('copy',
                              'setValues',
                              'setValuesIndexRange',
                              'getValues',
                              'getValuesIndexRange',
                              'setSize',
                              'resize',
                              'getsize',
                              'size',
                              'resizeNoPtr',
                              'assert',
                              'return',
                              'blank',
                              'rankSample',
                              'nimArr_dmnorm_chol',
                              'nimArr_dmvt_chol',
                              'nimArr_dwish_chol',
                              'nimArr_dinvwish_chol',
                              'nimArr_dcar_normal',
                              'nimArr_dmulti',
                              'nimArr_dcat',
                              'nimArr_dinterval',
                              'nimArr_ddirch',
                              'nimArr_rmnorm_chol',
                              'nimArr_rmvt_chol',
                              'nimArr_rwish_chol',
                              'nimArr_rinvwish_chol',
                              'nimArr_rcar_normal',
                              'nimArr_rmulti',
                              'nimArr_rcat',
                              'nimArr_rinterval',
                              'nimArr_rdirch',
                              'calculate',
                              'calculateDiff',
                              'simulate',
                              'getLogProb',
                              'nimEquals',
                              'startNimbleTimer',
                              'endNimbleTimer')

## http://en.cppreference.com/w/cpp/language/operator_precedence

## Used to decide when to put parentheses around LHS or RHS based on operator precendence.
operatorRank <- c(
    list('<-' = 100, '^' = 4, '::' = 3),
    makeCallList(c('*','/','%*%', '%%'), 5),
    makeCallList(c('+', '-'), 6),
    makeCallList(c('>','<','<=', '>='), 7),
    makeCallList(c('==','!='), 8),
    list('!' = 10), ## follows R's precedence order, not C's
    list('&' = 13,
         '|' = 14,
         '&&' = 13,
         '||' = 14)
)
