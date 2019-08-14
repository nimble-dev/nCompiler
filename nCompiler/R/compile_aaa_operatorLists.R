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

updateOperatorDef <- function(ops, field, subfield = NULL, val) {
  for (op in ops) {
    if (is.null(subfield))
      operatorDefEnv[[op]][[field]] <- val
    else
      operatorDefEnv[[op]][[field]][[subfield]] <- val
  }
}

getOperatorDef <- function(op, field = NULL, subfield = NULL) {
  opInfo <- operatorDefEnv[[op]]
  if (is.null(opInfo) || is.null(field)) return(opInfo)
  if (is.null(opInfo[[field]]) || is.null(subfield)) return(opInfo[[field]])
  return(opInfo[[field]][[subfield]])
}

assignOperatorDef(
  'nFunction',
  list(
    labelAbstractTypes = list(
      handler = 'Generic_nFunction'),
    cppOutput = list(
      handler = 'Generic_nFunction')
  )
)

assignOperatorDef(
  'nClass_method',
  list(
    labelAbstractTypes = list(
      handler = 'Generic_nClass_method') ## will convert to nFunction
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
  c('parallel_for'),
  list(
    labelAbstractTypes = list(
      handler = 'ParallelFor'),
    finalTransformations = list(
      handler = "ParallelFor") ## Creates GeneralFor in the parallel_loop_body class
  )
)

assignOperatorDef(
  c('GeneralFor'),
  list(
    cppOutput = list(
      handler = 'GeneralFor')
  )
)

assignOperatorDef(
  c('rep'),
  list(
    labelAbstractTypes = list(
      handler = 'Rep'),
    eigenImpl = list(
      handler = 'Rep')
  )
)

assignOperatorDef(
  c(':'),
  list(
    labelAbstractTypes = list(
        handler = 'Colon')
  )
)

assignOperatorDef(
  c('('),
  list(
    labelAbstractTypes = list(
      handler = 'UnaryCwise'
    ),
    cppOutput = list(
      handler = 'Paren')
  )
)

assignOperatorDef(
  c('['),
  list(
    labelAbstractTypes = list(
      handler = 'Bracket'),
    eigenImpl = list(
      handler = 'Bracket'), ## converts `[` to `index[`
    cppOutput = list(
      handler = 'IndexingBracket') ## needed for generated code such as for AD.
  )
)

assignOperatorDef(
  c('index['),
  list(
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
        cppOutput = list(
            handler = 'Return')
    )
)

assignOperatorDef(
  '::',
  list(cppOutput = list(handler = 'MidOperator'))
)

assignOperatorDef(
  '$',
  list(
    labelAbstractTypes = list(
      handler = 'DollarSign')
  )
)

assignOperatorDef(
  'chainedCall',
  list(
    labelAbstractTypes = list(
      handler = 'ChainedCall')
    ## , cppOutput = list(
    ##   handler = 'ChainedCall')
  )
)

assignOperatorDef(
    c('+','-'),
    list(
        labelAbstractTypes = list(
          handler = 'BinaryUnaryCwise',
          returnTypeCode = returnTypeCodes$promoteNoLogical),
        eigenImpl = list(
          handler = 'cWiseAddSub'),
        cppOutput = list(
          handler = 'BinaryOrUnary')
    )
)

assignOperatorDef(
  c('min', 'max'),
  list(
    simpleTransformations = list(
      handler = 'minMax'),
    labelAbstractTypes = list(
        handler = 'UnaryReduction',
        returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      handler = 'Reduction',
      method = TRUE),
    cppOutput = list()
  )
)
updateOperatorDef('max', 'cppOutput', 'cppString', 'maximum')
updateOperatorDef('min', 'cppOutput', 'cppString', 'minimum')

assignOperatorDef(
  c('pmin', 'pmax'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      handler = 'cWiseBinary',
      method = TRUE),
    cppOutput = list()
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
  '->method',
  list(
    help = '->method(A, foo, b) is for A->foo(b)',
    cppOutput = list(
      handler = 'PtrMethod'
    )
  )
)

assignOperatorDef(
  '->member',
  list(
    help = '->member(A, b) is for A->b',
    cppOutput = list(
      handler = 'PtrMember'
    )
  )
)

assignOperatorDef(
  '.method',
  list(
    help = '.method(A, foo, b) is for A.foo(b)',
    cppOutput = list(
      handler = 'Method'
    )
  )
)

assignOperatorDef(
  '.member',
  list(
    help = '.member(A, b) is for A.b',
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
      handler = 'Reduction',
      method = TRUE
    ),
    cppOutput = list(
      handler = 'TensorReduction'
    )
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
      handler = 'Reduction',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    )
  )
)

assignOperatorDef(
  c('exp', 'lgamma', 'log', 'rsqrt', 'sqrt', 'tanh'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      handler = 'cWiseUnary',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    )
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
      handler = 'cWiseUnary',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    )
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
      handler = 'cWiseUnary_external',
      method = TRUE
    ),
    cppOutput = list(
      handler = ''
    )
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
      handler = 'cWiseBinaryLogical'),
    cppOutput = list(
      handler = 'MidOperator')
  )
)
updateOperatorDef('<=', 'eigenImpl', 'swapOp', '>=')
updateOperatorDef('>=', 'eigenImpl', 'swapOp', '<=')
updateOperatorDef('<', 'eigenImpl', 'swapOp', '>')
updateOperatorDef('>', 'eigenImpl', 'swapOp', '<')
updateOperatorDef('&', 'cppOutput', 'cppString', ' && ')
updateOperatorDef('|', 'cppOutput', 'cppString', ' || ')

assignOperatorDef(
  c('/'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      handler = 'cWiseMultDiv'),
    cppOutput = list(
      handler = 'MidOperator')
  )
)

assignOperatorDef(
  c('*'),
  list(
    returnTypeHandling = list(
      code = returnTypeCodes$promoteNoLogical),
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      handler = 'cWiseMultDiv'),
    cppOutput = list(
      handler = 'MidOperator')
  )
)

assignOperatorDef(
  c('^'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      handler = 'cWiseByScalar', ## Eigen::Tensor requires the rhs of pow to be scalar
      method = TRUE),
    cppOutput = list(
      cppString = 'pow',
      handler = '')
  )
)

assignOperatorDef(
  c('%%'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      handler = 'cWiseByScalar'), ## Eigen::Tensor requires the rhs of % to be scalar
    cppOutput = list(
      handler = 'MidOperator',
      cppString = ' % ')
  )
)

assignOperatorDef(
  c('%*%'),
  list()
)

assignOperatorDef(
  c('dbeta', 'dbinom', 'ddexp', 'dgamma', 'dinvgamma', 'dlnorm', 'dnbinom',
    'dnorm', 'dt', 'dt_nonstandard', 'dunif', 'dweibull'),
  list(
    labelAbstractTypes = list(
      handler = 'Distribution',
      returnTypeCode = returnTypeCodes$double
    ),
    eigenImpl = list(
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
