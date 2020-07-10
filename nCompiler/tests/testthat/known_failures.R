## Include one-off failures in 'compilation' or 'runtime' with full test name
## (e.g. '/ arg1 = numericArray(nDim=3) arg2 = logicalArray(nDim=3)'.  If many
## tests are failing and adding them all to math_knownFailures becomes
## overwhelming, consider removing the op's entries in math_argTypes in
## testing_operatorLists.R.


##################
## unary operators
##################

nCompiler:::updateOperatorDef(
  c('min', 'max', 'all', 'any', 'exp', 'length', 'log', 'rsqrt', 'sqrt', 'tanh',
    'cube', 'square'),
  'testing', 'known_failures',
  list(
    math = list(
      compilation = list('numericScalar', 'integerScalar', 'logicalScalar')
    ),
    AD = list(
      compilation = list('numericScalar')
    )
  )
)

nCompiler:::updateOperatorDef(
  c('abs'),
  'testing', 'known_failures',
  list(
    math = list(
      compilation = list('numericScalar', 'integerScalar', 'logicalScalar')
    ),
    AD = list(
      compilation = list('numericScalar'),
      runtime = list('numericVector(7)')
    )
  )
)

nCompiler:::updateOperatorDef(
  c('sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'asinh', 'acosh', 'atanh',
    'logit', 'ilogit', 'expit', 'probit', 'iprobit', 'phi', 'cloglog',
    'icloglog', 'ceiling', 'floor', 'round', 'trunc', 'lgamma', 'loggam',
    'log1p', 'lfactorial', 'logfact', 'mean', 'prod', 'sum'),
  'testing', 'known_failures',
  list(
    math = list(
      compilation = list('numericScalar', 'integerScalar', 'logicalScalar')
    ),
    AD = list(
      compilation = list('numericScalar', 'numericVector(7)')
    )
  )
)

###################
## binary operators
###################

nCompiler:::updateOperatorDef(
  c('-'),
  'testing', 'known_failures',
  list(
    AD = list(
      compilation = list(
        ## This case fails to compile in an nClass when AD is on, but does
        ## compile as an nFunction.
        c('numericVector(7)', 'numericScalar')
      )
    )
  )
)

nCompiler:::updateOperatorDef(
  c('+'),
  'testing', 'known_failures',
  list(
    AD = list(
      runtime = list(
        c('numericVector(7)', 'numericScalar')
      )
    )
  )
)

nCompiler:::updateOperatorDef(
  c('/'),
  'testing', 'known_failures',
  list(
    AD = list(
      runtime = list(
        c('numericVector(7)', 'numericVector(7)')
      )
    )
  )
)

nCompiler:::updateOperatorDef(
  c('pmin', 'pmax'),
  'testing', 'known_failures',
  list(
    math = list(
      compilation = make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar'),
      runtime = c(
        make_argType_tuples('numericMatrix', 'integerMatrix', 'logicalMatrix',
                            'numericArray(nDim=3)', 'integerArray(nDim=3)',
                            'logicalArray(nDim=3)', rhs = 'numericVector'),
        ## in R, when first arg is scalar the result is a vector, so swapping
        ## args won't work for scalar and matrix/array combos
        make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar',
                            rhs = c('numericMatrix', 'integerMatrix', 'logicalMatrix',
                            'numericArray(nDim=3)', 'integerArray(nDim=3)',
                            'logicalArray(nDim=3)'))
      )
    )
  )
)

nCompiler:::updateOperatorDef(
  c('&', '|'),
  'testing', 'known_failures',
  list(
    math = list(
      compilation = c(
        ## doesn't work when one arg is Eigen::Tensor and the other is scalar
        make_argType_tuples('numericVector', 'integerVector', 'logicalVector',
                            'numericMatrix', 'integerMatrix', 'logicalMatrix',
                            'numericArray(nDim=3)', 'integerArray(nDim=3)',
                            'logicalArray(nDim=3)',
                            rhs = c('numericScalar', 'integerScalar', 'logicalScalar')),
        make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar',
                            rhs = c('numericVector', 'integerVector', 'logicalVector',
                                    'numericMatrix', 'integerMatrix', 'logicalMatrix',
                                    'numericArray(nDim=3)', 'integerArray(nDim=3)',
                                    'logicalArray(nDim=3)'))
      )
    )
  )
)

nCompiler:::updateOperatorDef(
  c('%%'),
  'testing', 'known_failures',
  list(
    math = list(
      ## %% currently only works for arguments that promote to integers
      compilation = c(
        ## we only test with rhs scalar... see math_operatorLists.R
        make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar',
                            'numericVector', 'integerVector', 'logicalVector',
                            'numericMatrix', 'integerMatrix', 'logicalMatrix',
                            'numericArray(nDim=3)', 'integerArray(nDim=3)',
                            'logicalArray(nDim=3)',
                            rhs = c('numericScalar')),
        make_argType_tuples('numericScalar', 'numericVector', 'numericMatrix',
                            'numericArray(nDim=3)',
                            rhs = c('logicalScalar', 'integerScalar'))
      )
    )
  )
)

nCompiler:::updateOperatorDef(
  c('^'),
  'testing', 'known_failures',
  list(
    math = list(
      compilation = c(
        ## current doesn't work for scalar bases
        make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar')
      )
    )
  )
)
