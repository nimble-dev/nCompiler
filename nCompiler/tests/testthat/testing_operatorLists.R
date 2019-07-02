##################
## unary operators
##################

## math only
nCompiler:::updateOperatorDef(
  c('min', 'max', 'all', 'any'),
  'testing',
  val = list(
    math_argTypes = list('numericScalar', 'integerScalar', 'logicalScalar',
                         'numericVector', 'integerVector', 'logicalVector',
                         'numericMatrix', 'integerMatrix', 'logicalMatrix',
                         'numericArray(nDim=3)',  'integerArray(nDim=3)',
                         'logicalArray(nDim=3)'),
    reductionOp = TRUE ## this info is needed for return_type_string()
  )
)

nCompiler:::updateOperatorDef(
  c('mean', 'prod', 'squaredNorm', 'exp', 'inverse', 'log', 'rsqrt', 'sqrt',
    'tanh', 'abs', 'cube', 'square', 'atan', 'logit'),
  'testing',
  val = list(
    ## derivatives currently only available for scalar and vector inputs  
    AD_argTypes = list('numericScalar', 'numericVector(7)'),
    math_argTypes = list('numericScalar', 'integerScalar', 'logicalScalar',
                         'numericVector', 'integerVector', 'logicalVector',
                         'numericMatrix', 'integerMatrix', 'logicalMatrix',
                         'numericArray(nDim=3)',  'integerArray(nDim=3)',
                         'logicalArray(nDim=3)')
  )
)

nCompiler:::updateOperatorDef(
  c('mean', 'prod', 'squaredNorm'),
  'testing', 'reductionOp', TRUE
)

###################
## binary operators
###################

nCompiler:::updateOperatorDef(
  c('pmin', 'pmax', '==', '!=', '<=', '>=', '<', '>', '&', '|', '+', '/', '*', '%%'),
  'testing',
  val = list(
    math_argTypes = make_argType_tuples('numericScalar', 'integerScalar',
                                        'logicalScalar', 'numericVector',
                                        'integerVector', 'logicalVector',
                                        'numericMatrix', 'integerMatrix',
                                        'logicalMatrix', 'numericArray(nDim=3)',
                                        'integerArray(nDim=3)',  'logicalArray(nDim=3)')
  )
)

## add AD_argTypes for a few operators
nCompiler:::updateOperatorDef(
  c('+', '/', '*'),
  'testing', 'AD_argTypes',
  list(c('numericScalar', 'numericScalar'),
       c('numericScalar', 'numericVector(7)'),
       c('numericVector(7)', 'numericVector(7)'),
       c('numericVector(7)', 'numericScalar'))
)

## add descriptive and safe names for gold-file names
nCompiler:::updateOperatorDef('+', 'testing', 'alpha_name', 'plus')
nCompiler:::updateOperatorDef('==', 'testing', 'alpha_name', 'eq')
nCompiler:::updateOperatorDef('!=', 'testing', 'alpha_name', 'neq')
nCompiler:::updateOperatorDef('<=', 'testing', 'alpha_name', 'le')
nCompiler:::updateOperatorDef('>=', 'testing', 'alpha_name', 'ge')
nCompiler:::updateOperatorDef('<', 'testing', 'alpha_name', 'lt')
nCompiler:::updateOperatorDef('>', 'testing', 'alpha_name', 'gt')
nCompiler:::updateOperatorDef('&', 'testing', 'alpha_name', 'and')
nCompiler:::updateOperatorDef('|', 'testing', 'alpha_name', 'or')
nCompiler:::updateOperatorDef('/', 'testing', 'alpha_name', 'div')
nCompiler:::updateOperatorDef('*', 'testing', 'alpha_name', 'mult')
nCompiler:::updateOperatorDef('%%', 'testing', 'alpha_name', 'mod')

#############################
## unary and binary operators
#############################

## the *_argTypes include both the unary and binary cases
nCompiler:::updateOperatorDef(
  c('-'),
  'testing',
  val = list(
    AD_argTypes = list('numericScalar()', 'numericVector(7)',
                       c('numericScalar', 'numericScalar'),
                       c('numericScalar', 'numericVector(7)'),
                       c('numericVector(7)', 'numericVector(7)'),
                       c('numericVector(7)', 'numericScalar')),
    math_argTypes = c(make_argType_tuples('numericScalar', 'integerScalar',
                                          'logicalScalar', 'numericVector',
                                          'integerVector', 'logicalVector',
                                          'numericMatrix', 'integerMatrix',
                                          'logicalMatrix', 'numericArray(nDim=3)',
                                          'integerArray(nDim=3)',  'logicalArray(nDim=3)'),
                      'numericScalar', 'integerScalar', 'logicalScalar',
                      'numericVector', 'integerVector', 'logicalVector',
                      'numericMatrix', 'integerMatrix', 'logicalMatrix',
                      'numericArray(nDim=3)',  'integerArray(nDim=3)',
                      'logicalArray(nDim=3)'),
    alpha_name = 'minus'
  )
)
###################
## pow (rhs scalar)
###################

nCompiler:::updateOperatorDef(
  '^',
  'testing',
  val = list(
    ## AD_argTypes = list(c('numericScalar', 'numericScalar'),
    ##                    c('numericVector(7)', 'numericScalar')),
    math_argTypes = make_argType_tuples('numericScalar', 'integerScalar',
                                        'logicalScalar', 'numericVector',
                                        'integerVector', 'logicalVector',
                                        'numericMatrix', 'integerMatrix',
                                        'logicalMatrix',
                                        'numericArray(nDim=3)',
                                        'integerArray(nDim=3)',
                                        'logicalArray(nDim=3)',
                                        rhs = c('numericScalar',
                                                'integerScalar',
                                                'logicalScalar')),
    alpha_name = 'pow'
  )
)

##############
## matrix mult
##############

nCompiler:::updateOperatorDef(
  '%*%',
  'testing',
  val = list(matrixMultOp = TRUE, alpha_name = 'matmul')
)

#########################
## distribution functions
#########################

nCompiler:::updateOperatorDef(
  c('dbeta', 'dbinom', 'ddexp', 'dgamma', 'dinvgamma', 'dlnorm', 'dnbinom',
    'dnorm', 'dt', 'dt_nonstandard', 'dunif', 'dweibull'),
  'testing',
  val = list(recyclingRuleOp = TRUE)
)












