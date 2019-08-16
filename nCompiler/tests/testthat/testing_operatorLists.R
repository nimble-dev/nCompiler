## To add an operator for math or AD testing:

## * Use updateOperatorDef() to add the field 'testing' to the operator's
##   def. All test-specific info will go in this field.
## * In 'testing', add 'math_argTypes' and/or 'AD_argTypes', which should be a
##   list where each entry is a character vector specifying the argTypes to
##   test. For example, for math testing a binary operator:
##     list(c('numericVector', 'logicalScalar'), c('logicalMatrix', 'numericMatrix'))
##   would create two tests. Note that AD argTypes need sizes. The following would
##   create three tests of a unary operator:
##     list('numericVector(7)', 'numericScalar', 'numericMatrix(size = c(2, 3))')
## * If the operator string is not filename safe, set 'alpha_name'. E.g., '*'
##   becomes 'mult' in the gold file filename.
## * Some special operators need additional info for return_type_string() to
##   generate the correct output type based on the inputs. Set these to TRUE
##   where appropriate. So far, they are:
##   - 'matrixMultOp' (e.g. %*%)
##   - 'recyclingRuleOp' (e.g. 'dnorm')
##   - 'reductionOp' (e.g. 'mean')
##
## TODO: add explanation of input_gen_funs (for now see the note for '/')

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
  c('sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'asinh', 'acosh', 'atanh',
    'logit', 'ilogit', 'expit', 'probit', 'iprobit', 'phi', 'cloglog',
    'icloglog', 'ceiling', 'floor', 'round', 'trunc', 'lgamma', 'loggam',
    'log1p', 'lfactorial', 'logfact', 'mean', 'prod', 'exp', 'log', 'rsqrt',
    'sqrt', 'abs', 'cube', 'square'),
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
  c('log', 'rsqrt', 'sqrt'), 'testing', 'input_gen_funs',
  list(
    arg1 = function(arg_size, type) {
      switch(
        type,
        "double"  = abs(rnorm(prod(arg_size))), ## no negatives
        "integer" = rgeom(prod(arg_size), 0.5),
        "logical" = sample(c(TRUE, FALSE), prod(arg_size), replace = TRUE)
      )
    }
  )
)

nCompiler:::updateOperatorDef(
  'squaredNorm',
  'testing',
  val = list(
    ## math_argTypes = list('numericScalar', 'integerScalar', 'logicalScalar',
    ##                      'numericVector', 'integerVector', 'logicalVector',
    ##                      'numericMatrix', 'integerMatrix', 'logicalMatrix',
    ##                      'numericArray(nDim=3)',  'integerArray(nDim=3)',
    ##                      'logicalArray(nDim=3)')
  )
)

nCompiler:::updateOperatorDef(
  c('mean', 'prod', 'squaredNorm'),
  'testing', 'reductionOp', TRUE
)

###################
## binary operators
###################

binaryOp_argTypes <- c(
  ## combine scalars and vectors in every way
  make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar',
                      'numericVector', 'integerVector', 'logicalVector'),
  ## matrices combine with matrices of same shape
  make_argType_tuples('numericMatrix', 'integerMatrix', 'logicalMatrix'),
  ## combine arrays with arrays of same shape
  make_argType_tuples('numericArray(nDim=3)', 'integerArray(nDim=3)',
                      'logicalArray(nDim=3)'),
  ## combine scalars and matrices / arrays
  make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar',
                      rhs = c('numericMatrix', 'integerMatrix',
                              'logicalMatrix', 'numericArray(nDim=3)',
                              'integerArray(nDim=3)',
                              'logicalArray(nDim=3)')),
  make_argType_tuples('numericMatrix', 'integerMatrix', 'logicalMatrix',
                      'numericArray(nDim=3)', 'integerArray(nDim=3)',
                      'logicalArray(nDim=3)',
                      rhs = c('numericScalar', 'integerScalar', 'logicalScalar'))
)

nCompiler:::updateOperatorDef(
  c('pmin', 'pmax', '==', '!=', '<=', '>=', '<', '>', '&', '|', '+', '/', '*'),
  'testing',
  val = list(
    math_argTypes = binaryOp_argTypes
  )
)

## Ensure that we don't divide by 0.
## Since arg1 = NULL, the default input generation function in argType_2_input
## will be used, but for arg2 it will use the custom input generation function
## we provide.
## See make_input() and argType_2_input() in testing_utils.R.
nCompiler:::updateOperatorDef(
  c('/'), 'testing', 'input_gen_funs',
  list(
    arg1 = NULL,
    arg2 = function(arg_size, type) {
      switch(
        type,
        "double"  = rnorm(prod(arg_size)),
        "integer" = rgeom(prod(arg_size), 0.5) + 1, ## no zeros
        "logical" = rep(TRUE, prod(arg_size)) ## always TRUE
      )
    }
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

#############################
## unary and binary operators
#############################

## the *_argTypes include both the unary and binary cases
nCompiler:::updateOperatorDef(
  c('-'),
  'testing',
  val = list(
    AD_argTypes = list('numericScalar', 'numericVector(7)',
                       c('numericScalar', 'numericScalar'),
                       c('numericScalar', 'numericVector(7)'),
                       c('numericVector(7)', 'numericVector(7)'),
                       c('numericVector(7)', 'numericScalar')),
    math_argTypes = c(binaryOp_argTypes, ## remaining argTypes for unary usage:
                      'numericScalar', 'integerScalar',
                      'logicalScalar', 'numericVector', 'integerVector',
                      'logicalVector', 'numericMatrix', 'integerMatrix',
                      'logicalMatrix', 'numericArray(nDim=3)',
                      'integerArray(nDim=3)', 'logicalArray(nDim=3)'),
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

#########################################
## %% (rhs scalar when lhs Eigen::Tensor)
#########################################

nCompiler:::updateOperatorDef(
  '%%',
  'testing',
  val = list(
    math_argTypes = c(
      make_argType_tuples('numericScalar', 'integerScalar', 'logicalScalar'),
      make_argType_tuples('numericVector', 'integerVector', 'logicalVector',
                          'numericMatrix', 'integerMatrix', 'logicalMatrix',
                          'numericArray(nDim=3)', 'integerArray(nDim=3)',
                          'logicalArray(nDim=3)', rhs = c('numericScalar',
                                                          'integerScalar',
                                                          'logicalScalar'))),
    alpha_name = 'mod',
    ## use same input_gen_funs as for '/' so we don't divide by 0
    input_gen_funs = nCompiler:::getOperatorDef('/', 'testing', 'input_gen_funs')
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
