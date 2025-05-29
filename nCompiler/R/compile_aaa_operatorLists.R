## Each operator will have a list of information for each compiler stage.
## The information for each stage will be a list.

## Check in which handlers below returnTypeCode is actually used

returnTypeCodes <- list(
  AD = 1L,
  double = 2L,
  integer = 3L,
  logical = 4L,
  promote = 5L,
  promoteToDoubleOrAD = 6L,
  promoteNoLogical = 7L)

returnTypeString2Code <- function(returnTypeString) {
  if(is.character(returnTypeString))
    do.call('switch', c(list("double"), nCompiler:::returnTypeCodes))
  else
    returnTypeString
}

returnTypeCode2String <- function(returnTypeCode) {
  if(is.numeric(returnTypeCode)) names(returnTypeCodes)[returnTypeCode]
  else returnTypeCode
}

operatorDefEnv <- new.env(parent=emptyenv())
operatorDefUserEnv <- new.env(parent=operatorDefEnv)

getHandlerEnv <- function(stage) {
  switch(stage,
         simpleTransformations = simpleTransformationsEnv,
         processAD = processADenv,
         labelAbstractTypes = labelAbstractTypesEnv,
         normalizeCalls = normalizeCallsEnv,
         generateCpp = genCppEnv,
         finalTransformations = finalTransformationsEnv,
         eigenImpl = eigenizeEnv,
         NULL
         )
}

registerOpDef <- function(opDefs, modify=TRUE, replaceEnv=TRUE) {
  opNames <- names(opDefs)
  if(is.null(opNames))
    stop("opDef must be a named list, with names that are the operator names.")
  for(i in seq_along(opDefs)) {
    opName <- opNames[i]
    currentOpDef <- opDefs[[opName]]
    if(isTRUE(replaceEnv)) {
      for(field in names(currentOpDef)) {
        if(is.list(currentOpDef[[field]])) {
          handler <- currentOpDef[[field]][["handler"]]
          if(is.function(handler)) {
            handlerEnv <- getHandlerEnv(field)
            if(!is.null(handlerEnv)) {
              environment(handler) <- handlerEnv
              currentOpDef[[field]][["handler"]] <- handler
            }
          }
        }
      }
    }
    if(isTRUE(modify)) {
      existingOpDef <- getOperatorDef(opName)
      if(!is.null(existingOpDef)) {
        for(field in names(currentOpDef)) {
          existingOpDef[[field]] <- currentOpDef[[field]]
        }
        currentOpDef <- existingOpDef
      }
    }
    operatorDefUserEnv[[opName]] <- currentOpDef
  }
}

deregisterOpDef <- function(names) {
  if(!isTRUE(is.character(names))) names <- ls(names) # in case an env or list is passed in as names.
  suppressWarnings(rm(list=names, envir=operatorDefUserEnv))
}

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
  opInfo <- get0(op, envir=operatorDefUserEnv)
#  opInfo <- operatorDefEnv[[op]]
  if (is.null(opInfo) || is.null(field)) return(opInfo)
  if (is.null(opInfo[[field]]) || is.null(subfield)) return(opInfo[[field]])
  return(opInfo[[field]][[subfield]])
}

assignOperatorDef(
  'nList',
  list(
    matchDef = function(type, length) {},
    compileArgs = c("type"),
    labelAbstractTypes = list(
      handler = 'nList'),
    cppOutput = list(
      handler = 'nList')
  )
)

assignOperatorDef(
  'nFunctionRef',
  list(
    cppOutput = list(handler = 'Generic_nClass_method_ref')
  )
)

assignOperatorDef(
  'NFCALL_', # This is used for non-method nFunctions in normalizeCalls and for any (including method) nFunctions after normalizeCalls
  list(
    labelAbstractTypes = list(
      handler = 'nFunction_or_method_call'),
   normalizeCalls = list(
      handler = 'nFunction_or_method_call'),
    cppOutput = list(
      handler = 'Generic_nFunction')
  )
)

assignOperatorDef(
  'NCMETHOD_', # This is a transient label that only exists within normalizeCalls
  list(
    ## labelAbstractTypes = list(
    ##   handler = 'nFunction_or_method_call'),
    normalizeCalls = list(
      handler = 'nFunction_or_method_call')#, # becomes NFCALL_
    ## cppOutput = list(
    ##   handler = 'Generic_nFunction')
  )
)

assignOperatorDef(
  c('dim'),
  list(
    labelAbstractTypes = list(
      handler = 'dim'
    ),
    cppOutput = list(
      handler = 'dim'
    )
  )
)

assignOperatorDef( # c()
  c('nC'),
  list(
    labelAbstractTypes = list(
      handler = 'nC',
      return_nDim = 1,
      returnTypeCodes$promote),
    eigenImpl = list()
  )
)

assignOperatorDef(
  c('construct_new_nClass'),
  list(
    cppOutput = list(
      handler = 'nClass_constructor')
  )
)

assignOperatorDef(
  c('nAD', 'nNumeric', 'nInteger', 'nLogical', 'nMatrix', 'nArray'),
  list(
    labelAbstractTypes = list(
      handler = 'InitData'),
    eigenImpl = list(
      handler = 'TensorCreation')
  )
)
updateOperatorDef(
  c('nAD','nNumeric', 'nInteger', 'nLogical'),
  'labelAbstractTypes', 'return_nDim', 1
)
updateOperatorDef(
  'nAD',
  'labelAbstractTypes', 'returnTypeCode', returnTypeCodes$AD
)
updateOperatorDef(
  'nNumeric',
  'labelAbstractTypes', 'returnTypeCode', returnTypeCodes$double
)
updateOperatorDef(
  'nInteger',
  'labelAbstractTypes', 'returnTypeCode', returnTypeCodes$integer
)
updateOperatorDef(
  'nLogical',
  'labelAbstractTypes', 'returnTypeCode', returnTypeCodes$logical
)
updateOperatorDef(
  c('nMatrix', 'nArray'),
  'labelAbstractTypes', 'returnTypeCode', returnTypeCodes$promote
)

assignOperatorDef(
  'type_is',
  list(
    matchDef = function(type, value) {},
    compileArgs = c("type"),
    help = 'type_is("integerVector", foo(x)) declares that foo(x) return an integer vector (or any valid type declaration including e.g. quote(integerVector())).',
    labelAbstractTypes = list(
      handler = 'type_is')
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
  c('which'),
  list(
    labelAbstractTypes = list(
      handler = 'Which'),
    eigenImpl = list(
      handler = 'Which')
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
  c('parallel_reduce'),
  list(
    labelAbstractTypes = list(
      handler = 'ParallelReduce'),
    finalTransformations = list(
      handler = 'ParallelReduce')
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
  c('nRep'),
  list(
    matchDef = function(x, times, length.out, each){},
    labelAbstractTypes = list(
      handler = 'VectorReturnType',
      returnTypeCode = returnTypeCodes$promote),
    eigenImpl = list(
      handler = 'Rep')
  )
)

assignOperatorDef(
  c(':'),
  list(
    labelAbstractTypes = list(
      handler = 'Colon'),
    eigenImpl = list(
      handler = 'Colon')
  )
)

assignOperatorDef(
  c('nSeq'),
  list(
    matchDef = function(from, to, by, length.out) {},
    labelAbstractTypes = list(
      handler = 'Seq'),
    eigenImpl = list(
      handler = 'Seq'#,
      #      i1 = literalIntegerExpr(1), # see compile_zzz_operatorLists.R, where there are added after the relevant definitions
      #      d1 = literalDoubleExpr(1)
    )
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
  c('[['),
  list(
    labelAbstractTypes = list(
      handler = 'DoubleBracket'),
    cppOutput = list(
      handler = 'IndexingBracket') # generates to single bracket for C++
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

nCompiler:::assignOperatorDef(
  c('IndexByScalar'),
  list(
    cppOutput = list(handler = 'IndexByScalar')
  )
)

nCompiler:::assignOperatorDef(
  c('IndexByVec'),
  list(
    cppOutput = list(handler = 'IndexByVec')
  )
)

nCompiler:::assignOperatorDef(
  c('IndexBySeqs'),
  list(
    cppOutput = list(handler = 'IndexBySeqs')
  )
)

assignmentOperators <- c('<-','<<-','=')

assignOperatorDef(
  assignmentOperators,
  list(
    labelAbstractTypes = list(
      handler = 'Assign'),
    eigenImpl = list(
      beforeHandler = 'Assign_Before',
      handler = 'Assign'),
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
          handler = 'cWiseAddSub',
          replacements = list(
            '+' = list(
              'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::plus>',
              'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::plus>'
            ),
            '-' = list(
              'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::minus>',
              'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::minus>'
            )
          )
        ),
        cppOutput = list(
          handler = 'BinaryOrUnary')
    )
)

assignOperatorDef(
  c('inprod'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryReduction'
    ),
    eigenImpl = list(
      handler = 'BinaryReduction'
    )
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
      removeForScalar = TRUE,
      method = TRUE),
    cppOutput = list()
  )
)
updateOperatorDef('max', 'cppOutput', 'cppString', 'maximum')
updateOperatorDef('min', 'cppOutput', 'cppString', 'minimum')

assignOperatorDef(
  c('pairmin', 'pairmax'),
  list(
    labelAbstractTypes = list(
        handler = 'BinaryCwise',
        returnTypeCode = returnTypeCodes$promoteNoLogical),
    cppOutput = list()
  )
)
updateOperatorDef('pairmax', 'cppOutput', 'cppString', 'std::max')
updateOperatorDef('pairmin', 'cppOutput', 'cppString', 'std::min')

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
  'nEval_',
  list(
    help = 'nEval_(A, type) is for forcing evaluation of A before assignment in x <- A, needed to fix aliasing if A is an expression containing parts of x',
    cppOutput = list(
      handler = 'nEval_'
    )
  )
)


assignOperatorDef(
  'scalarcast',
  list(
    help = 'scalarcast(A, type) is for scalar_cast_<type>::cast(A).',
    cppOutput = list(
      handler = 'ScalarCast'
    )
  )
)

assignOperatorDef( ## Used only for diag(x); could it use eigencast instead?
  'StaticCast',
  list(
    help = 'StaticCast(A) is for static_cast<code$type>(A).',
    cppOutput = list(
      handler = 'StaticCast'
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
  c('nCpp','cppLiteral'),
  list(
    matchDef = function(text, types) {},
    compileArgs = c("text","types"),
    help = 'cppLiteral("x = y;") inserts x = y; directly into the C++ output.',
    simpleTransformations = list(handler = "Literal"),
    labelAbstractTypes = list(handler = 'Literal'),
    cppOutput = list(handler = 'Literal')
  )
)

assignOperatorDef(
  c('mean', 'prod', 'squaredNorm', 'sum'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryReduction',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      handler = 'Reduction',
      removeForScalar = TRUE,
      method = TRUE
    ),
    cppOutput = list(
      handler = 'TensorReduction'
    )
  )
)

updateOperatorDef('squaredNorm', 'eigenImpl', 'removeForScalar', FALSE)

assignOperatorDef(
  c('nVar', 'nSd'),
  list(
    help = 'Example help entry',
    labelAbstractTypes = list(
      handler = 'UnaryReduction',
      returnTypeCode = returnTypeCodes$double
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
      noPromotion = TRUE,
      castLogical = TRUE,
      removeForScalar = TRUE,
      method = TRUE
    ),
    cppOutput = list(
    )
  )
)

assignOperatorDef(
  c('exp', 'lgamma', 'log', 'sqrt', 'tanh'), # We had an 'rsqrt'.  What was the use case for it?
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
      handler = 'AsIs'
    )
  )
)

updateOperatorDef('cube', 'eigenImpl', 'scalarCall', 'nc_cube')
updateOperatorDef('square', 'eigenImpl', 'scalarCall', 'nc_square')

assignOperatorDef(
  c('sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'asinh', 'acosh', 'atanh',
    'logit', 'ilogit', 'expit', 'probit', 'iprobit', 'phi', 'cloglog',
    'icloglog', 'ceiling', 'floor', 'round', 'trunc', 'gamma', 'lgamma',
    'loggam', 'log1p', 'factorial', 'lfactorial', 'logfact', 'cosh', 'sinh',
    'nStep'),
  list(
    help = 'cWiseUnary operators implemented via Tensor.unaryExpr',
    labelAbstractTypes = list(
      handler = 'UnaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      handler = 'cWiseUnary_external',
      method = TRUE
    ),
    cppOutput = list(
      handler = 'AsIs' # relevant only for scalar cases
    )
  )
)
updateOperatorDef('expit', 'eigenImpl', 'nameReplacement', 'ilogit')
updateOperatorDef('phi', 'eigenImpl', 'nameReplacement', 'iprobit')
updateOperatorDef('ceiling', 'eigenImpl', 'nameReplacement', 'ceil')
updateOperatorDef('round', 'eigenImpl', 'nameReplacement', 'nRound')
updateOperatorDef('trunc', 'eigenImpl', 'nameReplacement', 'nTrunc')
updateOperatorDef('gamma', 'eigenImpl', 'nameReplacement', 'gammafn')
updateOperatorDef('lgamma', 'eigenImpl', 'nameReplacement', 'lgammafn')
updateOperatorDef('loggam', 'eigenImpl', 'nameReplacement', 'lgammafn')
updateOperatorDef('logfact', 'eigenImpl', 'nameReplacement', 'lfactorial')

## binaryMidLogicalOperatorsComparison
assignOperatorDef(
  c('==','!=','<=','>=','<','>'),
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

assignOperatorDef(
  c('!'),
  list(
    labelAbstractTypes = list(
      handler = 'UnaryCwise',
      returnTypeCode = returnTypeCodes$logical
    ),
    eigenImpl = list(
      handler = 'cWiseUnary_external',
      method = TRUE
    ),
    cppOutput = list(
      handler = 'AsIs' # relevant only for scalar cases
    )
  )
)

assignOperatorDef(
  c('&','|'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwiseLogical',
      returnTypeCode = returnTypeCodes$logical),
    eigenImpl = list(
      handler = 'cWiseBinaryLogicalAndOr'),
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
updateOperatorDef('<=', 'eigenImpl', 'replacements', list(
  '<=' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::leq>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::leq>'
  )
))
updateOperatorDef('>=', 'eigenImpl', 'replacements', list(
  '>=' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::geq>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::geq>'
  )
))
updateOperatorDef('<', 'eigenImpl', 'replacements', list(
  '<' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::lt>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::lt>'
  )
))
updateOperatorDef('>', 'eigenImpl', 'replacements', list(
  '>' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::gt>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::gt>'
  )
))
updateOperatorDef('&', 'eigenImpl', 'replacements', list(
  '&' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::logical_and>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::logical_and>'
  )
))
updateOperatorDef('|', 'eigenImpl', 'replacements', list(
  '|' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::logical_or>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::logical_or>'
  )
))
updateOperatorDef('==', 'eigenImpl', 'replacements', list(
  '==' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::logical_eq>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::logical_eq>'
  )
))
updateOperatorDef('!=', 'eigenImpl', 'replacements', list(
  '!=' = list(
    'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::logical_neq>',
    'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::logical_neq>'
  )
))


assignOperatorDef(
  # This is for C++ Lambda expressions,
  # which may be generated as part of implementation
  # of some Eigen component-wise operations.
  'LambdaFun_',
  list(
    cppOutput = list(handler = 'LambdaFun_')
  ))

assignOperatorDef(
  c('/'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      handler = 'cWiseMultDiv',
      replacements = list(
        '/' = list(
          'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::divide>',
          'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::divide>'
        )
      )
    ),
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
      handler = 'cWiseMultDiv',
      replacements = list(
        '*' = list(
          'LHS' = 'nCompiler::binaryOpReshapeLHS<nCompiler::product>',
          'RHS' = 'nCompiler::binaryOpReshapeRHS<nCompiler::product>'
        )
      )
    ),
    cppOutput = list(
      handler = 'MidOperator')
  )
)

assignOperatorDef(
  c('^', 'pow'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$double),
    eigenImpl = list(
      handler = 'cWiseByScalar', ## Eigen::Tensor requires the rhs of pow to be scalar
      method = TRUE),
    cppOutput = list(
      cppString = 'pow'
    )
  )
)

assignOperatorDef(
  c('%%'),
  list(
    labelAbstractTypes = list(
      handler = 'BinaryCwise',
      returnTypeCode = returnTypeCodes$promoteNoLogical),
    eigenImpl = list(
      ## Eigen::Tensor requires the rhs of % to be scalar
      handler = 'cWiseByScalar', 
      ## Backwards compatibility with nimble functionality
      allScalar = TRUE
    ), 
    cppOutput = list(
      handler = 'AsIs',
      cppString = 'nc_mod')
  )
)

assignOperatorDef(
  c('nMul'),
  list(
    labelAbstractTypes = list(
      handler = 'nMul'
    )
  )
)

assignOperatorDef(
  # Note: besselK is not a distribution, but its usage has nearly identical 
  # functional needs as density functions
  c('dbeta', 'dbinom', 'dexp', 'dgamma', 'dinvgamma', 'dlnorm', 'dnbinom',
    'dnorm', 'dt', 'dt_nonstandard', 'dunif', 'dweibull', 'besselK'),
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

updateOperatorDef(
  'dbeta', 'matchDef', val = function(x, shape1, shape2, log = FALSE) {}
)
updateOperatorDef(
  'dbinom', 'matchDef', val = function(x, size, prob, log = FALSE) {}
)
updateOperatorDef(
  'dexp', 'matchDef', val = function(x, rate = 1, log = FALSE) {}
)
updateOperatorDef(
  'dgamma', 'matchDef', val = function(x, shape, scale = 1, log = FALSE) {}
)
updateOperatorDef(
  'dinvgamma', 'matchDef', val = function(x, shape, scale = 1, log = FALSE) {}
)
updateOperatorDef(
  'dlnorm', 'matchDef', val = function(x, meanlog = 0, sdlog = 1, log = FALSE) {}
)
updateOperatorDef(
  'dnbinom', 'matchDef', val = function(x, size, prob, log = FALSE) {}
)
updateOperatorDef(
  'dnorm', 'matchDef', val = function(x, mean = 0, sd = 1, log = FALSE) {}
)
updateOperatorDef(
  'dt', 'matchDef', val = function(x, df, log = FALSE) {}
)
updateOperatorDef(
  'dt_nonstandard', 'matchDef', 
  val = function(x, df = 1, mu = 0, sigma = 1, log = FALSE) {}
)
updateOperatorDef(
  'dunif', 'matchDef', val = function(x, min = 0, max = 1, log = FALSE) {}
)
updateOperatorDef(
  'dweibull', 'matchDef', val = function(x, shape, scale = 1, log = FALSE) {}
)
updateOperatorDef(
  'besselK', 'matchDef', val = function(x, nu, expon.scaled = FALSE) {}
)

assignOperatorDef(
  c('rbeta', 'rbinom', 'rexp', 'rgamma', 'rinvgamma', 'rlnorm', 'rnbinom',
    'rnorm', 'rt', 'rt_nonstandard', 'runif', 'rweibull'),
  list(
    labelAbstractTypes = list(
      handler = 'RandomGeneration',
      returnTypeCode = returnTypeCodes$double
    ),
    cppOutput = list(
      handler = 'RR_Distribution'
    )
  )
)

updateOperatorDef('rbeta', 'matchDef', val = function(n, shape1, shape2) {})
updateOperatorDef('rbinom', 'matchDef', val = function(n, size, prob) {})
updateOperatorDef('rexp', 'matchDef', val = function(n, rate = 1) {})
updateOperatorDef('rgamma', 'matchDef', val = function(n, shape, scale = 1) {})
updateOperatorDef('rinvgamma', 'matchDef', val = function(n, shape, scale = 1) {})
updateOperatorDef('rlnorm', 'matchDef', val = function(n, meanlog = 0, sdlog = 1) {})
updateOperatorDef('rnbinom', 'matchDef', val = function(n, size, prob) {})
updateOperatorDef('rnorm', 'matchDef', val = function(n, mean = 0, sd = 1) {})
updateOperatorDef('rt', 'matchDef', val = function(n, df) {})
updateOperatorDef('rt_nonstandard', 'matchDef', val = function(n, df = 1, mu = 0, sigma = 1) {})
updateOperatorDef('runif', 'matchDef', val = function(n, min = 0, max = 1) {})
updateOperatorDef('rweibull', 'matchDef', val = function(n, shape, scale = 1) {})

assignOperatorDef(
  c('length'),
  list(
    labelAbstractTypes = list(
      handler = 'UnaryReduction',
      returnTypeCode = returnTypeCodes$integer
    ),
    eigenImpl = list(
      handler = 'Reduction',
      noPromotion = TRUE,
      replaceForScalar = 1,
      method = TRUE
    ),
    cppOutput = list(
      cppString = 'size'
    )
  )
)

assignOperatorDef(
  'asSparse',
  list(
    labelAbstractTypes = list(
      handler = 'asSparse'
    ),
    eigenImpl = list(
      handler = 'asSparse'
    )
  )
)

assignOperatorDef(
  'asDense',
  list(
    labelAbstractTypes = list(
      handler = 'asDense'
    ),
    eigenImpl = list(
      handler = 'asDense'
    )
  )
)

assignOperatorDef(
  c('nrow', 'ncol'),
  list(
    labelAbstractTypes = list(
      handler = 'RecurseAndLabel',
      returnTypeCode = returnTypeCodes$integer
    )
  )
)

assignOperatorDef(
  c('nChol'),
  list(
    labelAbstractTypes = list(
      handler = 'nChol',
      returnTypeCode = returnTypeCodes$double
    )
  )
)

assignOperatorDef(
  c('nLogdet'),
  list(
    labelAbstractTypes = list(
      handler = 'UnaryReduction'
    )
  )
)

assignOperatorDef(
  c('nDiag', 'nDiagonal'),
  list(
    labelAbstractTypes = list(
      handler = 'Diag'
    ),
    eigenImpl = list(
      handler = 'Diag'
    )
  )
)

assignOperatorDef(
  c('nForwardsolve', 'nBacksolve', 'nSolve'),
  list(
    labelAbstractTypes = list(
      handler = 'ArgReturnType',
      argTypeInd = 2
    )
  )
)

assignOperatorDef(
  c('t'),
  list(
    labelAbstractTypes = list(
      handler = 'ArgReturnType',
      argTypeInd = 1
    )
  )
)

assignOperatorDef(
  'nEigen',
  list(
    matchDef = function(x, symmetric, valuesOnly=FALSE) {},
    labelAbstractTypes = list(
      handler = 'nEigen'
    )
  )
)

assignOperatorDef(
  'nSvd',
  list(
    labelAbstractTypes = list(
      handler = 'nSvd'
    )
  )
)

assignOperatorDef(
  'nOptim',
  list(
    labelAbstractTypes = list(
      handler = 'nOptim'
    ),
    cppOutput = list(
      handler = 'PrependNamespace'
    )
  )
)

assignOperatorDef(
  'nDerivs',
  list(
    matchDef = function(call, wrt, order, update = TRUE, reset = FALSE, roles) {},
    labelAbstractTypes = list(
      handler = 'nDerivs'
    ),
    processAD = list(
      handler = 'nDerivs'
    ),
    cppOutput = list(
      handler = 'nDerivs'
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
#    length = 'size',
    is.nan = 'ISNAN',
    is.nan.vec = 'ISNAN',
    is.na = 'ISNA',
    is.na.vec = 'ISNA',
#    lgamma = 'lgammafn',
#    logfact = 'lfactorial',
#    loggam = 'lgammafn',
#    gamma = 'gammafn',
#    expit = 'ilogit',
#    phi = 'iprobit',
#    ceiling = 'ceil',
#    trunc = 'ftrunc',
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

ifOrWhile <- c('if','while')

# aliasRiskOperators <- c('t', 'asRow') # If we develop asRow
# These are currently hard-coded in compile_eigenize system.

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
