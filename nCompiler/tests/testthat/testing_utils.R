require(nCompiler)
require(testthat)

## ...: Any number of character strings representing valid argTypes, e.g.
##      make_arg_tuples('numericScalar', 'logicalVector(7)', 'integerMatrix')
##      gives the same result as:
##      list(c("numericScalar", "numericScalar"),
##        c("logicalVector(7)", "numericScalar"),
##        c("integerMatrix", "numericScalar"),
##        c("numericScalar", "logicalVector(7)"),
##        c("logicalVector(7)", "logicalVector(7)"),
##        c("integerMatrix", "logicalVector(7)"),
##        c("numericScalar", "integerMatrix"),
##        c("logicalVector(7)", "integerMatrix"),
##        c("integerMatrix", "integerMatrix"))
## rhs: An optional character vector of argTypes to restrict the right hand
##      side of the tuple, in case we don't want to reuse ... on the right.
make_argType_tuples <- function(..., rhs = NULL) {
  argTypes <- c(...)
  if (is.null(rhs)) rhs <- argTypes
  ans <- as.list(
    data.frame(t(expand.grid(argTypes, rhs)), stringsAsFactors=FALSE)
  )
  names(ans) <- NULL
  ans
}

make_input <- function(argType, argCheck = NULL, size = 3) {
  arg <- switch(
    argType,
    "numericScalar" = rnorm(1),
    "integerScalar" = rgeom(1, 0.5),
    "logicalScalar" = sample(c(TRUE, FALSE), size = 1),
    "numericVector" = rnorm(size),
    "integerVector" = rgeom(size, 0.5),
    "logicalVector" = sample(c(TRUE, FALSE), size = size, replace = TRUE),
    ## Make different sized dimensions to avoid bugs that might be hidden by
    ## symmetry.
    "numericMatrix" = matrix(rnorm(size*(size+1)), nrow = size, ncol = size+1),
    "integerMatrix" = matrix(
      rgeom(size*(size+1), 0.5), nrow = size, ncol = size+1
    ),
    "logicalMatrix" = matrix(
      sample(c(TRUE, FALSE), size*(size+1), replace = TRUE),
      nrow = size, ncol = size+1
    ),
    "numericArray(nDim=3)" = array(rnorm(size*(size+1)*(size+2)), dim = size + 0:2),
    "integerArray(nDim=3)" = array(
      rgeom(size*(size+1)*(size+2), 0.5), dim = size + 0:2
    ),
    "logicalArray(nDim=3)" = array(
      sample(c(TRUE, FALSE), size*(size+1)*(size+2), replace = TRUE),
      dim = size + 0:2
    )
  )
  ## try again if argCheck returns FALSE
  if (!is.null(argCheck) && !argCheck(arg))
    return(make_input(argType, argCheck, size))
  else
    return(arg)
}

## Takes an argSymbol and if argSymbol$size is NA adds default sizes.
add_missing_size <- function(argSymbol, vector_size = 3, matrix_size = c(3, 4)) {
  if (any(is.na(argSymbol$size))) {
    if (argSymbol$nDim == 1)
      argSymbol$size <- vector_size
    else if (argSymbol$nDim == 2)
      argSymbol$size <- matrix_size
  }
  invisible(argSymbol)
}

arg_type_2_input <- function(argType, input_gen_fun = NULL) {
  argSymbol <- add_missing_size(
    nCompiler:::argType2symbol(argType)
  )
  type <- argSymbol$type
  nDim <- argSymbol$nDim
  size <- argSymbol$size
  if (is.null(input_gen_fun))
    input_gen_fun <- switch(
      type,
      "double"  = function(arg_size) rnorm(prod(arg_size)),
      "integer" = function(arg_size) rgeom(prod(arg_size), 0.5),
      "logical" = function(arg_size)
        sample(c(TRUE, FALSE), prod(arg_size), replace = TRUE)
    )
  arg <- switch(
    nDim + 1,
    input_gen_fun(1), ## nDim is 0
    input_gen_fun(size), ## nDim is 1
    matrix(input_gen_fun(size), nrow = size[1], ncol = size[2]), ## nDim is 2
    array(input_gen_fun(size), dim = size) ## nDim is 3
  )
  if (is.null(arg))
    stop('Something went wrong while making test input.', call.=FALSE)
  return(arg)
}

gen_nFunction <- function(param) {
  fun <- function() {}
  formals(fun) <- lapply(param$argTypes, function(x) quote(expr=))
  tmp <- quote({})
  tmp[[2]] <- param$expr
  tmp[[3]] <- quote(return(ans))
  body(fun) <- tmp
  return(
    nFunction(
      fun, argTypes = param$argTypes, returnType = param$returnType
    )
  )
}

gen_nClass <- function(param) {
  return(
    nClass(
      Rpublic = param$Rpublic,
      Cpublic = param$Cpublic,
      enableDerivs = param$enableDerivs
    )
  )
}

## Run a few early steps that are common to test_math and test_AD:
##
## 1. Isolate params that lead to known compilation failures and if not gold
##    testing, verify that those params do indeed fail to compile.
## 2. Isolate params that are not known to lead to compilation failures and
##    create an nFunction for each.
## 3. Put the nFunctions in one nClass and:
##    a) If this is a gold test, run the test and return.
##    b) If not and compile_all_funs is TRUE, compile each nFunction
##       individually.
## 4. Compile the nClass and return the compiled object.
##
## TODO: add argument descriptions
##
test_base <- function(param_list, test_name = '', test_fun = NULL,
                      dir = file.path(tempdir(), "nCompiler_generatedCode"),
                      control = list(), verbose = nOptions('verbose'),
                      compile_all_funs = FALSE, gold_test = FALSE, ...) {
  # TODO: port over the nimble AD testing knownFailure setup
  compile_error <- sapply(
    param_list, function(param)
      !is.null(param$knownFailure) && grepl('compiles', param$knownFailure)
  )

  ## only test knownFailures in full testing
  if (isFALSE(gold_test)) {
    ## these tests should fail during compilation
    nFuns_error <- lapply(param_list[compile_error], gen_nFunction)
    if (length(nFuns_error) > 0) {
      names(nFuns_error) <- paste0('nFun_error', 1:length(nFuns_error))
      ## TODO: compile all nFunctions to ensure each one fails to compile?
      nC_error <- gen_nClass(list(Cpublic = nFuns_error))
      test_that(paste0(test_name, " knownFailures fail to compile"),
                expect_error(nCompile_nClass(nC_error, control = control))
                )
    }
  }

  ## these tests should compile
  compiles <- param_list[!compile_error]

  ## Ensure that generated strings for unique names
  ## have counters that start at 1
  nCompiler:::resetLabelFunctionCreators()

  nFuns <- lapply(compiles, gen_nFunction)

  if (length(nFuns) > 0) {

    nFun_names <- names(nFuns) ## for verbose output
    names(nFuns) <- paste0('nFun', 1:length(nFuns))

    enableDerivs_filter <- sapply(compiles, function(param) {
      isTRUE(param$enableDerivs)
    })

    nC <- gen_nClass(list(
      Cpublic = nFuns,
      enableDerivs = names(nFuns)[enableDerivs_filter]
    ))

    ## if gold_test is TRUE, go directly to nCompiler stages without any
    ## checking for knownFailures and without proceeding to C++ compilation
    if (isTRUE(gold_test)) {
      control$endStage <- 'makeRcppPacket'
      ## make sure generated class / function names are the same every time
      return(
        test_gold_file(
          nCompile_nClass(nC, control = control), test_name, ...)
      )
    } else if (compile_all_funs) {
      ## useful for debugging an nClass compilation failure
      ## e.g. test_math(binaryOpTests[['+']], compile_all_funs = TRUE)
      for (i in seq_along(nFuns)) {
        if (verbose)
          cat(paste('### Compiling function for test of:', nFun_names[i], '###\n'))
        nCompile_nFunction(nFuns[[i]], control)
      }
    }
    if (is.function(test_fun))
      test_fun( # run remainder of test
        list(param_list = compiles, nC = nC, test_name = test_name),
        control = control, verbose = verbose
      )
  }
}

## Runs test_fun on a list of params.
## ... additional args to pass to test_gold_file
test_batch <- function(test_fun, batch,
                       test_name = deparse(substitute(batch)), size = 3,
                       dir = file.path(tempdir(), "nCompiler_generatedCode"),
                       control = list(), verbose = nOptions('verbose'),
                       skip = c(), gold_test = FALSE, ...) {
  indices <- seq_along(batch)
  if (is.numeric(skip) && all(skip > 0 & skip %% 1 == 0))
    indices <- indices[-skip] ## skip is indices in batch to skip
  if (is.character(skip))
    skip <- names(batch) %in% skip ## convert to logical
  if (is.logical(skip) && length(skip) == length(indices))
    indices <- indices[!skip] ## skip[i] is TRUE if we should skip the corresponding test

  if (isFALSE(gold_test)) {
    for (i in indices) {
      param <- batch[[i]]
      name <- names(batch)[i]
      msg <- paste0(test_name, ' #', i, ' (', name, ')')
      if (is.null(param$skip) || !param$skip) {
        if (verbose) {
          cat("### -------------------------------------------- ###\n")
          cat(paste0("### Testing ", msg, " ###\n"))
        }
        test_param(
          test_fun, batch[[i]], msg, size, dir, control, verbose, ...
        )
        if (verbose) cat("### -------------------------------------------- ###\n")
      } else if (verbose) {
        cat("### -------------------------------------------- ###\n")
        cat(paste0('### Skipping ', msg, ' ###\n'))
        cat("### -------------------------------------------- ###\n")
      }
    }

  } else {
    RcppPackets <- lapply(
      batch, test_fun,
      test_name, size, dir, control, verbose,
      gold_test = TRUE, batch_mode = TRUE
    )
    hContent <- unlist(sapply(RcppPackets, `[[`, 'hContent'))
    cppContent <- unlist(sapply(RcppPackets, `[[`, 'cppContent'))
    RcppPacket <- list(
      hContent = hContent,
      cppContent = cppContent
      ## filebase not needed
    )
    ## construct an RcppPacket for test_gold_file
    ## If ... contains write_gold_file = TRUE, then it is just written.
    ## Otherwise, read gold_file and check that current generated code matches.
    test_gold_file(RcppPacket, ...)
  }
}

## TODO: this function needs to be refactored or removed
## This is a parametrized test, where `param_list` is a list of lists with names:
##   param$name - An op name
##   param$expr - A quoted expression `quote(out <- some_function_of(arg1, arg2, ...))`.
##   param$argTypes - A list of the input types.
##   param$returnType - The output type character string.
test_param <- function(test_fun, param_list, test_name = '', size = 3,
                       dir = file.path(tempdir(), "nCompiler_generatedCode"),
                       control = list(), verbose = nOptions('verbose'), ...) {
  if (!is.list(param_list)) stop('param must be a list', call.=FALSE)

  ## in some cases, expect_error does not suppress error messages (I believe
  ## this has to do with how we trap errors in compilation), so make sure user
  ## realizes expectation
  if('knownFailureReport' %in% names(param_list) && param_list$knownFailureReport)
    cat("\nBegin expected error message:\n")

  test_that(test_name, {
    test_fun(param_list, test_name, size, dir, control, verbose, ...)
  })

  invisible(NULL)
}

wrap_if_matches <- function(pattern, string, wrapper, expr) {
  if (!is.null(pattern) && any(grepl(paste0('^', pattern, '$'), string))) {
    wrapper(expr)
  } else {
    expr
  }
}

inverseCallReplacements <- as.list(
  names(nCompiler:::specificCallReplacements)
)
names(inverseCallReplacements) <- unlist(
  nCompiler:::specificCallReplacements
)
inverseReplace <- function(x) {
  replacement <- inverseCallReplacements[[x]]
  if(is.null(replacement)) x else replacement
}

modifyOnMatch <- function(x, pattern, key, value, env = parent.frame(), ...) {
  ## Modify any elements of a named list that match pattern.
  ##
  ## @param x A named list of lists.
  ## @param pattern A regex pattern to compare with `names(x)`.
  ## @param key The key to modify in any lists whose names match `pattern`.
  ## @param value The new value for `key`.
  ## @param env The environment in which to modify `x`.
  ## @param ... Additional arguments for `grepl`.
  for (name in names(x)) {
    if (grepl(pattern, name, ...)) {
      eval(substitute(x[[name]][[key]] <- value), env)
    }
  }
}

modifyBatchOnMatch <-
  function(x, op_pattern, arg_pattern, key, value, env = parent.frame(), ...) {
    ## Modify the entries of a data structure like unaryOpTests from test-math.R.
    ##
    ## @param x A named list of lists.
    ## @param op_regex A regex pattern to compare with `names(x)`.
    ## @param arg_regex A regex pattern to combine with op_regex and pass
    ##                  to `modifyOnMatch`.
    ## @param key The key to modify in any lists whose names match `pattern`.
    ## @param value The new value for `key`.
    ## @param env The environment in which to modify `x`.
    ## @param ... Additional arguments for `grepl`.
    for (op in names(x)) {
      if (grepl(op_pattern, op, ...))
        eval(substitute(modifyOnMatch(x[[op]], arg_pattern, key, value)), env)
    }
  }

## field:    An operator def field, such as 'testing'.
## subfield: An optional subfield of the 'field', e.g. 'argTypes'.
## test:     A function which returns TRUE or FALSE based on a single
##           value.
get_matching_ops <- function(field, subfield = NULL, test) {
  ## Returns vector of operator names where the value in a given field (or its
  ## subfield) returns TRUE when the test function is applied to it.
  ops <- ls(nCompiler:::operatorDefEnv)
  values <- sapply(ops, nCompiler:::getOperatorDef, field, subfield)
  if (is.null(values)) return(character(0))
  names(values)[sapply(values, test)]
}

get_ops_values <- function(field, subfield = NULL) {
  ## Return a named (by operator) list of the values found in field/subfield.
  ops <- ls(nCompiler:::operatorDefEnv)
  values <- sapply(ops, nCompiler:::getOperatorDef, field, subfield,
                   simplify = FALSE)
  non_null <- sapply(values, function(x) !is.null(x))
  return(values[non_null])
}

## Takes an operator and its input types as a character vector and
## creates a string representing the returnType for the operation.
##
## op:       An operator string
## argTypes: A character vector of argTypes (e.g. "double(0)".
##
return_type_string <- function(op, argTypes) {

  returnTypeCode <- nCompiler:::getOperatorDef(op, 'labelAbstractTypes',
                                               'returnTypeCode')
  recycling_rule_op <- nCompiler:::getOperatorDef(op, 'testing',
                                                  'recyclingRuleOp')

  if (is.null(returnTypeCode))
    if (!isTRUE(recycling_rule_op)) return(argTypes[1])
  else returnTypeCode <- 1

  scalarTypeString <- switch(
    returnTypeCode,
    'numeric', # 1
    'integer', # 2
    'logical'  # 3
  )

  args <- lapply(
    argTypes, function(argType)
      nCompiler:::argType2symbol(argType)
  )

  if (is.null(scalarTypeString)) ## returnTypeCode is 4 or 5
    scalarTypeString <-
      if (length(argTypes) == 1)
        if (returnTypeCode == 5 && args[[1]]$type == 'logical') 'integer'
  else args[[1]]$type
  else if (length(argTypes) == 2) {
    aot <- nCompiler:::arithmeticOutputType(args[[1]]$type, args[[2]]$type)
    if (returnTypeCode == 5 && aot == 'logical') 'integer'
    else aot
  } else {
    stop(
      paste0(
        'Testing does not currently know how to handle ops with more than 2',
        ' args and returnTypeCode not equal to 1, 2, or 3.',
        call. = FALSE
      )
    )
  }

  ## arithmeticOutputType might return 'double'
  if (scalarTypeString == 'double') scalarTypeString <- 'numeric'

  reduction_op <- nCompiler:::getOperatorDef(op, 'testing', 'reductionOp')

  # TODO: other labelAbstractTypes handlers for reductions?
  nDim <- if (isTRUE(reduction_op)) 0
  else max(sapply(args, `[[`, 'nDim'))

  if (nDim > 3)
    stop(
      'Testing does not currently support args with nDim > 3',
      call. = FALSE
    )

  matrix_mult_op <- nCompiler:::getOperatorDef(op, 'testing', 'matrixMultOp')

  # if arg sizes weren't provided this will just be NULL
  sizes <- if (nDim == 0) NULL
  else if (length(argTypes) == 1) args[[1]]$size
  else if (isTRUE(matrix_mult_op))  {
    if (!length(argTypes) == 2)
      stop(
        paste0(
          'matrixMultOps should only have 2 args but got ',
          length(argTypes)
        ), call. = FALSE
      )
    if (is.null(args[[1]]$size)) NULL
    else c(args[[1]]$size[1], args[[2]]$size[2])
  } else if (nDim == 2) {
    # one arg is a matrix but this is not matrix multiplication so
    # assume that output size is same as the first arg with nDim == 2
    has_right_nDim <- sapply(args, function(arg) arg$nDim == nDim)
    args[has_right_nDim][[1]]$size
  } else {
    # nDim is 1 so either recycling rule or simple vector operator
    if (is.null(args[[1]]$size)) NULL
    else max((sapply(args, `[[`, 'size')))
  }

  size_string <- if (is.null(sizes)) 'NA' else paste0(
    'c(', paste(sizes, collapse = ', '), ')'
  )

  dimString <- switch(
    nDim + 1,
    'Scalar()', # nDim is 0
    paste0('Vector(', size_string, ')'), # nDim is 1
    paste0('Matrix(sizes = ', size_string, ')'), # nDim is 2
    paste0('Array(nDim = 3, sizes = ', size_string, ')') # nDim is 3
  )

  return(paste0(scalarTypeString, dimString))
}

# TODO: replace usage of returnTypeString with return_type_string and remove.
returnTypeString <- function(op, argTypes) {
  ## Takes an operator and its input types as a character vector and
  ## creates a string representing the returnType for the operation.
  if (!is.character(argTypes))
    stop('Argument `argTypes` must be a character vector.', call.=FALSE)
  if (!length(argTypes) %in% c(1L, 2L))
    stop('Can only `argTypes` of length 1 or 2.', call.=FALSE)

  returnTypeCode <-
    nCompiler:::getOperatorDef(op, 'labelAbstractTypes', 'returnTypeCode')
  if (is.null(returnTypeCode)) return(argTypes[1])
  
  arg1 <- nCompiler:::argType2symbol(argTypes[1])
  if (length(argTypes) == 2)
    arg2 <- nCompiler:::argType2symbol(argTypes[2])
  
  scalarTypeString <- switch(
    returnTypeCode,
    'numeric', ## 1
    'integer', ## 2
    'logical'  ## 3
  )

  if (is.null(scalarTypeString)) ## returnTypeCode is 4 or 5
    scalarTypeString <-
      if (length(argTypes) == 1)
        nCompiler:::arithmeticOutputType(
          arg1$type, returnTypeCode = returnTypeCode
        )
  else
    nCompiler:::arithmeticOutputType(
      arg1$type, arg2$type, returnTypeCode
    )

  ## arithmeticOutputType might return 'double'
  if (scalarTypeString == 'double') scalarTypeString <- 'numeric'

  nDim <- if (length(argTypes) == 1) {
    handler <- nCompiler:::getOperatorDef(
      op, 'labelAbstractTypes', 'handler'
    )
    if (!is.null(handler) && handler == 'UnaryReduction') 0
    else arg1$nDim
  } else max(arg1$nDim, arg2$nDim)

  dimString <- switch(
    nDim + 1,
    'Scalar', ## nDim is 0
    'Vector', ## nDim is 1
    'Matrix', ## nDim is 2
    'Array(nDim=3)', ## nDim is 3
    stop('Cannot handle argTypes with nDim > 3.')
  )

  return(paste0(scalarTypeString, dimString))
}

make_test_param <- function(op, argTypes, more_args = NULL) {
  arg_names <- names(argTypes)

  if (is.null(arg_names)) {
    arg_names <- paste0('arg', 1:length(argTypes))
    op_args <- lapply(arg_names, as.name)
  } else {
    op_args <- sapply(arg_names, as.name, simplify = FALSE)
  }

  args_string <- paste0(arg_names, ' = ', argTypes, collapse = ' ')
  name <- paste(op, args_string)

  expr <- substitute(
    ans <- this_call,
    list(
      this_call = as.call(c(
        substitute(FOO, list(FOO = as.name(op))),
        op_args, more_args
      ))
    )
  )

  argTypesList <- as.list(argTypes)
  names(argTypesList) <- arg_names
  argTypesList <- lapply(argTypesList, function(arg) {
    parse(text = arg)[[1]]
  })

  list(
    name = name,
    expr = expr,
    argTypes = argTypesList,
    returnType = return_type_string(op, argTypes)
  )
}

# TODO: replace usage of makeOperatorParam with make_test_param and remove.
makeOperatorParam <- function(op, argTypes) {
  if (length(argTypes) == 1) {
    name <- paste(op, argTypes)
    expr <- substitute(
      ans <- FOO(arg1), list(FOO = as.name(inverseReplace(op)))
    )
  } else if (length(argTypes) == 2) {
    name <- paste(op, argTypes[1], argTypes[2])
    expr <- substitute(
      ans <- FOO(arg1, arg2), list(FOO = as.name(inverseReplace(op)))
    )
  }  else {
    stop("Cannot currently handle testing with more than 2 arguments.",
         call. = FALSE)
  }

  argTypesList <- as.list(argTypes)
  names(argTypesList) <- paste0('arg', 1:length(argTypes))

  list(
    name = name,
    expr = expr,
    argTypes = argTypesList,
    returnType = returnTypeString(op, argTypes)
  )
}

getBinaryArgChecks <- function(op) {
  ## not implemented
  switch(
    op,
    #'/' = list(NULL, function(x) {
    #  if (length(x) == 1) x != 0
    #  else TRUE
    #},
    list(NULL, NULL)
  )
}

compare_files_using_diff <- function(trial_file, correct_file, main = "") {
  if (main == "") main <- paste0(trial_file, ' and ', correct_file, ' do not match\n')
  diff_out <- system2('diff', c(trial_file, correct_file), stdout = TRUE)
  test_that(
    paste0(main, paste0(diff_out, collapse = '\n')),
    expect_true(length(diff_out) == 0)
  )
  invisible(NULL)
}

test_gold_file <- function(uncompiled, filename = paste0('test_', date()),
                           gold_file_dir = system.file(
                             file.path('tests', 'testthat', 'gold_files'),
                             package = 'nCompiler'
                           ), write_gold_file = FALSE, batch_mode = FALSE) {
  filename <- paste0(gsub(' ', '_', filename), '.gold')
  ## replace operators that can't be used in filenames with an alphabetic name
  ## greedily replace by ordering according to decreasing number of characters
  replacements <- unlist(get_ops_values('testing', 'alpha_name'))
  replacements <- replacements[
    order(nchar(names(replacements)), decreasing = TRUE)
  ]
  for (i in seq_along(replacements)) {
    filename <- gsub(
      names(replacements)[i], replacements[i], filename, fixed = TRUE
    )
  }
  filepath <- file.path(gold_file_dir, filename)
  if (isNF(uncompiled)) {
    RcppPacket <- NFinternals(uncompiled)$RcppPacket
  } else if (inherits(uncompiled, 'cpp_nClassClass'))
    RcppPacket <- nCompiler:::cppDefs_2_RcppPacket(
      uncompiled, filebase = '.'
      ## filebase won't be used since we provide 'con' to writeCpp_nCompiler
    )
  else ## assume uncompiled is an RcppPacket
    RcppPacket <- uncompiled
  if (isTRUE(write_gold_file)) { ## either create or overwrite gold_file
    con <- file(filepath, open = "w")
    nCompiler:::writeCpp_nCompiler(
      RcppPacket, con = con
    )
    close(con)
  } else if (isFALSE(batch_mode)) {
    ## read the existing gold file and compare to the current RcppPacket
    temp_file <- paste0(filepath, 'tmp')
    con <- file(temp_file, open = "w")
    nCompiler:::writeCpp_nCompiler(
      RcppPacket, con = con
    )
    close(con)
    compare_files_using_diff(temp_file, filepath)
    ## TODO: file.remove(temp_file) ?
  }
  invisible(RcppPacket)
}

## batch_of_ops:    A list such as that made by make_AD_test_batch(),
##                  with one named entry per operator,
##                  which itself is a list with any number of
##                  test parameterizations (op + arg types), e.g.:
##                  list(
##                    '-' = list(
##                      '- arg1 = numericScalar arg2 = numericScalar' = list(
##                        name = '- arg1 = numericScalar arg2 = numericScalar',
##                        expr = quote(ans <- arg1 - arg2),
##                        argTypes = list(
##                          quote(numericScalar),
##                          quote(numericScalar)
##                        ),
##                        # TODO: why string when argTypes are quoted?
##                        returnType = 'numericScalar()',
##                        wrts = list(
##                          'arg1',
##                          'arg2',
##                          c('arg1', 'arg2'),
##                          c('arg2', 'arg1')
##                        )
##                      ),
##                      '- arg1 = numericScalar arg2 = numericVector(7)' = list(
##                        ...
##                      )
##                    ),
##                    '+' = list(
##                      ...
##                    )
##                  )
## test_name:       Optional name describing the batch_of_ops (e.g. "unaryOpTests").
## test_fun:        Optional testing function (such as test_math() or test_AD()) to
##                  use in test_base().
## full:            If TRUE, run full testing rather than gold file testing.
## granularity:     When 'full' is TRUE, determines how to  divide the batch_of_ops
##                  into separate calls to test_base():
##                  1 = not currently handled by run_test_suite
##                  2 = put all test params in 'batch_of_ops' in one big nClass
##                  3 = one nClass per operator (this is also what gold testing does)
##                  4 = one nClass with one nFunction per operator/input combo
## write_gold_file: When TRUE, test_gold_file() saves gold files to
##                  gold_file_dir. Has no effect when 'full' is TRUE.
## gold_file_dir:   Where test_gold_file() should look for and save gold files.
##
## TODO: improve the way this works / is used to include granularity = 1
run_test_suite <- function(batch_of_ops, test_name = '', test_fun = NULL,
                           full = FALSE, granularity = NA,
                           write_gold_file = FALSE, gold_file_dir = system.file(
                             file.path('tests', 'testthat', 'gold_files'),
                             package = 'nCompiler'
                           )) {
  if (!full) {

    ## Run gold file testing with one file per op in batch_of_ops.
    ## Don't pass test_fun to test_base so that only gold testing runs.
    for (op in names(batch_of_ops)) {
      test_base(
        batch_of_ops[[op]], paste(c(test_name, op), collapse = '_'),
        gold_test = TRUE, write_gold_file = write_gold_file,
        gold_file_dir = gold_file_dir
      )
    }

  } else if (isTRUE(granularity == 3)) {

    for (test_param in unlist(batch_of_ops, recursive = FALSE))
      ## TODO: avoid having to wrap test_param in list()?
      test_base(list(test_param), test_param$name, test_fun)

  } else if (isTRUE(granularity == 2)) {

    for (op in names(batch_of_ops))
      test_base(batch_of_ops[[op]], op, test_fun)

  } else if (isTRUE(granularity == 1)) {

    test_base(unlist(batch_of_ops, recursive = FALSE), test_name, test_fun)
  }
}
