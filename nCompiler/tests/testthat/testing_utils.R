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

make_input <- function(argTypes, input_gen_funs = NULL) {
  if (is.null(input_gen_funs) || is.null(names(input_gen_funs)))
    if (length(input_gen_funs) <= 1)
      input <- lapply(argTypes, argType_2_input, input_gen_funs)
  else
    stop(
      'input_gen_funs of length greater than 1 must have names',
      call. = FALSE
    )
  else {
    input <- sapply(
      names(argTypes),
      function(name)
        argType_2_input(argTypes[[name]], input_gen_funs[[name]]),
      simplify = FALSE
    )
  }
  ##
  ## generate inputs that depend on the other inputs
  ##
  is_fun <- sapply(input, is.function)
  input[is_fun] <- lapply(
    input[is_fun], function(fun) {
      eval(as.call(c(fun, input[names(formals(fun))])))
    }
  )
  input
}

## Takes an argSymbol and if argSymbol$size is NA adds default sizes.
add_missing_size <- function(argSymbol, vector_size = 3, matrix_size = c(3, 4),
                             array_size = c(3, 4, 5)) {
  if (argSymbol$nDim > 3)
    stop(
      'Testing does not currently support args with nDim > 3',
      call. = FALSE
    )
  if (any(is.na(argSymbol$size)))
    argSymbol$size <- switch(argSymbol$nDim,
                             vector_size,
                             matrix_size,
                             array_size)
  invisible(argSymbol)
}

argType_2_input <- function(argType, input_gen_fun = NULL) {
  argSymbol <- add_missing_size(
    nCompiler:::argType2symbol(argType)
  )
  type <- argSymbol$type
  nDim <- argSymbol$nDim
  size <- argSymbol$size
  if (is.null(input_gen_fun))
    input_gen_fun <- function(arg_size, arg_type) {
      switch(
        arg_type,
        "double"  = rnorm(prod(arg_size)),
        "integer" = rgeom(prod(arg_size), 0.5),
        "logical" = sample(c(TRUE, FALSE), prod(arg_size), replace = TRUE)
      )
    }
  arg <- switch(
    nDim + 1,
    input_gen_fun(1, type), ## nDim is 0
    input_gen_fun(size, type), ## nDim is 1
    matrix(input_gen_fun(size, type), nrow = size[1], ncol = size[2]), ## nDim is 2
    array(input_gen_fun(size, type), dim = size) ## nDim is 3
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
                      compile_all_funs = FALSE, gold_test = FALSE,
                      suppress_err_msgs = TRUE, ...) {
  # TODO: port over the nimble AD testing knownFailure setup
  compile_error <- sapply(
    param_list, function(param)
      !is.null(param$knownFailure) && grepl('compiles', param$knownFailure)
  )

  ## only test knownFailures in full testing
  if (isFALSE(gold_test)) {
    ## these tests should fail during compilation
    error_params <- param_list[compile_error]
    if (length(error_params) > 0) {
      nFuns_error <- lapply(error_params, gen_nFunction)
      names(nFuns_error) <- paste0('nFun_error', 1:length(nFuns_error))
      for (i in seq_along(nFuns_error)) {
        if (verbose)
          cat(paste('### Known compilation failure: ',
                    error_params[[i]]$name, '###\n'))
        test_that(paste0(test_name, " knownFailures fail to compile"), {
          if (suppress_err_msgs)
            expect_error(capture.output(
              nCompile_nFunction(nFuns_error[[i]],
                                 control = control)))
          else
            expect_error(nCompile_nFunction(nFuns_error[[i]],
                                            control = control))
        })
      }
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
        nCompile_nFunction(nFuns[[i]], control = control)
      }
    }
    if (is.function(test_fun))
      test_fun( # run remainder of test
        list(param_list = compiles, nC = nC, test_name = test_name),
        control = control, verbose = verbose, ...
      )
  }
}

wrap_if_matches <- function(pattern, string, wrapper, expr) {
  if (!is.null(pattern) && any(grepl(paste0('^', pattern, '$'), string))) {
    wrapper(expr)
  } else {
    expr
  }
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
get_matching_ops <- function(field, subfield = NULL, test = isTRUE) {
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
    aot <- nCompiler:::arithmeticOutputType(args[[1]]$type, args[[2]]$type,
                                            returnTypeCode)
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

  nDim <- if (isTRUE(reduction_op)) 0 else max(sapply(args, `[[`, 'nDim'))

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
  } else if (nDim %in% c(2, 3)) {
    # one arg is a matrix (but this is not matrix multiplication) or an array,
    # so assume that output size is same as the first arg with this nDim
    has_right_nDim <- sapply(args, function(arg) arg$nDim == nDim)
    args[has_right_nDim][[1]]$size
  } else {
    # nDim is 1 so either recycling rule or simple vector operator
    if (is.null(args[[1]]$size)) NULL
    else max(sapply(args, `[[`, 'size'))
  }

  size_string <- if (is.null(sizes) || is.na(sizes)) '' else {
    if (nDim == 1) size_str <- sizes
    else {
      size_str <- paste0('size = c(', paste(sizes, collapse = ', '), ')')
      if (nDim == 3) size_str <- paste0(', ', size_str)
    }

    size_str
  }

  dimString <- switch(
    nDim + 1,
    'Scalar()', # nDim is 0
    paste0('Vector(', size_string, ')'), # nDim is 1
    paste0('Matrix(', size_string, ')'), # nDim is 2
    paste0('Array(nDim = 3', size_string, ')') # nDim is 3
  )

  return(paste0(scalarTypeString, dimString))
}

make_test_param <- function(op, argTypes, input_gen_funs = NULL, more_args = NULL) {
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
    input_gen_funs = input_gen_funs,
    returnType = return_type_string(op, argTypes)
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
                           ), write_gold_file = FALSE, batch_mode = FALSE,
                           ...) {
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
## ...:             Additional arguments to pass to test_base
##
run_test_suite <- function(batch_of_ops, test_name = '', test_fun = NULL,
                           full = FALSE, granularity = NA,
                           write_gold_file = FALSE, gold_file_dir = system.file(
                             file.path('tests', 'testthat', 'gold_files'),
                             package = 'nCompiler'
                           ), ...) {
  if (!full) {

    ## Run gold file testing with one file per op in batch_of_ops.
    ## Don't pass test_fun to test_base so that only gold testing runs.
    for (op in names(batch_of_ops)) {
      test_base(
        batch_of_ops[[op]], paste(c(test_name, op), collapse = '_'),
        gold_test = TRUE, write_gold_file = write_gold_file,
        gold_file_dir = gold_file_dir, ...
      )
    }

  } else if (isTRUE(granularity == 3)) {

    for (test_param in unlist(batch_of_ops, recursive = FALSE))
      ## TODO: avoid having to wrap test_param in list()?
      test_base(list(test_param), test_param$name, test_fun, ...)

  } else if (isTRUE(granularity == 2)) {

    for (op in names(batch_of_ops))
      test_base(batch_of_ops[[op]], op, test_fun, ...)

  } else if (isTRUE(granularity == 1)) {

    test_base(unlist(batch_of_ops, recursive = FALSE), test_name, test_fun, ...)
  }
}

#############################
## input generation functions
#############################

## arg_size comes from arg$size where arg is a symbolBasic object
gen_pos_def_matrix <- function(arg_size) {
  m <- arg_size[1] ## assumes matrix argType is square
  mat <- diag(1:m)
  mat[lower.tri(mat)] <- runif(m*(m - 1)/2)
  mat %*% t(mat)
}
