require(nCompiler)
require(testthat)

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

## Runs test_fun on a list of params.
## ... additional args to pass to test_gold_file
test_batch <- function(test_fun, batch,
                       case_name = deparse(substitute(batch)), size = 3,
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

  if (identical(gold_file, '')) {
    for (i in indices) {
      param <- batch[[i]]
      name <- names(batch)[i]
      msg <- paste0(case_name, ' #', i, ' (', name, ')')
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
    ## TODO: use skip in gold file testing?
    RcppPackets <- lapply(
      batch, test_fun,
      '', size, dir, control, verbose,
      gold_file = gold_file, batch_mode = TRUE
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
    test_gold_file(RcppPacket, , ...)
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

  test_that(case_name, {
    test_fun(param, test_name, size, dir, control, verbose, ...)
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


getMatchingOps <- function(field, key, value) {
  ## Returns vector of operator names where a given field has
  ## a key with a given value, or an empty vector if no matches.
  ops <- ls(nCompiler:::operatorDefEnv)
  values <- unlist(
    sapply(ops, nCompiler:::getOperatorDef, field, key)
  )
  if (is.null(values)) return(character(0))
  names(values)[values == value]
}

get_ops_values <- function(field, key) {
  ## Returns vector of operator names where a given field has
  ## a key with a given value, or an empty vector if no matches.
  ops <- ls(nCompiler:::operatorDefEnv)
  values <- unlist(
    sapply(ops, nCompiler:::getOperatorDef, field, key)
  )
  if (is.null(values)) return(character(0))
  return(values)
}

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

## TODO: decide how to integrate this function with test_batch
test_gold_file <- function(uncompiled, filename = paste0('test_', date()),
                           gold_file_dir = system.file(
                             file.path('tests', 'testthat', 'gold_files'),
                             package = 'nCompiler'
                           ), write_gold_file = FALSE, batch_mode = FALSE) {
  filename <- paste0(gsub(' ', '_', filename), '.gold')
  ## replace operators that can't be used in filenames with an alphabetic name
  ## greedily replace by ordering according to decreasing number of characters
  replacements <- get_ops_values('testthat', 'alpha_name')
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
    ## TODO: create gold file directory and write to that directory
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
