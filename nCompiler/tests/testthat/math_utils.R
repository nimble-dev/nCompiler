## This function should be passed to test_base() in testing_utils.R.
test_math <- function(base_list, verbose = nOptions('verbose'),
                      catch_failures = FALSE, control = list(), seed = 0,
                      ...) {
  param_list <- base_list$param_list
  nC <- base_list$nC
  if (verbose)
    cat(paste('#### Compiling test of', base_list$test_name, '\n'))
  nC_compiled <- try(nCompile_nClass(nC, control = control), silent = TRUE)

  ## TODO: use an expect_* here instead?
  if (inherits(nC_compiled, 'try-error')) {
    msg <- paste0(
      'The test of ', base_list$test_name, ' failed to compile.\n', nC_compiled[1]
    )

    if (isTRUE(catch_failures)) {
      warning(msg, call. = FALSE, immediate. = TRUE)
      return(invisible(NULL))
    } else {
      stop(msg, call = FALSE)
    }
  }

  nC_compiled_obj <- nC_compiled$new()

  for (i in seq_along(param_list)) {
    param <- param_list[[i]]
    nFun_i <- paste0('nFun', i)

    set.seed(seed)

    if (!is.null(param$skip) && param$skip) {
      if (verbose) cat(paste('### Skipping test of', param$name, '\n'))
    } else {
      if (verbose) cat(paste('### Testing', param$name, '\n'))

      input <- make_input(param$argTypes, param$input_gen_funs)

      if (verbose) cat("## Calling R version of nFunction \n")
      ansR <- try(
        do.call(nC$public_methods[[nFun_i]], input),
        silent = TRUE
      )

      if (inherits(ansR, 'try-error')) {
        warning(
          paste(
            'Calling R version of test', param$name,
            'resulted in an error:', ansR[1]
          ),
          immediate. = TRUE
        )
        if (verbose) cat('## Skipping to next test \n')
        next
      }

      if (verbose) cat("### Calling compiled nFunction \n")
      test_that("uncompiled and compiled math outputs match", {
        wrap_if_true(param$runtime_failure, expect_error, {
          ansC <- try(do.call(nC_compiled_obj[[nFun_i]], input), silent = TRUE)
          if (inherits(ansC, 'try-error')) {
            msg  <- paste('Calling compiled version of test', param$name,
                          'resulted in an error:', ansC[1])
            if (isTRUE(catch_failures)) {
              warning(msg, immediate. = TRUE)
              return(invisible(NULL))
            } else {
              stop(msg, call = FALSE)
            }
          }
          if (verbose) {
            cat("## Testing equality of compiled and uncompiled output\n")
            if (isTRUE(param$runtime_failure))
              cat('### This results in a known runtime failure.')
          }
          if (is.array(ansC)) {
            expect_equal(as.array(ansR),
                         ansC,
                         info = paste("Test:", param$name))
          } else {
            expect_equal(ansR,
                         ansC,
                         info = paste("Test:", param$name))
          }
        })
      })
    }
  }
  invisible(NULL)
}

## op:   operator name
##
make_math_test_params_one_op <- function(op) {
  opInfo <- nCompiler:::getOperatorDef(op, 'testing')
  if (is.null(opInfo) || is.null(opInfo[['math_argTypes']])) return(NULL)
  argTypes <- opInfo[['math_argTypes']]
  known_failures <- opInfo$known_failures
  math_failures <- if (is.null(known_failures)) NULL else known_failures$math
  ans <- lapply(argTypes, function(argTypes_) {
    make_test_param(
      op, argTypes_, input_gen_funs = opInfo[['input_gen_funs']],
      more_args = opInfo[['more_args']], known_failures = math_failures
    )
  })
  names(ans) <- sapply(ans, `[[`, 'name')
  invisible(ans)
}

make_math_test_params <- function(ops) {
  sapply(ops, make_math_test_params_one_op, simplify = FALSE)
}

get_math_ops <- function() {
  get_matching_ops('testing', 'math_argTypes', function(x) !is.null(x))
}
