########################
## Main AD testing utils
########################

## Take a test parameterization created by make_param_batch() or
## make_distribution_fun_AD_param(), generate a random input, and test for
## matching nDerivs outputs from uncompiled and compiled versions of an
## nFunction.
##
## base_list:          a list with entries:
##                     'param_list': list of test parameterizations as created
##                                   by make_param_batch()
##                     'nC': an nClass with one 'Cpublic' method per test
##                           parameterization in 'param_list'
## verbose:            if TRUE, print messages to console while testing
## control:            passed to nCompile_nClass() as the control argument
## catch_failures:     if TRUE, don't stop testing when a testthat expect_*
##                     fails
## seed:               Seed to use in set.seed() before generating random
##                     inputs.
## return_compiled_nf: If TRUE, include the compiled nimbleFunction instance in
##                     the output.
## knownFailures:      Not yet used.
##
## returns: a list with the randomly generated input and possibly the compiled
##          nClass instance, or NULL if the test has a known compilation
##          failure
##
test_AD <- function(base_list, verbose = nOptions('verbose'),
                    catch_failures = FALSE, control = list(), seed = 0,
                    return_compiled_nf = FALSE, knownFailures = list()) {
  if (verbose)
    cat(paste('#### Compiling test of', base_list$test_name, '\n'))

  param_list <- base_list$param_list
  nC <- base_list$nC

  ##
  ## compile the nClass 'nC'
  ##

  ## user provided compiled nimbleFunction?
  nC_compiled <- base_list$nC_compiled
  if (is.null(nC_compiled)) {
    if (verbose) cat("## Compiling nClass \n")

    nC_compiled <- try(
      nCompile_nClass(nC, control = control),
      silent = TRUE)
  }

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

  nC_obj <- nC_compiled$new()

  for (i in seq_along(param_list)) {

    param <- param_list[[i]]

    if (!is.null(param$skip) && param$skip) {
      if (verbose) cat(paste('### Skipping test of', param$name, '\n'))
    } else
      if (verbose) cat(paste('### Testing', param$name, '\n'))

    ## TODO: remove dependence on this Cpublic method naming convention
    nFun_i <- paste0('nFun', i)

    # reset the seed for every test
    if (is.numeric(seed)) set.seed(seed)

    ##
    ## generate inputs for the Cpublic methods
    ##
    input <- make_input(param$argTypes, param$input_gen_funs)

    ##
    ## call R versions of nimbleFunction methods with generated input
    ##
    if (verbose)
      cat("## Calling uncompiled version of nClass method '",
          nFun_i, "'\n", sep = '')

    this_nf <- nC$public_methods[[nFun_i]]

    Rderivs <- try(
      lapply(param$wrts, function(wrt) {
        nCompiler:::nDerivs_nf(
          fxnCall = as.call(c(quote(this_nf), input)),
          wrt = wrt
        )
      }), silent = TRUE
    )
    if (inherits(Rderivs, 'try-error')) {
      msg <- paste(
        'Calling R version of test', param$name,
        'resulted in an error:\n', Rderivs[1]
      )
      if (isTRUE(catch_failures)) ## continue to compilation
        warning(msg, call. = FALSE, immediate. = TRUE)
      else
        stop(msg, call. = FALSE) ## throw an error here
    }

    ##
    ## call Cpublic methods of compiled nClass with generated input
    ##
    if (verbose)
      cat("## Calling compiled version of nClass method '",
          nFun_i, "'\n", sep = '')

    Cderivs <- lapply(param$wrts, function(wrt) {
      derivs_obj <- nCompiler:::nDerivs_full(
        fxnCall = as.call(c(substitute(
          nC_obj$NFUN,
          list(NFUN = nFun_i)
        ), input)), wrt = wrt, NC = nC
      )
      list(
        value = derivs_obj$value,
        gradient = derivs_obj$gradient,
        hessian = derivs_obj$hessian
      )
    })

    for (wrt in names(param$wrts)) {
      if (verbose)
        cat("## Testing equality of outputs for ", wrt, '\n')
      test_that(
        paste0("Compiled and uncompiled output matches for ", wrt),
        {
          expect_equal( ## check values
            as.vector(Cderivs[[wrt]]$value),
            as.vector(Rderivs[[wrt]]$value),
            info = paste0("values with ", wrt)
          )
          expect_equal( ## check gradients
            as.vector(Cderivs[[wrt]]$gradient),
            as.vector(Rderivs[[wrt]]$gradient),
            info = paste0("gradients with ", wrt)
          )
          expect_equal( ## check hessians
            as.vector(Cderivs[[wrt]]$hessian),
            as.vector(Rderivs[[wrt]]$hessian),
            info = paste0("hessians with ", wrt),
            tol = 1e-07 ## known issue with numDeriv output
          )
        }
      )
    }

    ## TODO: test hessian and distribution fun's log arg
    if (FALSE) {
      if ('log' %in% names(param$args)) {
        input2 <- input
        input2$log <- as.numeric(!input$log)
        Rderivs2 <- try(
          sapply(names(param$methods), function(method) {
            do.call(nfInst[[method]], input2)
          }, USE.NAMES = TRUE), silent = TRUE
        )
        Cderivs2 <- sapply(names(param$methods), function(method) {
          do.call(CnfInst[[method]], input2)
        }, USE.NAMES = TRUE)
      }
      
      ##
      ## loop over test methods (each with a different wrt arg)
      ##
      ## set expect_equal tolerances
      tol1 <- if (is.null(param$tol1)) 1e-8 else param$tol1
      tol2 <- if (is.null(param$tol1)) 1e-7 else param$tol2
      tol3 <- if (is.null(param$tol2)) 1e-6 else param$tol3
      for (method_name in names(param$methods)) {
        if (verbose) {
          cat(paste0(
            "## Testing ", method_name, ': ',
            paste0(param$wrts[[method_name]], collapse = ', '),
            '\n'
          ))
        }
        ##
        ## test values
        ##
        value_test_fails <- is_method_failure(
          param$name, method_name, 'value', knownFailures
        )
        value_test <- wrap_if_true(value_test_fails, expect_failure, {
          if (verbose) cat("## Checking values\n")
          expect_equal(
            Cderivs[[method_name]]$value,
            Rderivs[[method_name]]$value,
            tolerance = tol1
          )
          if ('log' %in% names(param$args)) {
            if (verbose) cat("## Checking log behavior for values\n")
            expect_equal(
              Cderivs2[[method_name]]$value,
              Rderivs2[[method_name]]$value,
              tolerance = tol1
            )
            expect_false(isTRUE(all.equal(
              Rderivs[[method_name]]$value,
              Rderivs2[[method_name]]$value,
              tolerance = tol1
            )))
            expect_false(isTRUE(all.equal(
              Cderivs[[method_name]]$value,
              Cderivs2[[method_name]]$value,
              tolerance = tol1
            )))
          }
        }, wrap_in_try = isTRUE(catch_failures))
        if (isTRUE(catch_failures) && inherits(value_test, 'try-error')) {
          warning(
            paste0(
              'There was something wrong with the values of ',
              param$name, ' with wrt = c(',
              paste0(param$wrts[[method_name]], collapse = ', '), ').\n',
              value_test[1]
            ),
            call. = FALSE,
            immediate. = TRUE
          )
        } else if (value_test_fails) {
          if (verbose) {
            cat(paste0(
              "## As expected, test of values failed for ", method_name, ' with wrt: ',
              paste0(param$wrts[[method_name]], collapse = ', '),
              '\n'
            ))
          }
          ## stop testing after an expected failure
          break
        }
        ##
        ## test jacobians
        ##
        jacobian_test_fails <- is_method_failure(
          param$name, method_name, 'jacobian', knownFailures
        )
        jacobian_test <- wrap_if_true(jacobian_test_fails, expect_failure, {
          if (verbose) cat("## Checking jacobians\n")
          expect_equal(
            Cderivs[[method_name]]$jacobian,
            Rderivs[[method_name]]$jacobian,
            tolerance = tol2
          )
          if ('log' %in% names(param$args)) {
            if (verbose) cat("## Checking log behavior for jacobians\n")
            expect_equal(
              Cderivs2[[method_name]]$jacobian,
              Rderivs2[[method_name]]$jacobian,
              tolerance = tol2
            )
            expect_false(isTRUE(all.equal(
              Rderivs[[method_name]]$jacobian,
              Rderivs2[[method_name]]$jacobian,
              tolerance = tol2
            )))
            expect_false(isTRUE(all.equal(
              Cderivs[[method_name]]$jacobian,
              Cderivs2[[method_name]]$jacobian,
              tolerance = tol2
            )))
          }
        }, wrap_in_try = isTRUE(catch_failures))
        if (isTRUE(catch_failures) && inherits(jacobian_test, 'try-error')) {
          warning(
            paste0(
              'There was something wrong with the jacobian of ',
              param$name, ' with wrt = c(',
              paste0(param$wrts[[method_name]], collapse = ', '), ').\n',
              jacobian_test[1]
            ),
            call. = FALSE,
            immediate. = TRUE
          )
        } else if (jacobian_test_fails) {
          if (verbose) {
            cat(paste0(
              "## As expected, test of jacobian failed for ", method_name, ' with wrt: ',
              paste0(param$wrts[[method_name]], collapse = ', '),
              '\n'
            ))
          }
          ## stop testing after an expected failure
          break
        }
        ##
        ## test hessians
        ##
        hessian_test_fails <- is_method_failure(
          param$name, method_name, 'hessian', knownFailures
        )
        hessian_test <- wrap_if_true(hessian_test_fails, expect_failure, {
          if (verbose) cat("## Checking hessians\n")
          expect_equal(
            Cderivs[[method_name]]$hessian,
            Rderivs[[method_name]]$hessian,
            tolerance = tol3
          )
          if ('log' %in% names(param$args)) {
            if (verbose) cat("## Checking log behavior for hessians\n")
            expect_equal(
              Cderivs2[[method_name]]$hessian,
              Rderivs2[[method_name]]$hessian,
              tolerance = tol3
            )
            expect_false(isTRUE(all.equal(
              Rderivs[[method_name]]$hessian,
              Rderivs2[[method_name]]$hessian,
              tolerance = tol3
            )))
            expect_false(isTRUE(all.equal(
              Cderivs[[method_name]]$hessian,
              Cderivs2[[method_name]]$hessian,
              tolerance = tol3
            )))
          }
        }, wrap_in_try = isTRUE(catch_failures))
        if (isTRUE(catch_failures) && inherits(hessian_test, 'try-error')) {
          warning(
            paste0(
              'There was something wrong with the hessian of ',
              param$name, ' with wrt = c(',
              paste0(param$wrts[[method_name]], collapse = ', '), ').\n',
              hessian_test[1]
            ),
            call. = FALSE,
            immediate. = TRUE
          )
        } else if (hessian_test_fails) {
          if (verbose) {
            cat(paste0(
              "## As expected, test of hessian failed for ", method_name, ' with wrt: ',
              paste0(param$wrts[[method_name]], collapse = ', '),
              '\n'
            ))
          }
          ## stop testing after an expected failure
          break
        }
      }
      if (verbose) cat("### Test successful \n\n")
      if (return_compiled_nf)
        invisible(list(CnfInst = CnfInst, input = input))
      else if(!compilation_fails) {
        ## nimble:::clearCompiled(CnfInst)
        invisible(list(input = input))
      }
    } ## if (FALSE) {
  }
  invisible(NULL)
}

#########################################
## AD test parameterization builder utils
#########################################

## Takes a named list of `argTypes` and returns a list of character
## vectors, each of which is valid as the `wrt` argument of `nimDerivs()`.
## Each argument on its own and all combinations of the arguments will
## always be included, and then make_wrt will try to create up to `n_random`
## additional character vectors with random combinations of the arguments
## and indexing of those arguments when possible (i.e. for non-scalar args).
## n_arg_reps determines how many times an argument can be used in a given
## wrt character vector. By default, any argument will appear only 1 time.
make_wrt <- function(argTypes, n_random = 10, n_arg_reps = 1) {

  ## always include each arg on its own, and all combinations of the args
  wrts <- as.list(names(argTypes))
  if (length(argTypes) > 1)
    for (m in 2:length(argTypes)) {
      this_combn <- combn(names(argTypes), m)
      wrts <- c(
        wrts,
        unlist(apply(this_combn, 2, list), recursive = FALSE)
      )
    }

  argSymbols <- lapply(
    argTypes, function(argType)
      add_missing_size(nCompiler:::argType2symbol(argType))
  )

  while (n_random > 0) {
    n_random  <- n_random - 1
    n <- sample(1:length(argTypes), 1) # how many of the args to use?
    ## grab a random subset of the args of length n
    args <- sample(argSymbols, n)
    ## may repeat an arg up to n_arg_reps times
    reps <- sample(1:n_arg_reps, length(args), replace = TRUE)
    this_wrt <- c()
    for (i in 1:length(args)) {
      while (reps[i] > 0) {
        reps[i] <- reps[i] - 1
        ## coin flip determines whether to index vectors/matrices
        use_indexing <- sample(c(TRUE, FALSE), 1)
        if (use_indexing && args[[i]]$nDim > 0) {
          rand_row <- sample(1:args[[i]]$size[1], size = 1)
          ## another coin flip determines whether to use : in indexing or not
          use_colon <- sample(c(TRUE, FALSE), 1)
          if (use_colon && rand_row < args[[i]]$size[1]) {
            end_row <- rand_row +
              sample(1:(args[[i]]$size[1] - rand_row), size = 1)
            rand_row <- paste0(rand_row, ':', end_row)
          }
          index <- rand_row
          if (args[[i]]$nDim == 2) {
            rand_col <- sample(1:args[[i]]$size[2], size = 1)
            ## one more coin flip to subscript second dimension
            use_colon_again <- sample(c(TRUE, FALSE), 1)
            if (use_colon_again && rand_col < args[[i]]$size[2]) {
              end_col <- rand_col +
                sample(1:(args[[i]]$size[2] - rand_col), size = 1)
              rand_col <- paste0(rand_col, ':', end_col)
            }
            index <- paste0(index, ',', rand_col)
          }
          this_wrt <- c(this_wrt, paste0(names(args)[i], '[', index, ']'))
        }
        ## if first coin flip was FALSE, just
        ## use the arg name without indexing
        else this_wrt <- c(this_wrt, names(args)[i])
      }
    }
    if (!is.null(this_wrt)) wrts <- c(wrts, list(unique(this_wrt)))
  }
  wrts <- unique(wrts)
  names(wrts) <- paste0('wrt: ', sapply(wrts, paste, collapse = ', '))
  wrts
}

## Make an operator parameterization to be used by test_AD. This method can be
## used with make_test_param_batch() and is used by make_distribution_fun_AD_param().
##
## op:             Character string, the operator that will be the focus of the
##                 test.
## argTypes:       Character vector of argType strings that, when parsed, can be
##                 passed to argType2symbol. If named, the names will as the formals
##                 of the nimbleFunction generator's run method and other methods.
##                 If not, formals are generated as arg1, arg2, etc.
## wrt_args:       Optional character vector of args to use in make_wrt(). If NULL,
##                 assumes that all the arguments should be used.
## input_gen_funs: A list of input generation functions which is simply passed to
##                 the output list. Should be NULL (use defaults found in
##                 argType_2_input()), length 1 (use same input gen mechanism for each
##                 argType, or a named list with names from among the argType names
##                 (possibly the sequentially generated names). This will be NULL
##                 when bulk generating the test params using make_test_param_batch() and
##                 added later via modify_on_match(). Used in the call to
##                 make_AD_test_param() in make_distribution_fun_AD_test(). 
## more_args:      A named list of additional fixed arguments to use in the
##                 generated operator call. E.g., if op = 'dnorm',
##                 argTypes = c('double(1, 5)', 'double(0)'), and
##                 more_args = list(log = 1), the call to make_test_param will include
##                 the expression dnorm(arg1, arg2, log = 1).
## seed:           A seed to use in set.seed().
##
## returns: A list with the following elements:
##          name:           from make_test_param
##          expr:           from make_test_param
##          argTypes:       from make_test_param
##          returnType:     from make_test_param
##          wrts:           a list of character vectors, each of which is the wrt
##                          argument for the corresponding method in methods
##          input_gen_funs: A list of random input generation functions to be
##                          used by argType_2_input(). 
make_AD_test_param <- function(op, argTypes, wrt_args = NULL,
                               input_gen_funs = NULL, more_args = NULL,
                               seed = 0, known_failures = NULL) {
  ## set the seed for make_wrt
  if (is.numeric(seed)) set.seed(seed)
  test_param <- make_test_param(op, argTypes, input_gen_funs, more_args, known_failures)

  if (is.null(wrt_args)) wrt_args_filter <- rep(TRUE, length(argTypes))
  else wrt_args_filter <- wrt_args
  wrts <- make_wrt(test_param$argTypes[wrt_args_filter])

  invisible(
    c(test_param,
      list(
        wrts = wrts,
        enableDerivs = TRUE))
  )
}

## op:   operator name
## seed: a random seed
##
make_AD_test_params_one_op <- function(op, seed = 0) {
  opInfo <- nCompiler:::getOperatorDef(op, 'testing')
  if (is.null(opInfo) || is.null(opInfo[['AD_argTypes']])) return(NULL)
  argTypes <- opInfo[['AD_argTypes']]
  known_failures <- opInfo$known_failures
  AD_failures <- if (is.null(known_failures)) NULL else known_failures$AD
  ans <- lapply(argTypes, function(argTypes_) {
    make_AD_test_param(op, argTypes_, wrt_args = opInfo[['wrt_args']],
                     input_gen_funs = opInfo[['input_gen_funs']],
                     more_args = opInfo[['more_args']], seed = seed,
                     known_failures = AD_failures)
  })
  names(ans) <- sapply(ans, `[[`, 'name')
  invisible(ans)
}

make_AD_test_params <- function(ops, seed = 0) {
  sapply(ops, make_AD_test_params_one_op, seed = seed, simplify = FALSE)
}

get_AD_ops <- function() {
  get_matching_ops('testing', 'AD_argTypes', function(x) !is.null(x))
}

## Takes an element of distn_params list and returns a list of AD test
## parameterizations, each of which test_AD can use.
##
## distn_param: Probability distribution parameterization, which
##              must have the following fields/subfields:
##              - name
##              - variants
##              - args
##                - rand_variate
##                  - type
##              Additional args must also have type field.
##
make_distribution_fun_AD_test <- function(distn_param) {
  distn_name <- distn_param$name
  ops <- sapply(distn_param$variants, paste0, distn_name, simplify = FALSE)

  rand_variate_idx <- which(names(distn_param$args) == 'rand_variate')

  argTypes <- sapply(distn_param$variants, function(variant) {
    op <- paste0(variant, distn_name)
    rand_variate_type <- distn_param$args$rand_variate$type
    first_argType <- switch(
      variant,
      d = rand_variate_type,
      p = rand_variate_type,
      q = c('double(0)')##, 'double(1, 4)')
    )
    first_arg_name <- switch(variant, d = 'x', p = 'q', q = 'p')
    ## need this complicated expand.grid call here because the argTypes might be
    ## character vectors (e.g. if rand_variate_type is c("double(0)", "double(1, 4)")
    ## then we create a test where we sample a scalar from the support and
    ## another where we sample a vector of length 4 from the support
    grid <- eval(as.call(c(
      expand.grid, list(first_argType),
      lapply(distn_param$args, `[[`, 'type')[-rand_variate_idx]
    )))
    argTypes <- as.list(data.frame(t(grid), stringsAsFactors=FALSE))
    lapply(argTypes, function(v) {
      names(v) <- c(
        first_arg_name,
        names(distn_param$args[-rand_variate_idx])
      )
      v
    })
  }, simplify = FALSE)
  input_gen_funs <- lapply(distn_param$args[-rand_variate_idx], `[[`, 'input_gen_fun')
  input_gen_funs_list <- sapply(distn_param$variants, function(variant) {
    arg1_input_gen_fun <- switch(
      variant,
      d = list(x = distn_param$args$rand_variate$input_gen_fun),
      p = list(q = distn_param$args$rand_variate$input_gen_fun),
      q = list(p = runif)
    )
    c(arg1_input_gen_fun, input_gen_funs)
  }, simplify = FALSE)
  test_params <- unlist(
    lapply(
      distn_param$variants, function(variant) {
        lapply(
          argTypes[[variant]],
          function(these_argTypes) {
            wrt_args = intersect(
              distn_param$wrt, names(these_argTypes)
            )
            make_AD_test_param(
              ops[[variant]], these_argTypes,
              wrt_args = wrt_args,
              input_gen_funs = input_gen_funs_list[[variant]],
              more_args = distn_param$more_args[[variant]]
            )
          }
        )
      }
    ), recursive = FALSE
  )
  names(test_params) <- sapply(test_params, `[[`, 'name')
  return(test_params)
}
