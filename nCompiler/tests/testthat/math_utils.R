## see test-math.R
test_math <- function(param_list, test_name = '', size = 3,
                      dir = file.path(tempdir(), "nCompiler_generatedCode"),
                      control = list(), verbose = nOptions('verbose'),
                      compile_all_funs = FALSE, gold_test = FALSE, ...) {
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
      nC_error <- gen_nClass(list(Cpublic = nFuns_error))
      expect_error(nCompile_nClass(nC_error, control = control), info = test_name)
    }
  }

  ## these tests should compile
  compiles <- param_list[!compile_error]
  nFuns <- lapply(compiles, gen_nFunction)

  if (length(nFuns) > 0) {

    nFun_names <- names(nFuns) ## for verbose output
    names(nFuns) <- paste0('nFun', 1:length(nFuns))

    nC <- gen_nClass(list(Cpublic = nFuns))

    ## if gold_test is TRUE, go directly to nCompiler stages without any
    ## checking for knownFailures and without proceeding to C++ compilation
    if (isTRUE(gold_test)) {
      control$endStage <- 15 ## makeRcppPacket
      ## make sure generated class / function names are the same every time
      nCompiler:::resetLabelFunctionCreators()
      return(
        test_gold_file(
          nCompile_nClass(nC, control = control), test_name, ...)
      )
    }

    if (compile_all_funs) {
      ## useful for debugging an nClass compilation failure
      ## e.g. test_math(binaryOpTests[['+']], compile_all_funs = TRUE)
      for (i in seq_along(nFuns)) {
        if (verbose)
          cat(paste('### Compiling function for test of:', nFun_names[i], '###\n'))
        nCompile_nFunction(nFuns[[i]], control)
      }
    }
    
    nC_compiled <- nCompile_nClass(nC, control = control)
    obj <- nC_compiled$new()

    for (i in seq_along(compiles)) {
      param <- compiles[[i]]
      nFun_i <- paste0('nFun', i)

      set.seed(0) ## TODO: where's the best spot to set the seed

      if (!is.null(param$skip) && param$skip) {
        if (verbose) cat(paste('### Skipping test of', param$name, '###\n'))
      } else {
        if (verbose) cat(paste('### Testing', param$name, '###\n'))
        if (!is.null(param$argChecks)) {
          args <- mapply(
            make_input, param$argTypes, param$argChecks, size, SIMPLIFY = FALSE
          )
        } else args <- lapply(param$argTypes, make_input, size = size)

        if (verbose) cat("## Calling R version of nFunction ##\n")
        ansR <- try(
          do.call(nC$public_methods[[nFun_i]], args),
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
          if (verbose) cat('## Skipping to next test ##\n')
          next
        }

        if (verbose) cat("## Calling compiled nFunction ##\n")
        wrap_if_matches(param$knownFailure, 'runs', expect_error, {
          ansC <- do.call(obj[[nFun_i]], args)
          if(verbose) cat("## Testing equality ##\n")
          if(is.array(ansC)) {
            expect_equal(as.array(ansR),
                         ansC,
                         info = paste("Test:", param$name))
          } else {
            expect_equal(ansR,
                         ansC,
                         info = paste("Test:", param$name))
          }
        })
      }
    }
  }
  invisible(NULL)
}

## batch_of_ops: list with one named entry per operator
##               which itself is a list with any number of
##               test parameterizations (op + arg types)
## TODO: improve the way this works / is used to include granularity = 1
test_math_suite <- function(batch_of_ops, test_name = 'math', full, granularity,
                            write_gold_file, gold_file_dir) {
  if (!full) {

    for (op in names(batch_of_ops)) {## granularity level is 3
      test_math(
        batch_of_ops[[op]], paste0(c(test_name, op), collapse = '_'),
        gold_test = TRUE, write_gold_file = write_gold_file,
        gold_file_dir = gold_file_dir
      )
    }

  } else if (granularity == 4) {
    
    for (unaryOpTest in unlist(batch_of_ops, recursive = FALSE))
      ## TODO: avoid having to wrap unaryOpTest in list()?
      test_math(list(unaryOpTest), unaryOpTest$name)

  } else if (granularity == 3) {

    for (op in names(batch_of_ops))
      test_math(batch_of_ops[[op]], op)

  } else if (granularity == 2) {

    test_math(
      unlist(batch_of_ops, recursive = FALSE),
      deparse(substitute(batch_of_ops)),
    )
  }
}
