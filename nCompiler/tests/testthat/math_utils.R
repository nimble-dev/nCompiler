## see test-math.R
# TODO: use catch_failures
# TODO: give test_math and test_AD similar or same arguments
test_math <- function(test_base_list, size = 3, catch_failures = FALSE,
                      control = list(), seed = 0,
                      verbose = nOptions('verbose'), ...) {
  param_list <- test_base_list$param_list
  nC <- test_base_list$nC
  nC_compiled <- nCompile_nClass(nC, control = control)
  nC_compiled_obj <- nC_compiled$new()

  # TODO: would it make sense to move the for loop into test_base()?
  for (i in seq_along(param_list)) {
    param <- param_list[[i]]
    nFun_i <- paste0('nFun', i)

    set.seed(seed)

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
      test_that("uncompiled and compiled math outputs match", {
        wrap_if_matches(param$knownFailure, 'runs', expect_error, {
          ansC <- do.call(nC_compiled_obj[[nFun_i]], args)
          if (verbose) cat("## Testing equality ##\n")
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
