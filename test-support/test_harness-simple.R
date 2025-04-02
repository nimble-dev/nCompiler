# Simple methods to run individual test files, logging errors to disk or as 
# GitHub issues
#
# intent is to inventory nimble regression tests that fail when run with 
# nCompiler as a back-end compiler.  test failures identify additional nCompiler 
# functionality needed to replace the classic nimble compilation chain.

library(testthat)

# make sure nimble and nCompiler versions get logged in sessionInfo()
library(nimble)
library(nCompiler)

# Custom testthat reporter to indicate which tests ran with errors, etc.
# 
# Behavior: reporter logs to the console the current test being run and any 
#   errors, etc. the test triggers.  tests that run without errors will only
#   be logged as being run (i.e., "SUCCESS" is not written to log file)
#
# source: https://github.com/r-lib/testthat/issues/1006
DetailedSummaryReporter <- R6::R6Class(
  "DetailedSummaryReporter", 
  inherit = testthat::SummaryReporter,
  public = list(
    start_test = function(context, test) {
      self$cat_tight(" [test] ", test, ": ")
    },
    end_test = function(context, test) {
      self$cat_line()
    },
    start_context = function(context) {
      self$cat_tight('[context] ', context, ":\n")
    },
    end_context = function(context) { },
    add_result = function(context, test, result) {
      if (testthat:::expectation_broken(result)) {
        self$failures$push(result)
      } else if (testthat:::expectation_skip(result)) {
        self$skips$push(result)
      } else if (testthat:::expectation_warning(result)) {
        self$warnings$push(result)
      } else {
        if (isTRUE(self$omit_dots)) {
          return()
        }
      }
      self$cat_tight(private$get_summary(result))
    }
  )
)

# Custom testthat reporter to indicate which tests ran with errors, etc.
# 
# Behavior: reporter creates a GitHub issue for each test with errors or other 
#   testthat issues
#
GithubIssueReporter <- R6::R6Class(
  "GithubIssueReporter", 
  inherit = testthat::SummaryReporter,
  public = list(
    
    test_file = NULL,
    base_name = NULL,
    
    github_repo = NULL,
    
    tmpfile = NULL,
    log_issue = FALSE,
    
    initialize = function(test_file, github_repo) {
      super$initialize()
      self$test_file = test_file
      self$base_name = basename(self$test_file)
      self$github_repo = github_repo
    },
    
    start_test = function(context, test) {
      # reset issue flag
      self$log_issue = FALSE
      
      self$cat_tight(" [test] ", test, ": ")
      
      # capture test' console output in temp file
      self$tmpfile = tempfile()
      sink(self$tmpfile)
      cat(paste0("Error in `", self$test_file, "` test suite:\n```\n"))
    },
    
    end_test = function(context, test) {
      self$cat_line()
      
      if(isTRUE(self$log_issue)) {
        cat('\n```\n')
        # log session info
        cat('Session info:\n```\n')
        print(sessionInfo())
        cat('```')
        # close file connection
        sink()
        # create github issue
        gh::gh(
          endpoint = '/repos/{repo}/issues',
          repo = self$github_repo,
          .method = 'POST',
          .params = list(
            title = paste(paste0('[',self$base_name,']'), test),
            body = readr::read_file(self$tmpfile)
          )
        )
        # reconnect to temp file
        sink(self$tmpfile)
      }
      
      # close and clear temp file
      sink()
      unlink(self$tmpfile)
    },
    
    start_context = function(context) {
      self$cat_tight('[context] ', context, ":\n")
    },
    
    end_context = function(context) { },
    
    add_result = function(context, test, result) {
      if (testthat:::expectation_broken(result)) {
        self$log_issue = TRUE
        self$failures$push(result)
      } else if (testthat:::expectation_skip(result)) {
        self$log_issue = TRUE
        self$skips$push(result)
      } else if (testthat:::expectation_warning(result)) {
        self$log_issue = TRUE
        self$warnings$push(result)
      } else {
        if (isTRUE(self$omit_dots)) {
          return()
        }
      }
      self$cat_tight(private$get_summary(result))
    }
  )
)

# set/create log file directory
odir = file.path('test-support', 'logs')
dir.create(path = odir, showWarnings = FALSE, recursive = TRUE)

# specify test files to run
test_suite = rbind(
  data.frame(base_name = 'math', path = file.path('nCompiler', 'tests', 'nimble')),
  data.frame(base_name = 'indexing', path = file.path('nCompiler', 'tests', 'testthat')),
  data.frame(base_name = 'allocations', path = file.path('nCompiler', 'tests', 'nimble')),
  data.frame(base_name = 'coreR', path = file.path('nCompiler', 'tests', 'nimble'))
)

# run tests
invisible(apply(test_suite, 1, function(r) {
  # redirect console output to log file
  log_file = paste0('test-', r['base_name'], '.log')
  # sink(file.path(odir, log_file))
  # debugging info
  # print(sessionInfo())
  # cat('\n')
  # run tests
  test_file = file.path(r['path'], paste0('test-', r['base_name'], '.R'))
  test_file(
    path = test_file, 
    reporter = DetailedSummaryReporter$new()
    # reporter = GithubIssueReporter$new(
    #   test_file = test_file,
    #   github_repo = 'jmhewitt/nCompiler'
    # )
  )
  # close log file
  # sink()
}))
