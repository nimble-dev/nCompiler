#!/usr/bin/env Rscript

help_message <-
    "Initial testing routine for nCompiler"

argv <- commandArgs(trailingOnly = TRUE)

## This is a bare-bones implementation that can be expanded similar to
## nimble's system later.
##
## ./run-tests.R  # Default to all tests with files that do not have "not working" in the first line
## ./run-tests.R test1 [test2] [...]  # Run test-test1.R, test-test2.R, etc.

require(nCompiler)
NCpath <- path.package("nCompiler")
testpath <- file.path(NCpath,'tests','testthat')
if (length(grep('^-', argv, invert = TRUE))) {
    # Run only tests specified on commmand line.
    allTests <- paste0('test-', argv[!grepl('^-', argv)], '.R')
    specificTests <- TRUE
} else {
    allTests <- list.files(testpath)
    specificTests <- FALSE
}
if(length(allTests) == 0) {
  stop(paste0("There are no tests.\n  Be sure installation included ",
              "tests.\n (R CMD INSTALL <...> --install-tests)"),
       call.=FALSE)
}
allTests <- allTests[grepl('test-.*\\.R', allTests)]
if(!specificTests) {
  notWorking <- unlist(lapply(allTests,
                              function(testfile) {
                                firstLine <-
                                  readLines(file.path(testpath, testfile),
                                            n = 1)
                                firstLine <- toupper(firstLine)
                                any(grepl("NOT WORKING", firstLine))
                              }
                              )
                       )
  allTests <- allTests[!notWorking]
  cat('Running nCompiler tests.\nIgnoring "not working" because specific tests were requested.\n')
} else {
  cat('Running nCompiler tests.\nInclude "not working" (not case sensitive) in the first line of a test file if it should not be run.\n')
}
for(test in allTests) {
    cat('TESTING', test, '\n')
    name <- gsub('test-(.*)\\.R', '\\1', test)
    reporter <- '"summary"'
    script <- paste0('library(methods);',
                     'library(testthat);',
                     'library(nCompiler);',
                     'test_package("nCompiler", "^', name, '$",',
                     '                      reporter = ', reporter, ')'
                     # tryCatch seems to be masking errors
#                     'tryCatch(test_package("nCompiler", "^', name, '$",',
#                     '                      reporter = ', reporter, '),',
                                        #                     '  error = function(e) quit(status = 1))'
                     )
    system2("Rscript", c("-e", shQuote(script)))
}
